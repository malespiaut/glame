; glame.scm
; $Id: glame.scm,v 1.27 2000/04/25 08:58:00 richi Exp $
;
; Copyright (C) 2000 Richard Guenther
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;


;
; GLAME scheme library, std functions
;

(define help
  (lambda ()
    (display "No online help available yet (type \"(quit)\" to exit from GLAME).\nPlease refer to the GLAME manual or the info pages.\n")))

; what output filter should we use?
(define audio-out "audio-out")
; what file reader should we use?
(define read-file "read-file")
; what file write should we use?
(define write-file "write-file")

;
; clever scheme helpers
;

(define (map-pairs f l)
  (if (or (null? l) (null? (cdr l)))
      #t
      (begin (f (car l) (cadr l))
	     (map-pairs f (cdr l)))))

(define while-not-false
  (lambda (f)
    (if (eq? #f (f)) #t (while-not-false f))))

(define repeat-n
  (lambda (x n)
    (if (>= 0 n) '() (cons x (repeat-n x (- n 1))))))


;
; some high level filternetwork helpers
;

(define net-new filternetwork_new)

; (net-add-node net "node" '("param" val) ...)
; (net-add-node net "node")
(define net-add-node
  (lambda (net node . params)
    (let ((n (filternetwork_add_node net node "")))
      (if (eq? n #f) (begin (display "no node ") (display node) (newline) #f)
	  (begin
	    (apply node-set-params n params)
	    n)))))

; (net-add-nodes net '("node" '("param" val)...) ...)
(define net-add-nodes
  (lambda (net . nodes)
    (if (null? nodes) '()
	(let* ((n (apply net-add-node net (car nodes)))
	       (nn (if (eq? n #f) #f (apply net-add-nodes net (cdr nodes)))))
	  (if (eq? n #f) #f
	      (if (eq? nn #f)
		  (begin
		    (filternetwork_delete_node n)
		    #f)
		  (cons n nn)))))))

; (nodes-delete node node ...)
(define nodes-delete
  (lambda nodes
    (if (null? nodes) '()
	(begin
	  (filternetwork_delete_node (car nodes))
	  (apply nodes-delete (cdr nodes))))))

; linear connect the nodes
; (nodes-connect `(,node ,node ...) `(..) ...)
(define nodes-connect
  (lambda nodes
    (map (lambda (nodes)
	   (map-pairs
	    (lambda (s d) (filternetwork_add_connection s "out" d "in"))
	    nodes))
	 nodes)))

; (node-set-params node `("label" ,value) `("label" ...) ...)
(define node-set-params
  (lambda (node . params)
    (map (lambda (p)
	   (if (not (null? p))
	       (filternode_set_param node (car p) (cadr p))))
	 params)))


; run the net, wait for completion and delete it
(define net-run
  (lambda (net)
    (if (eq? #t (filternetwork_launch net))
	(begin
	  (filternetwork_start net)
	  (filternetwork_wait net)
	  (filternetwork_delete net)))))

; start the net (in the background, not deleting it afterwards),
; i.e. you can use filternetwork_(pause|start|terminate) on it.
(define net-run-bg
  (lambda (net)
    (if (eq? #t (filternetwork_launch net))
	(filternetwork_start net))
    net))


;-----------------------------------------------------------------
;
; file/track/audio I/O stuff
;

;
; high level swapfile/track helper
;

(define file-to-track
  (lambda (file group track)
    (let* ((net (net-new))
	   (rf (net-add-node net read-file))
	   (i 1))
      (filternode_set_param rf "filename" file)
      (while-not-false
       (lambda ()
	 (let ((to (net-add-node net "track-out")))
	   (if (eq? (filternetwork_add_connection rf "out" to "in") #f)
	       (begin (filternetwork_delete_node to) #f)
	       (begin
		 (node-set-params to
		   `("group" ,group)
		   `("track" ,(string-append track "-" (number->string i))))
		 (set! i (+ i 1))
		 #t)))))
      (net-run net))))


; (play-track "group" "track")
(define play-track
  (lambda (group track)
    (let* ((net (net-new))
	   (ti (net-add-node net "track-in"))
	   (ao (net-add-node net audio-out)))
      (node-set-params ti `("group" ,group) `("track" ,track))
      (nodes-connect `(,ti ,ao))
      (net-run net))))

; (play-tracks '("group" "track") '(...) ...)
(define play-tracks
  (lambda tracks
    (let* ((net (net-new))
	   (mix (net-add-node net "mix2"))
	   (ao (net-add-node net audio-out)))
      (nodes-connect `(,mix ,ao))
      (map (lambda (t)
	     (let ((ti (net-add-node net "track_in")))
	       (node-set-params ti `("group" ,(car t)) `("track" ,(cadr t)))
	       (nodes-connect `(,ti ,mix))))
	   tracks)
      (net-run net))))


;
; play a soundfile - automagically finds the number of channels
;

(define play
  (lambda (fname)
    (let* ((net (net-new))
	   (rf (net-add-node net read-file))
	   (ao (net-add-node net audio-out)))
      (node-set-params rf `("filename" ,fname))
      (while-not-false
         (lambda () (filternetwork_add_connection rf "out" ao "in")))
      (net-run net))))


;
; play a file with automagically determining #channels _and_
; applying a chain of effects (with an optional list of parameters applied to each)
; (play-eff "test.wav" '("echo"))
; (play-eff "test.wav" '("echo") '("iir" '("attack" 100)))
;

(define play-eff
  (lambda (fname . effects)
    (let* ((net (net-new))
	   (rf (net-add-node net read-file))
	   (ao (net-add-node net audio-out)))
      (node-set-params rf `("filename" ,fname))
      (while-not-false
       (lambda ()
	 (let* ((eff (apply net-add-nodes net effects))
		(conn (filternetwork_add_connection rf "out" (car eff) "in")))
	   (if (eq? conn #f)
	       (begin (apply nodes-delete eff) #f)
	       (begin (nodes-connect (append eff (list ao))) #t)))))
      (net-run net))))

;
; load, process and save file
;

(define save-eff
  (lambda (fname oname . effects)
    (let* ((net (net-new))
	   (rf (net-add-node net read-file))
	   (wf (net-add-node net write-file)))
      (node-set-params rf `("filename" ,fname))
      (node-set-params wf `("filename" ,oname))
      (while-not-false
       (lambda ()
	 (let* ((eff (apply net-add-nodes net effects))
		(conn (filternetwork_add_connection rf "out" (car eff) "in")))
	   (if (eq? conn #f)
	       (begin (apply nodes-delete eff) #f)
	       (begin (nodes-connect (append eff (list ao))) #t)))))
      (net-run net))))


;--------------------------------------------------------------
;
; Macro filters for common use (and tutorial purposes)
;

;
; mp3 reader using the pipe-in filter and mpg123
;

;(let* ((net (net-new))
;       (p (net-add-node net "pipe-in")))
;  (filternode_set_param p "cmd" "mpg123 -q -s ")
;  (filternetwork_add_output net p "out" "out" "output")
;  (filternetwork_add_param net p "tail" "filename" "filename")
;  (filternetwork_to_filter net "read-mp3" "mp3 reader"))

;
; feedback echo2 macro filter
;

;(let* ((net (net-new))
;       (extend (net-add-node net "extend"))
;       (mix2 (net-add-node net "mix"))
;       (one2n (net-add-node net "one2n"))
;       (delay (net-add-node net "delay"))
;       (va (net-add-node net "volume-adjust")))
;  (filternetwork_add_input net extend "in" "in" "echo source")
;  (filternetwork_add_output net one2n "out" "out" "source with echo")
;  (filternetwork_add_param net delay "delay" "delay" "echo delay")
;  (filternetwork_add_param net va "factor" "mix" "echo mix ratio")
;  (filternetwork_add_param net extend "time" "extend" "time to extend")
;  (filternetwork_add_param net mix2 "gain" "gain" "output gain")
;  (filternode_set_param net "delay" 200)
;  (filternode_set_param net "extend" 600)
;  (filternode_set_param net "mix" 0.7)
;  (nodes-connect `(,extend ,mix2 ,one2n ,delay ,va ,mix2))
;  (filternetwork_to_filter net "echo2" "echo as macro filter"))




;------------------------------------------------------------
;
; testing stuff - remember, there is a ~/.glame.scm file
; automatically loaded, too - so perhaps some of this stuff
; belongs there...
;

;
; test_latency and test_network implementations
;
; test_network is different from the C version, so you need
; to execute the following sequence of commands for any given
; network (in a file, should define a network returning procedure):
; (load "network")
; (test-network network '("param1" "value1") '(...) ....)

(define test-latency
  (lambda (depth)
    (let* ((net (net-new))
	   (nodes (apply net-add-nodes net (cons '("ping") (repeat-n '("null") depth)))))
      (nodes-connect (append nodes (list (car nodes))))
      (net-run net))))

(define test-network
  (lambda (name . params)
    (let ((net (name)))
      (apply node-set-params net params)
      (net-run net))))


;
; test save
;

(define testsave
  (lambda (iname oname)
    (let* ((net (net-new))
           (rf (net-add-node net read-file))
	   (wf (net-add-node net "write_file")))
    (node-set-params rf `("filename" ,iname))
    (node-set-params wf `("filename" ,oname))
    (while-not-false
      (lambda () (filternetwork_add_connection rf "out" wf "in")))
    (net-run net))))

;
; test rms
;

(define testrms
  (lambda (fname)
    (let* ((net (net-new))
           (rf (net-add-node net read-file))
	   (mix (net-add-node net "mix2"))
	   (stat (net-add-node net "statistic"))
	   (drms (net-add-node net "debugrms")))
    (node-set-params rf `("filename" ,fname))
    (while-not-false 
      (lambda () (filternetwork_add_connection rf "out" mix "in")))
    (nodes-connect  `(,mix ,stat ,drms))
    (net-run net))))

;
; test iir
;

(define testiir
  (lambda (fname . params)
    (let* ((net (net-new))
           (rf (net-add-node net read-file))
	   (mix (net-add-node net "mix2"))
	   (iir (net-add-node net "iir"))
	   (stat (net-add-node net "statistic"))
	   (drms (net-add-node net "debugrms")))
    (node-set-params rf `("filename" ,fname))
    (apply node-set-params iir params)
    (while-not-false 
      (lambda () (filternetwork_add_connection rf "out" mix "in")))
    (nodes-connect  `(,mix ,iir ,stat ,drms))
    (net-run net))))
