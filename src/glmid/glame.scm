; glame.scm
; $Id: glame.scm,v 1.35 2000/09/21 09:23:06 richi Exp $
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

(define *help-text* '())

(define (add-help command args text)
  (set! *help-text* (cons (cons command (cons args text)) *help-text*)))

(define (display-command command)
  (let ((command-args-text (assq command *help-text*)))
    (if command-args-text
	(begin 
	  (write (cons command (cadr command-args-text)))
	  (display " -> ")
	  (display (cddr command-args-text))
	  (newline)
	  (newline))
	(begin (display "unknown command or no help available")
	       (newline)))))
  
(define (commands-w/help)
  (map car *help-text*))

; GLAME 0.2.0 compatibility
(define (plugin_description p) (plugin_query p "desc"))

(define (help-helper maybe-command)
  (let ((commands-w/help (commands-w/help)))
    (if (null? maybe-command)
	(begin
	  (display "online help not complete yet ")
	  (newline)
	  (display "Please refer to the GLAME manual or the info pages.")
	  (newline)
	  (for-each display-command commands-w/help))
	(display-command (car maybe-command)))))

(define-macro (help . maybe-command)
  `(help-helper ',maybe-command))

(add-help 'help '(command) "help") 

(add-help 'quit '() "exit GLAME")

; what output filter should we use?
(define audio-out "audio-out")
; what file reader should we use?
(define read-file "read-file")
; what file write should we use?
(define write-file "write-file")

;
; clever scheme helpers
;

(define (filter pred l)
  (if (null? l)
      '()
      (let ((el (car l)))
	(if (pred el)
	    (cons el (filter (cdr l)))
	    (filter (cdr l))))))

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

(add-help 'net-new '() "generate a new filter network")

; (net-add-node net "node" '("param" val) ...)
; (net-add-node net "node")
(define net-add-node
  (lambda (net node . params)
    (let ((n (filternetwork_add_node net node "")))
      (if (eq? n #f) (begin (display "no node ") 
			    (display node) 
			    (newline) 
			    #f)
	  (begin
	    (apply node-set-params n params)
	    n)))))

(add-help 'net-add-node '(net "node" '("param" val) ...) 
	  "create filter and add it to the network")

; (net-add-nodes net '("node" '("param" val)...) ...)
(define net-add-nodes
  (lambda (net . nodes)
    (if (null? nodes) 
	'()
	(let* ((n (apply net-add-node net (car nodes)))
	       (nn (if (eq? n #f) 
		       #f 
		       (apply net-add-nodes net (cdr nodes)))))
	  (if (eq? n #f) #f
	      (if (eq? nn #f)
		  (begin
		    (filternetwork_delete_node n)
		    #f)
		  (cons n nn)))))))

(add-help 'net-add-nodes '(net '("node" '("param" val) ...) ...)
	  "create filters and add it to the network")

; (nodes-delete node node ...)
(define nodes-delete
  (lambda nodes
    (if (null? nodes) '()
	(begin
	  (filternetwork_delete_node (car nodes))
	  (apply nodes-delete (cdr nodes))))))

(add-help 'nodes-delete '(node ...) "delete nodes")

; linear connect the nodes
; (nodes-connect `(,node ,node ...) `(..) ...)
(define nodes-connect
  (lambda nodes
    (map (lambda (nodes)
	   (map-pairs
	    (lambda (s d) (filternetwork_add_connection s "out" d "in"))
	    nodes))
	 nodes)))

(add-help 'nodes-connect '((node ...) ...)
	  "linear connect the nodes of each list")

; (node-set-params node `("label" ,value) `("label" ...) ...)
(define node-set-params
  (lambda (node . params)
    (map (lambda (p)
	   (if (not (null? p))
	       (filternode_set_param node (car p) (cadr p))))
	 params)))


(add-help 'node-set-params '(("label" value) ...)
	  "set the parameter with the specified label to value")

; run the net, wait for completion and delete it
(define net-run
  (lambda (net)
    (if (eq? #t (filternetwork_launch net))
	(begin
	  (filternetwork_start net)
	  (filternetwork_wait net)
	  (filternetwork_delete net)))))

(add-help 'net-run '(net)
	  "run the net, wait for completion and delete it.")

; start the net (in the background, not deleting it afterwards),
; i.e. you can use filternetwork_(pause|start|terminate) on it.
(define net-run-bg
  (lambda (net)
    (if (eq? #t (filternetwork_launch net))
	(filternetwork_start net))
    net))

(add-help 'net-run-bg '(net)
	  "run the net in the background")
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
	   (rf (net-add-node net read-file)))
      (filternode_set_param rf "filename" file)
      (let loop ((i 1))
	 (let ((to (net-add-node net "track-out")))
	   (if (eq? (filternetwork_add_connection rf "out" to "in") #f)
	       (filternetwork_delete_node to)
	       (begin
		 (node-set-params to
		   `("group" ,group)
		   `("track" ,(string-append track "-" (number->string i))))
		 (loop (+ i 1))))))
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

(add-help 'play '(filename) "play a soundfile")
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

(add-help 'play-eff '(filename effect ...)
	  "play a file with the effects applied. Effect may be a simple string or a list containing the string and additional parameters")
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
	       (begin (nodes-connect (append eff (list wf))) #t)))))
      (net-run net))))

(add-help 'save-eff '(input-filename output-filename effect ...)
	  "apply the effects to the input and write output")

;;; Macro to create a new filternetwork
;;;
;;; (create-net ((node "name") ...)
;;;             ((input-node "port" "label" "desc") ...)
;;;             ((output-node "port" "label" "desc") ...)
;;;             ((param-node "param" "label" "desc") ...)
;;;             body-expr) 
;;; --> new filternetwork

(define-macro (create-net nodes input output params body)
  `(let ((net (net-new)))
     ,(let lp ((nodes nodes))
	(if (null? nodes)
	    (let lp ((input input))
	       (if (null? input)
		   (let lp ((output output))
		     (if (null? output)
			 (let lp ((params params))
			   (if (null? params)
			       `(begin ,body net)
			       (let ((first (car params)))
			   `(begin (filternetwork_add_param net 
							     ,(car first)
							     ,(cadr first)
							     ,(caddr first)
							     ,(cadddr first))
				   ,(lp (cdr params))))))
			 (let ((first (car output)))
			   `(begin (filternetwork_add_output net 
							     ,(car first)
							     ,(cadr first)
							     ,(caddr first)
							     ,(cadddr first))
				   ,(lp (cdr output))))))
		   (let ((first (car input)))
		     `(begin (filternetwork_add_input net 
						      ,(car first)
						      ,(cadr first)
						      ,(caddr first)
						      ,(cadddr first))
			     ,(lp (cdr input))))))
	    `(let ((,(caar nodes) (net-add-node net ,(cadar nodes))))
	       ,(lp (cdr nodes)))))))


;--------------------------------------------------------------
;
; Macro filters for common use (and tutorial purposes)
;

;
; feedback echo2 macro filter
;
(filternetwork_to_filter
	(create-net ((extend "extend")
	     (mix2 "mix2")
	     (one2n "one2n")
	     (delay "delay")
	     (va "volume-adjust")) ; nodes
	    ((extend "in" "in" "echo source"))                     ; input
	    ((one2n "out" "out" "source with echo"))               ; output
	    ((delay "delay" "delay" "echo delay"); node param label desc
	     (va "factor" "mix" "echo mix ratio")
	     (extend "time" "extend" "time to extend")
	     (mix2 "gain" "gain" "output gain"))
	    (begin (filternode_set_param net "delay" 200)
		   (filternode_set_param net "extend" 600)
		   (filternode_set_param net "mix" 0.7)
		   (nodes-connect (list extend mix2 one2n delay va mix2))))
	"echo2" "echo as macro filter")

;
; mp3 reader using the pipe-in filter and mpg123
;
(filternetwork_to_filter
	(create-net ((p "pipe-in"))
	    ()
	    ((p "out" "out" "output"))
	    ((p "tail" "filename" "filename"))
	    (filternode_set_param p "cmd" "mpg123 -q -s "))
	"read-mp3" "mp3-reader")



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
