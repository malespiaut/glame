; glame.scm
; $Id: glame.scm,v 1.12 2000/03/25 15:56:25 richi Exp $
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
    (display "no help available yet.\n")))

; what output filter should we use?
(define audio-out "audio_out")


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
    (if (eqv? #f (f)) #t (while-not-false f))))

(define repeat-n
  (lambda (x n)
    (if (>= 0 n) '() (cons x (repeat-n x (- n 1))))))


;
; some high level filternetwork helpers
;

(define net-new filternetwork_new)

; (net-add-node net "node")
(define net-add-node
  (lambda (net node)
    (if (boolean? (filter_get node)) (plugin_get node))
    (filternetwork_add_node net node "")))

; (net-add-nodes net '("node" "node" ...))
(define net-add-nodes
  (lambda (net nodes)
    (if (null? nodes) '()
	(cons (net-add-node net (car nodes))
	      (net-add-nodes net (cdr nodes))))))

; linear connect the nodes
; (nodes-connect `(,node ,node ...) `(..) ...)
(define nodes-connect
  (lambda nodes
    (map (lambda (nodes)
	   (map-pairs
	    (lambda (s d) (filternetwork_add_connection s "out" d "in"))
	    nodes))
	 nodes)))

; (node-set-param node "label" value)
(define node-set-param
  (lambda (node label value)
    (filternode_set_param node label value)))

; (node-set-params node `("label" ,value) `("label" ...) ...)
(define node-set-params
  (lambda (node . params)
    (map (lambda (p)
	   (if (not (null? p))
	       (node-set-param node (car p) (cadr p))))
	 params)))

; (nodes-set-params `(,node ,node ..) `("label" ,value) ...)
(define nodes-set-params
  (lambda (nodes . params)
    (map (lambda (n) (node-set-params n params)) nodes)))

; run the net, wait for completion and delete it
(define net-run
  (lambda (net)
    (if (eqv? #t (filternetwork_launch net))
	(begin
	  (filternetwork_start net)
	  (filternetwork_wait net)
	  (filternetwork_delete net)))))

; start the net (in the background, not deleting it afterwards),
; i.e. you can use filternetwork_(pause|start|terminate) on it.
(define net-run-bg
  (lambda (net)
    (if (eqv? #t (filternetwork_launch net))
	(filternetwork_start net))
    net))


;
; high level swapfile/track helper
;

(define file-to-track
  (lambda (file group track)
    (let* ((net (net-new))
	   (rf (net-add-node net "read_file"))
	   (i 1))
      (filternode_set_param rf "filename" file)
      (while-not-false
       (lambda ()
	 (let ((to (net-add-node net "track_out")))
	   (if (boolean? (filternetwork_add_connection rf "out" to "in"))
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
	   (ti (net-add-node net "track_in"))
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
	   (nodes (net-add-nodes net (cons "ping" (repeat-n "null" depth)))))
      (nodes-connect (append nodes (list (car nodes))))
      (net-run net))))

(define test-network
  (lambda (name . params)
    (let ((net (name)))
      (node-set-params net params)
      (net-run net))))


;
; test save
;

(define testsave
  (lambda (iname oname)
    (let* ((net (net-new))
           (rf (net-add-node net "read_file"))
	   (wf (net-add-node net "write_file")))
    (node-set-param rf "filename" iname)
    (node-set-param wf "filename" oname)
    (while-not-false
      (lambda () (filternetwork_add_connection rf "out" wf "in")))
    (net-run net))))

;
; test rms
;

(define testrms
  (lambda (fname)
    (let* ((net (net-new))
           (rf (net-add-node net "read_file"))
	   (mix (net-add-node net "mix"))
	   (stat (net-add-node net "statistic"))
	   (drms (net-add-node net "debugrms")))
    (node-set-param rf "filename" fname)
    (while-not-false 
      (lambda () (filternetwork_add_connection rf "out" mix "in")))
    (nodes-connect  `(,mix ,stat ,drms))
    (net-run net))))


;
; play a soundfile - automagically finds the number of channels
;

(define play
  (lambda (fname)
    (let* ((net (net-new))
	   (rf (net-add-node net "read_file"))
	   (ao (net-add-node net audio-out)))
      (node-set-param rf "filename" fname)
      (while-not-false
         (lambda () (filternetwork_add_connection rf "out" ao "in")))
      (net-run net))))


;
; play a file with automagically determining #channels _and_
; applying an effect (with a list of parameters applied to it)
; example: (play-eff "test.wav" "echo" '("time" 500))
;

(define play-eff
  (lambda (fname effect . params)
    (let* ((net (net-new))
	   (rf (net-add-node net "read_file"))
	   (ao (net-add-node net audio-out)))
      (node-set-param rf "filename" fname)
      (while-not-false
       (lambda ()
	 (let* ((eff (net-add-node net effect))
		(conn (filternetwork_add_connection rf "out" eff "in")))
	   (if (boolean? conn)
	       (begin (filternetwork_delete_node eff) #f)
	       (begin
		 (node-set-params eff params)
		 (nodes-connect `(,eff ,ao))
		 conn)))))
      (net-run net))))


;
; feedback echo2 macro filter
;

(let* ((net (net-new))
       (extend (net-add-node net "extend"))
       (mix2 (net-add-node net "mix"))
       (one2n (net-add-node net "one2n"))
       (delay (net-add-node net "delay"))
       (va (net-add-node net "volume-adjust")))
  (filternetwork_add_input net extend "in" "in" "echo source")
  (filternetwork_add_output net one2n "out" "out" "source with echo")
  (filternetwork_add_param net delay "delay" "delay" "echo delay")
  (filternetwork_add_param net va "factor" "mix" "echo mix ratio")
  (filternetwork_add_param net extend "time" "extend" "time to extend")
  (filternetwork_add_param net mix2 "gain" "gain" "output gain")
  (filternode_set_param net "delay" 200)
  (filternode_set_param net "extend" 600)
  (filternode_set_param net "mix" 0.7)
  (nodes-connect `(,extend ,mix2 ,one2n ,delay ,va ,mix2))
  (filternetwork_to_filter net "echo2" "echo as macro filter"))

;
; feedforward echo3 macro filter
;

(let* ((net (net-new))
       (extend (net-add-node net "extend"))
       (mix2 (net-add-node net "mix"))
       (one2n (net-add-node net "one2n"))
       (delay (net-add-node net "delay"))
       (va (net-add-node net "volume-adjust")))
  (filternetwork_add_input net one2n "in" "in" "echo source")
  (filternetwork_add_output net mix2 "out" "out" "source with echo")
  (filternetwork_add_param net delay "delay" "delay" "echo delay")
  (filternetwork_add_param net va "factor" "mix" "echo mix ratio")
  (filternetwork_add_param net extend "time" "extend" "time to extend")
  (filternetwork_add_param net mix2 "gain" "gain" "output gain")
  (filternode_set_param net "delay" 200)
  (filternode_set_param net "extend" 200)
  (filternode_set_param net "mix" 0.7)
  (nodes-connect `(,one2n ,extend ,mix2) `(,one2n ,delay ,va ,mix2))
  (filternetwork_to_filter net "echo3" "echo as macro filter"))

;
; 'bouncing' macro filter, stereo needed
;

(let* ((net (net-new))
       (one2n11 (net-add-node net "one2n"))
       (one2n12 (net-add-node net "one2n"))
       (one2n21 (net-add-node net "one2n"))
       (one2n22 (net-add-node net "one2n"))
       (mix11 (net-add-node net "mix"))
       (mix12 (net-add-node net "mix"))
       (mix21 (net-add-node net "mix"))
       (mix22 (net-add-node net "mix"))
       (delay1 (net-add-node net "delay"))
       (delay2 (net-add-node net "delay")))
  (filternetwork_add_input net one2n11 "in" "left-in" "left input")
  (filternetwork_add_input net one2n21 "in" "right-in" "right input")
  (filternetwork_add_output net mix12 "out" "left-out" "left output")
  (filternetwork_add_output net mix22 "out" "right-out" "right output")
  (filternetwork_add_param net delay1 "delay" "delay1" "delay")
  (filternetwork_add_param net delay2 "delay" "delay2" "delay")
  (filternode_set_param net "delay1" 500)
  (filternode_set_param net "delay2" 500)
  (nodes-connect `(,one2n11 ,mix11 ,delay1 ,one2n12 ,mix12) `(,one2n11 ,mix12)
		 `(,one2n21 ,mix21 ,delay2 ,one2n22 ,mix22) `(,one2n21 ,mix22)
		 `(,one2n12 ,mix21) `(,one2n22 ,mix11))
  (filternetwork_to_filter net "bounce" "bounce"))

(define test-bounce
  (lambda (delay)
    (let* ((net (net-new))
	   (sinus (net-add-node net "sinus"))
	   (const (net-add-node net "const"))
	   (repeat (net-add-node net "repeat"))
	   (extend1 (net-add-node net "extend"))
	   (extend2 (net-add-node net "extend"))
	   (boun (net-add-node net "bounce"))
	   (ao (net-add-node net audio-out)))
      (filternode_set_param repeat "time" 1000)
      (filternode_set_param extend1 "time" 10000)
      (filternode_set_param extend2 "time" 11000)
      (nodes-connect `(,sinus ,repeat ,extend1) `(,const ,extend2)) 
      (filternetwork_add_connection extend1 "out" boun "left-in")
      (filternetwork_add_connection extend2 "out" boun "right-in")
      (filternetwork_add_connection boun "left-out" ao "in")
      (filternetwork_add_connection boun "right-out" ao "in")
      (net-run net))))