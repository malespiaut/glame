; glame.scm
; $Id: glame.scm,v 1.9 2000/03/21 09:40:09 richi Exp $
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
    (if (boolean? (f)) #t (while-not-false f))))

(define repeat-n
  (lambda (x n)
    (if (>= 0 n) '() (cons x (repeat-n x (- n 1))))))


;
; some high level filternetwork helpers
;

(define net-new filternetwork_new)

(define net-add-node
  (lambda (net node)
    (if (boolean? (filter_get node)) (plugin_get node))
    (filternetwork_add_node net node "")))

(define net-add-nodes
  (lambda (net nodes)
    (if (null? nodes) '()
	(cons (net-add-node net (car nodes))
	      (net-add-nodes net (cdr nodes))))))

(define nodes-connect
  (lambda nodes
    (map (lambda (nodes)
	   (map-pairs
	    (lambda (s d) (filternetwork_add_connection s "out" d "in"))
	    nodes))
	 nodes)))

(define net-run
  (lambda (net)
    (filternetwork_launch net)
    (filternetwork_start net)
    (filternetwork_wait net)
    (filternetwork_delete net)))



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
    (let *((net (name)))
      (map (lambda (p) (filternode_set_param net (car p) (cadr p))) params)
      (net-run net))))


;
; test save
;

(define testsave
  (lambda (iname oname)
    (let* ((net (filternetwork_new))
           (rf (filternetwork_add_node net "read_file" "readfile"))
	   (wf (filternetwork_add_node net "write_file" "writefile")))
    (filternode_set_param rf "filename" iname)
    (filternode_set_param wf "filename" oname)
    (while-not-false
      (lambda () (filternetwork_add_connection rf "out" wf "in")))
    (net-run net))))

;
; test rms
;

(define testrms
  (lambda (fname)
    (let* ((net (filternetwork_new))
           (rf (net-add-node net "read_file"))
	   (mix (net-add-node net "mix"))
	   (stat (net-add-node net "statistic"))
	   (drms (net-add-node net "debugrms")))
    (filternode_set_param rf "filename" fname)
    (while-not-false 
      (lambda () (filternetwork_add_connection rf "out" mix "in")))
    (nodes-connect  `(,mix ,stat ,drms))
    (net-run net))))

;
; play a soundfile - automagically finds the number of channels
;

(define play
  (lambda (fname)
    (let* ((net (filternetwork_new))
	   (rf (filternetwork_add_node net "read_file" "readfile"))
	   (ao (filternetwork_add_node net audio-out "aout")))
      (filternode_set_param rf "filename" fname)
      (while-not-false
         (lambda () (filternetwork_add_connection rf "out" ao "in")))
      (net-run net))))

;
; play-echo, created out of net_test_echo2, i.e. use
; with test-network to set parameters.
;

(define play-echo
  (lambda ()
    (let* ((net (net-new))
	   (rf (net-add-node net "read_file"))
	   (mix (net-add-node net "mix"))
	   (delay (net-add-node net "delay"))
	   (va (net-add-node net "volume-adjust"))
	   (dup (net-add-node net "one2n"))
	   (aout (net-add-node net audio-out)))
      (filternetwork_add_param net rf "filename" "filename" "filename")
      (filternetwork_add_param net va "factor" "mix" "mix")
      (filternetwork_add_param net delay "delay" "delay" "delay")
      (filternode_set_param delay "delay" 200)
      (filternode_set_param va "factor" 0.9)
      (nodes-connect `(,rf ,mix) `(,rf ,mix ,dup ,aout) `(,dup ,delay ,va ,mix))
      net)))


;
; play a file with automagically determining #channels _and_
; applying one effect (with a list of parameters applied to it)
; example: (play-eff "test.wav" "echo" '("time" 500))
;

(define play-eff
  (lambda (fname effect . params)
    (let* ((net (net-new))
	   (rf (net-add-node net "read_file"))
	   (mix (filternetwork_add_node net "mix" "play-eff-mix"))
	   (eff (net-add-node net effect))
	   (ao (net-add-node net audio-out)))
      (filternode_set_param rf "filename" fname)
      (while-not-false
         (lambda () (filternetwork_add_connection rf "out" mix "in")))
      (nodes-connect `(,mix ,eff ,ao))
      (map (lambda (p) (filternode_set_param eff (car p) (cadr p))) params)
      (net-run net))))

;
; an echo macro filter
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
  (filternode_set_param net "delay" 200)
  (filternode_set_param net "extend" 1000)
  (filternode_set_param net "mix" 0.9)
  (nodes-connect `(,extend ,mix2 ,one2n ,delay ,va ,mix2))
  (filternetwork_to_filter net "echo2" "echo as macro filter"))
