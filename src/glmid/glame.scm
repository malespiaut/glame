; glame.scm
; $Id: glame.scm,v 1.6 2000/03/20 00:27:24 mag Exp $
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


(define help
  (lambda ()
    (display "no help available yet.\n")))

; This scheme file is rated PG
; It contains violence, coarse language
; and maybe in future some sexscenes ;)

(define ficken
  (lambda ()
    (display "selber ficken!\n") (quit)))


;
; a test_latency implementation
;

(define (map-pairs f l)
  (if (or (null? l) (null? (cdr l)))
      #t
      (begin (f (car l) (cadr l))
	     (map-pairs f (cdr l)))))

(define create_nodes
  (lambda (net name nr)
    (if (< 0 nr)
      (append
         (create_nodes net name (- nr 1))
	 (list (filternetwork_add_node net name "nulls")))
      (list (filternetwork_add_node net name "nulls")))))

(define connect_linear
  (lambda (nodes)
    (map-pairs
      (lambda (s d) (filternetwork_add_connection s "out" d "in"))
      nodes)))

(define test_latency
  (lambda (depth)
    (let* ((net (filternetwork_new "tl" "test latency (linear)"))
	   (ping (filternetwork_add_node net "ping" "pinger"))
	   (nulls (create_nodes net "null" depth)))
      (connect_linear (cons ping (append nulls (list ping))))
      (filternetwork_launch net)
      (filternetwork_start net)
      (filternetwork_wait net)
      (filternetwork_delete net))))

;
; test save
;
(define while1
  (lambda (f)
    (if (boolean? (f)) #t (while1 f))))

(define testsave
  (lambda (iname oname)
    (let* ((net (filternetwork_new "testsave" "read and save files"))
           (rf (filternetwork_add_node net "read_file" "readfile"))
	   (wf (filternetwork_add_node net "write_file" "writefile")))
    (filternode_set_param rf "filename" iname)
    (filternode_set_param wf "filename" oname)
    (while1 (lambda () (filternetwork_add_connection rf "out" wf "in")))
    (filternetwork_launch net)
    (filternetwork_start net)
    (filternetwork_wait net)
    (filternetwork_delete net))))

;
; test rms
;

(define testrms
  (lambda (fname)
    (let* ((net (filternetwork_new "testrms" "obtain&debug rms stats"))
           (rf (filternetwork_add_node net "read_file" "readfile"))
	   (mix (filternetwork_add_node net "mix" "mixer"))
	   (stat (filternetwork_add_node net "statistic" "stats"))
	   (drms (filternetwork_add_node net "debugrms" "dbgrms")))
    (filternode_set_param rf "filename" fname)
    (while1 (lambda () (filternetwork_add_connection rf "out" mix "in")))
    (filternetwork_add_connection mix "out" stat "in")
    (filternetwork_add_connection stat "out" drms "in")
    (filternetwork_launch net)
    (filternetwork_start net)
    (filternetwork_wait net)
    (filternetwork_delete net))))

;
; play a soundfile
;

(define play
  (lambda (fname)
    (let* ((net (filternetwork_new "play" "plays files"))
	   (rf (filternetwork_add_node net "read_file" "readfile"))
	   (ao (filternetwork_add_node net "audio_out" "aout")))
      (filternode_set_param rf "filename" fname)
      (while1 (lambda () (filternetwork_add_connection rf "out" ao "in")))
      (filternetwork_launch net)
      (filternetwork_start net)
      (filternetwork_wait net)
      (filternetwork_delete net))))

;
; The "real" scheme glame midlayer - first try
;

(define net-new
  (lambda (name desc)
    (filternetwork_new name desc)))

(define net-add-nodes
  (lambda (net nodes)
    (if (string? nodes) (net-add net (list nodes))
	(if (null? nodes) '()
	    (cons (filternetwork_add_node net (car nodes) "")
		  (net-add net (cdr nodes)))))))

(define nodes-connect
  (lambda (nodes)
    (map-pairs
      (lambda (s d) (filternetwork_add_connection s "out" d "in"))
      nodes)))


;
; support the filternetwork2string syntax
;

(define FIXME (lambda () #f))

(define filternetwork
  (lambda (name description . cmds)
    (let* ((net (filternetwork_new name description))
	   (node (lambda (name filter . cmds)
		   (let* ((node (filternetwork_add_node net filter name))
			  (export-input (lambda (extlabel label desc) (FIXME)))
			  (export-output (lambda (extlabel label desc) (FIXME)))
			  (export-param (lambda (extlabel label desc) (FIXME)))
			  (set-param (lambda (label value) (FIXME))))
		     (map eval cmds)
		     node)))
	   (connect (lambda (node1 port1 node2 port2)
		      (let* ((pipe (filternetwork_add_connection node1 port1 node2 port2))
			     (set-sourceparam (lambda (label value) (FIXME)))
			     (set-destparam (lambda (label value) (FIXME))))
			(map eval cmds)
			pipe)))
	   (set-param (lambda (label value) (FIXME))))
      (map eval cmds)
      net)))

