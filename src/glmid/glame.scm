; glame.scm
; $Id: glame.scm,v 1.49 2001/03/21 09:20:10 richi Exp $
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

(define net-new filter_creat)

(add-help 'net-new '() "generate a new filter network")

; (net-add-node net "node" '("param" val) ...)
; (net-add-node net "node")
(define net-add-node
  (lambda (net node . params)
    (let ((n (filter_instantiate (plugin_get node))))
      (if (eq? (filter_add_node net n node) #f) (begin (display "no node ") 
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
		    (delete n)
		    #f)
		  (cons n nn)))))))

(add-help 'net-add-nodes '(net '("node" '("param" val) ...) ...)
	  "create filters and add it to the network")

; (nodes-delete node node ...)
(define nodes-delete
  (lambda nodes
    (if (null? nodes) '()
	(begin
	  (delete (car nodes))
	  (apply nodes-delete (cdr nodes))))))

(add-help 'nodes-delete '(node ...) "delete nodes")

; linear connect the nodes
; (nodes-connect `(,node ,node ...) `(..) ...)
(define nodes-connect
  (lambda nodes
    (map (lambda (nodes)
	   (map-pairs
	    (lambda (s d) (filter_connect s "out" d "in"))
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
    (if (eq? #t (filter_launch net))
	(begin
	  (filter_start net)
	  (filter_wait net)))))

(add-help 'net-run '(net)
	  "run the net, wait for completion and delete it.")

; start the net (in the background, not deleting it afterwards),
; i.e. you can use filter_(pause|start|terminate) on it.
(define net-run-bg
  (lambda (net)
    (if (eq? #t (filter_launch net))
	(filter_start net))
    net))

(add-help 'net-run-bg '(net)
	  "run the net in the background")
;-----------------------------------------------------------------
;
; file/swapfile/audio I/O stuff
;

;
; high level swapfile helper
;

(define swapfile
  (lambda (fname)
    (begin
      (swapfile_close)
      (swapfile_creat fname (* 32 1024 1024))
      (swapfile_open fname))))

(define sw-ls
  (lambda ()
    (letrec ((dir (sw_opendir))
	     (sw-list-directory
	      (lambda (dir)
		(let ((entry (sw_readdir dir)))
		  (cond ((= entry -1) #f)
			(else (let* ((fd (sw_open entry O_RDONLY TXN_NONE))
				     (st (sw_fstat fd))
				     (size (sw-st-size st)))
				(display (cons entry (cons size (sw_read_string fd (min 8 size))))) (newline)
				(sw_close fd)
				(sw-list-directory dir))))))))
      (sw-list-directory dir)
      (sw_closedir dir))))

(define sw-creat
  (lambda (fname . contents)
    (let ((fd (sw_open fname (+ O_CREAT O_RDWR O_TRUNC) TXN_NONE)))
      (map (lambda (str)
	     (let ((tfd (sw_open 1278369 (+ O_CREAT O_RDWR O_TRUNC O_EXCL) TXN_NONE)))
	       (sw_write tfd str)
	       (sw_lseek tfd 0 SEEK_SET)
	       (sw_sendfile fd tfd (string-length str) SWSENDFILE_INSERT)
	       (sw_close tfd)
	       (sw_unlink 1278369)))
	   contents)
      (sw_lseek fd 0 SEEK_SET)
      fd)))

(define sw-open
  (lambda (fname)
    (sw_open fname O_RDWR TXN_NONE)))

(define sw-close
  (lambda (fd)
    (sw_close fd)))

(define sw-st-name (lambda (st) (car st)))
(define sw-st-size (lambda (st) (cadr st)))
(define sw-st-mode (lambda (st) (caddr st)))
(define sw-st-offset (lambda (st) (cadddr st)))
(define sw-st-cluster_start (lambda (st) (car (cddddr st))))
(define sw-st-cluster_end (lambda (st) (cadr (cddddr st))))
(define sw-st-cluster_size (lambda (st) (caddr (cddddr st))))

(define sw-stat
  (lambda (fd)
    (let ((st (sw_fstat fd)))
      (list "Name:" (sw-st-name st)
	    "Size:" (sw-st-size st)
	    "Offset:" (sw-st-offset st)
	    "Cluster:" (sw-st-cluster_start st) (sw-st-cluster_size st)
	    "raw:" st))))

(define sw-clusters
  (lambda (fd)
    (letrec ((off (sw-st-offset (sw_fstat fd)))
	     (sw-display-clusters
	      (lambda (fd)
		(let ((size (sw-st-cluster_size (sw_fstat fd))))
		  (cond ((<= size 0) #f)
			(else (let ((data (sw_read_string fd size)))
				(display (cons size data))
				(sw-display-clusters fd))))))))
      (sw_lseek fd 0 SEEK_SET)
      (sw-display-clusters fd) (newline)
      (sw_lseek fd off SEEK_SET)
      #t)))

(define sw-contents
  (lambda (fd)
    (let ((st (sw_fstat fd)))
      (sw_lseek fd 0 SEEK_SET)
      (let ((data (sw_read_string fd (sw-st-size st))))
	(sw_lseek fd (sw-st-offset st) SEEK_SET)
	data))))

(define sw-display
  (lambda (fd)
    (begin
      (display (sw-stat fd)) (newline)
      (display (sw-contents fd)) (newline))))

(define swtest
  (lambda (name test result)
    (let ((res (test)))
      (begin
	(display name)
	(cond ((equal? res result) (begin (display " passed.\n") #t))
	      (else (begin
		      (display " failed.\n")
		      (display res) (newline)
		      (display "Should be\n")
		      (display result) (newline)
		      (display "*** Test not passed. ***\n")
		      #f)))))))

(define swtest-truncate
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd (sw-creat 999)))
		    (sw_write fd "Hello World!")
		    (sw_ftruncate fd 5)
		    (sw_ftruncate fd 64)
		    (let ((res (sw-contents fd)))
		      (sw_close fd)
		      (sw_unlink 999)
		      res)))))
      (swtest "Truncate" test "Hello"))))

(define swtest-rw-simple
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd (sw-creat 999)))
		    (sw_write fd "Hello")
		    (sw_write fd "World!")
		    (sw_lseek fd 5 SEEK_SET)
		    (sw_write fd " World!")
		    (let ((res (sw-contents fd)))
		      (sw_close fd)
		      (sw_unlink 999)
		      res)))))
      (swtest "Simple read-write" test "Hello World!"))))

(define swtest-cut-aligned
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd (sw-creat 999 "Hallo " "Leute, " "wie gehts?")))
		    (sw_lseek fd 6 SEEK_SET)
		    (sw_sendfile SW_NOFILE fd 7 SWSENDFILE_CUT)
		    (let ((result (sw-contents fd)))
		      (sw_close fd)
		      (sw_unlink 999)
		      result)))))
      (swtest "Aligned cut" test "Hallo wie gehts?"))))

(define swtest-cut-unaligned
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd (sw-creat 999 "Hallo Leute, wie gehts?")))
		    (sw_lseek fd 6 SEEK_SET)
		    (sw_sendfile SW_NOFILE fd 7 SWSENDFILE_CUT)
		    (let ((result (sw-contents fd)))
		      (sw_close fd)
		      (sw_unlink 999)
		      result)))))
      (swtest "Unaligned cut" test "Hallo wie gehts?"))))

(define swtest-cut-tail
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd (sw-creat 999 "Hallo Leute, wie gehts?")))
		    (sw_lseek fd -5 SEEK_END)
		    (sw_sendfile SW_NOFILE fd 6 SWSENDFILE_CUT)
		    (let ((result (sw-contents fd)))
		      (sw_close fd)
		      (sw_unlink 999)
		      result)))))
      (swtest "Tail cut" test "Hallo Leute, wie "))))

(define swtest-cut-tail-aligned
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd (sw-creat 999 "Hallo " "Leute, wie " "gehts?")))
		    (sw_lseek fd -5 SEEK_END)
		    (sw_sendfile SW_NOFILE fd 6 SWSENDFILE_CUT)
		    (let ((result (sw-contents fd)))
		      (sw_close fd)
		      (sw_unlink 999)
		      result)))))
      (swtest "Aligned tail cut" test "Hallo Leute, wie "))))

(define swtest-cut-tail-unaligned
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd (sw-creat 999 "Hallo " "Leute," " wie gehts?")))
		    (sw_lseek fd -5 SEEK_END)
		    (sw_sendfile SW_NOFILE fd 6 SWSENDFILE_CUT)
		    (let ((result (sw-contents fd)))
		      (sw_close fd)
		      (sw_unlink 999)
		      result)))))
      (swtest "Unaligned tail cut" test "Hallo Leute, wie "))))

(define swtest-cut-head
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd (sw-creat 999 "Hallo Leute, wie gehts?")))
		    (sw_lseek fd 0 SEEK_SET)
		    (sw_sendfile SW_NOFILE fd 6 SWSENDFILE_CUT)
		    (let ((result (sw-contents fd)))
		      (sw_close fd)
		      (sw_unlink 999)
		      result)))))
      (swtest "Head cut" test "Leute, wie gehts?"))))

(define swtest-cut-head-aligned
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd (sw-creat 999 "Hallo " "Leute, wie" " gehts?")))
		    (sw_lseek fd 0 SEEK_SET)
		    (sw_sendfile SW_NOFILE fd 6 SWSENDFILE_CUT)
		    (let ((result (sw-contents fd)))
		      (sw_close fd)
		      (sw_unlink 999)
		      result)))))
      (swtest "Aligned head cut" test "Leute, wie gehts?"))))

(define swtest-cut-head-unaligned
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd (sw-creat 999 "Hallo Leute," " wie" " gehts?")))
		    (sw_lseek fd 0 SEEK_SET)
		    (sw_sendfile SW_NOFILE fd 6 SWSENDFILE_CUT)
		    (let ((result (sw-contents fd)))
		      (sw_close fd)
		      (sw_unlink 999)
		      result)))))
      (swtest "Unaligned head cut" test "Leute, wie gehts?"))))

(define swtest-insert-aligned
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd1 (sw-creat 998 "Hallo " "wie gehts?"))
			(fd2 (sw-creat 999 "Leute, ")))
		    (sw_lseek fd1 6 SEEK_SET)
		    (sw_lseek fd2 0 SEEK_SET)
		    (sw_sendfile fd1 fd2 7 SWSENDFILE_INSERT)
		    (let ((result (sw-contents fd1)))
		      (sw_close fd1)
		      (sw_close fd2)
		      (sw_unlink 998)
		      (sw_unlink 999)
		      result)))))
      (swtest "Aligned insert" test "Hallo Leute, wie gehts?"))))

(define swtest-insert-unaligned
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd1 (sw-creat 998 "Hallo wie gehts?"))
			(fd2 (sw-creat 999 "Bla Leute, Blubb")))
		    (sw_lseek fd1 6 SEEK_SET)
		    (sw_lseek fd2 4 SEEK_SET)
		    (sw_sendfile fd1 fd2 7 SWSENDFILE_INSERT)
		    (let ((result (sw-contents fd1)))
		      (sw_close fd1)
		      (sw_close fd2)
		      (sw_unlink 998)
		      (sw_unlink 999)
		      result)))))
      (swtest "Unaligned insert" test "Hallo Leute, wie gehts?"))))

(define swtest-filecow
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd1 (sw-creat 998 "Hallo wie gehts?"))
			(fd2 (sw-creat 999)))
		    (sw_lseek fd1 0 SEEK_SET)
		    (sw_ftruncate fd2 16)
		    (sw_lseek fd2 0 SEEK_SET)
		    (sw_sendfile fd2 fd1 16 0)
		    (let ((result (sw-contents fd2)))
		      (sw_close fd1)
		      (sw_close fd2)
		      (sw_unlink 998)
		      (sw_unlink 999)
		      result)))))
      (swtest "file COW" test "Hallo wie gehts?"))))

(define swtest-all
  (lambda ()
    (and (swtest-rw-simple)
	 (swtest-truncate)
	 (swtest-cut-aligned)
	 (swtest-cut-unaligned)
	 (swtest-cut-head)
	 (swtest-cut-head-aligned)
	 (swtest-cut-head-unaligned)
	 (swtest-cut-tail)
	 (swtest-cut-tail-aligned)
	 (swtest-cut-tail-unaligned)
	 (swtest-insert-aligned)
	 (swtest-insert-unaligned)
	 (swtest-filecow))))



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
         (lambda () (filter_connect rf "out" ao "in")))
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
		(conn (filter_connect rf "out" (car eff) "in")))
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
		(conn (filter_connect rf "out" (car eff) "in")))
	   (if (eq? conn #f)
	       (begin (apply nodes-delete eff) #f)
	       (begin (nodes-connect (append eff (list wf))) #t)))))
      (net-run net))))

(add-help 'save-eff '(input-filename output-filename effect ...)
	  "apply the effects to the input and write output")


;
; read a file into one or two swapfiles
;

(define file-to-swap
  (lambda (fname sf1 sf2)
    (let* ((net (net-new))
	   (rf (net-add-node net read-file))
	   (so1 (net-add-node net "swapfile-out"))
	   (so2 (net-add-node net "swapfile-out")))
      (node-set-params rf `("filename" ,fname))
      (node-set-params so1 `("filename" ,sf1))
      (node-set-params so2 `("filename" ,sf2))
      (nodes-connect `(,rf ,so1))
      (nodes-connect `(,rf ,so2))
      (net-run net))))


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
(let ((plugin (glame_create_plugin
	(create-net ((extend "extend")
	     (mix "mix")
	     (one2n "one2n")
	     (delay "delay")
	     (va "volume-adjust")) ; nodes
	    ((extend "in" "in" "echo source"))                     ; input
	    ((one2n "out" "out" "source with echo"))               ; output
	    ((delay "delay" "delay" "echo delay"); node param label desc
	     (va "factor" "mix" "echo mix ratio")
	     (extend "time" "extend" "time to extend")
	     (mix "gain" "gain" "output gain"))
	    (begin (filternode_set_param net "delay" 200)
		   (filternode_set_param net "extend" 600)
		   (filternode_set_param net "mix" 0.7)
		   (nodes-connect (list extend mix one2n delay va mix))))
	"echo2")))
        (plugin_set plugin PLUGIN_DESCRIPTION "echo as macro filter")
	(plugin_set plugin PLUGIN_CATEGORY "Effects"))

;
; mp3 reader using the pipe-in filter and mpg123
; FIXME: does not work due to bug in pipe-in -- disabled
;
;(let ((plugin (glame_create_plugin
;		(create-net ((p "pipe-in"))
;	    		()
;	    		((p "out" "out" "output"))
;	    		((p "tail" "filename" "filename"))
;	    		(filternode_set_param p "cmd" "mpg123 -q -s "))
;		"read-mp3")))
;	(plugin_set plugin PLUGIN_DESCRIPTION "mp3-reader")
;	(plugin_set plugin PLUGIN_CATEGORY "InOut"))

;
; load an external representation of a filter and register it as a plugin
; using the specified plugin name and filename
;
(define (load-and-register-macro filename pluginname)
	(let* ((port (open-input-file filename))
	       (macro (read port))
	       (eof (read port)))
		(close-input-port port)
		(if (and (not (eof-object? macro)) (eof-object? eof))
			(glame_create_plugin (eval macro) pluginname)
			#f)))


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
      (lambda () (filter_connect rf "out" wf "in")))
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
      (lambda () (filter_connect rf "out" mix "in")))
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
      (lambda () (filter_connect rf "out" mix "in")))
    (nodes-connect  `(,mix ,iir ,stat ,drms))
    (net-run net))))
