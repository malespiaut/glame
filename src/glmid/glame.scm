; glame.scm
; $Id: glame.scm,v 1.82 2004/10/23 13:09:23 richi Exp $
;
; Copyright (C) 2000, 2001, 2002, 2003 Richard Guenther, Martin Gasbichler
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

; Compatibility
(define filter_creat filter-new)
(define filter_instantiate filter-new)
(define filter_to_string filter->string)
(define filter_add_node filter-add-node)
(define filter_connect filter-connect)
(define filternetwork_add_input filternetwork-add-input)
(define filternetwork_add_output filternetwork-add-output)
(define filternetwork_add_param filternetwork-add-param)
(define set_property set-property!)
(define get_property get-property)
(define filter_p filter?)


; Parameter setting
; specs are pairs of param label and param value

(define (set-params! par . specs)
  (let* ((params (map (lambda (param)
			(cons (param-label param) param))
		      par))
	 (pairs (map (lambda (spec)
		       (let ((p (assoc (car spec) params)))
			 (if p
			     (cons (cdr p) (cdr spec))
			     (throw 'glame-error))))
		     specs)))
    (for-each (lambda (p)
		(param-set! (car p) (cdr p)))
	      pairs)))

(define (filter-set-param! node label value)
  (set-params! (filter-params node) (cons label value)))

(define (filter-set-params! node . specs)
  (apply set-params! (cons (filter-params node) specs)))

(define (pipe-set-sourceparam! pipe label value)
  (set-params! (pipe-source-params pipe) (cons label value)))

(define (pipe-set-sourceparams! pipe . specs)
  (apply set-params! (cons (pipe-source-params pipe) specs)))

(define (pipe-set-destparam! pipe label value)
  (set-params! (pipe-dest-params pipe) (cons label value)))

(define (pipe-set-destparams! pipe . specs)
  (apply set-params! (cons (pipe-dest-params pipe) specs)))


; Compatibility
(define filternode_set_param filter-set-param!)
(define filterpipe_set_sourceparam pipe-set-sourceparam!)
(define filterpipe_set_destparam pipe-set-destparam!)
(define (node-set-params node . params)
  (apply filter-set-params!
	 (cons node
	       (map (lambda (spec)
		      (cons (car spec) (cadr spec)))
		    params))))



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


; GLAME config

;(define glame-config '((blah . (define x 'x)) (test . (display 'done))))
(if (not (defined? 'glame-config)) 
    (define glame-config '()))

; set the cdr of the key . value pair to the provided key or create
; a new pair of the both
(define (glame-config-set! key value)
  (let ((cfg (assq key glame-config)))
    (if cfg 
	(set-cdr! cfg value)
	(set! glame-config (cons (cons key value) glame-config)))))

; return the cdr of the key . value pair, or create a pair with
; the provided default value and return that
(define (glame-config-get key . default)
  (let ((cfg (assq key glame-config)))
    (if cfg 
	(cdr cfg)
	(begin
	  (if (null? default) 
	      (throw 'glame-error)
	      (glame-config-set! key (car default)))
	  (car default)))))

; GLAME 0.2.0 compatibility
(define (plugin_description p) (plugin-query p "desc"))

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

(define (fold-right f accu l)
  (if (null? l)
      accu
      (f (car l) (fold-right f accu (cdr l)))))

(define (satisfies-all? pred l)
  (if (null? l)
      #t
      (and (pred (car l))
	   (satisfies-all? pred (cdr l)))))

;
; some high level filternetwork helpers
;

(define net-new filter-new)

(add-help 'net-new '() "generate a new filter network")

; (net-add-node net "node" '("param" val) ...)
; (net-add-node net "node")
(define net-add-node
  (lambda (net node . params)
    (let ((n (filter-new (plugin-get node))))
      (filter-add-node net n node)
      (apply node-set-params n params)
      n)))

(add-help 'net-add-node '(net "node" ("param" val) ...) 
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
		    (filter-delete n)
		    #f)
		  (cons n nn)))))))

(add-help 'net-add-nodes '(net ("node" ("param" val) ...) ...)
	  "create filters and add it to the network")

; (nodes-delete node node ...)
(define nodes-delete
  (lambda nodes
    (if (null? nodes) '()
	(begin
	  (filter-delete (car nodes))
	  (apply nodes-delete (cdr nodes))))))

(add-help 'nodes-delete '(node ...) "delete nodes")

; linear connect the nodes
; (nodes-connect (list node node ...) (list ...) ...)
(define nodes-connect
  (lambda nodes
    (map (lambda (nodes)
	   (map-pairs
	    (lambda (s d) (filter-connect s "out" d "in"))
	    nodes))
	 nodes)))

(add-help 'nodes-connect '((node ...) ...)
	  "linear connect the nodes of each list")

(define (node-connect-n source sport dest dport)
  (let ((pipe (catch 'glame-error
		     (lambda () (filter-connect source sport dest dport))
	 	     (lambda args #f))))
    (if (pipe? pipe)
	(cons pipe (node-connect-n source sport dest dport))
	'())))

(add-help 'node-connect-n '(source port-name dest port-name)
	  "connect both nodes as much times as possible")


; run the net, wait for completion and delete it
(define net-run
  (lambda (net . bufsize)
    (let ((context
	   (if (null? bufsize)
	       (filter-launch net)
	       (filter-launch net (car bufsize)))))
      (filter-start context)
      (filter-wait context))))

(add-help 'net-run '(net)
	  "run the net, wait for completion and delete it.")

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
    (letrec ((dir (sw-opendir))
	     (sw-list-directory
	      (lambda (dir)
		(let ((entry (sw-readdir dir)))
		  (cond ((= entry -1) #f)
			(else (let* ((fd (sw-open entry O_RDONLY))
				     (st (sw-fstat fd))
				     (size (sw-st-size st)))
				(write (list entry size (sw-read-string fd (min 8 size))))
				(newline)
				(sw-close fd)
				(sw-list-directory dir))))))))
      (sw-list-directory dir)
      (sw-closedir dir))))

(define sw-creat
  (lambda (fname . contents)
    (let ((fd (sw-open fname (+ O_CREAT O_RDWR O_TRUNC))))
      (for-each
        (lambda (str)
	  (let ((tfd (sw-open 1278369 (+ O_CREAT O_RDWR O_TRUNC O_EXCL))))
	    (sw-write tfd str)
	    (sw-lseek tfd 0 SEEK_SET)
	    (sw-sendfile fd tfd (string-length str) SWSENDFILE_INSERT)
	    (sw-close tfd)
	    (sw-unlink 1278369)))
	contents)
      (sw-lseek fd 0 SEEK_SET)
      fd)))

(define sw-st-name (lambda (st) (car st)))
(define sw-st-size (lambda (st) (cadr st)))
(define sw-st-mode (lambda (st) (caddr st)))
(define sw-st-offset (lambda (st) (cadddr st)))
(define sw-st-cluster_start (lambda (st) (car (cddddr st))))
(define sw-st-cluster_end (lambda (st) (cadr (cddddr st))))
(define sw-st-cluster_size (lambda (st) (caddr (cddddr st))))

(define sw-stat
  (lambda (fd)
    (let ((st (sw-fstat fd)))
      (list "Name:" (sw-st-name st)
	    "Size:" (sw-st-size st)
	    "Offset:" (sw-st-offset st)
	    "Cluster:" (sw-st-cluster_start st) (sw-st-cluster_size st)
	    "raw:" st))))

(define sw-clusters
  (lambda (fd)
    (letrec ((off (sw-st-offset (sw-fstat fd)))
	     (sw-display-clusters
	      (lambda (fd)
		(let ((size (sw-st-cluster_size (sw-fstat fd))))
		  (cond ((<= size 0) #f)
			(else (let ((data (sw-read-string fd size)))
				(display (cons size data))
				(sw-display-clusters fd))))))))
      (sw-lseek fd 0 SEEK_SET)
      (sw-display-clusters fd) (newline)
      (sw-lseek fd off SEEK_SET)
      #t)))

(define sw-contents
  (lambda (fd)
    (let ((st (sw-fstat fd)))
      (sw-lseek fd 0 SEEK_SET)
      (let ((data (sw-read-string fd (sw-st-size st))))
	(sw-lseek fd (sw-st-offset st) SEEK_SET)
	data))))

(define sw-display
  (lambda (fd)
    (begin
      (display (sw-stat fd)) (newline)
      (display (sw-contents fd)) (newline))))



;
; play a soundfile - automagically finds the number of channels
;

(define play
  (lambda (fname)
    (let* ((net (net-new))
	   (rf (net-add-node net read-file (list "filename" fname)))
	   (ao (net-add-node net audio-out)))
      (node-connect-n rf "out" ao "in")
      (net-run net))))

(add-help 'play '(filename) "play a soundfile")


;
; play a file with automagically determining #channels _and_ applying
; a chain of effects (with an optional list of parameters applied to
; each)
; (play-eff "test.wav" '("echo"))
; (play-eff "test.wav" '("echo") '("iir" '("attack" 100)))
;

(define play-eff
  (lambda (fname . effects)
    (let* ((net (net-new))
	   (rf (net-add-node net read-file))
	   (ao (net-add-node net audio-out)))
      (filter-set-param! rf "filename" fname)
      (while-not-false
       (lambda ()
	 (let* ((eff (apply net-add-nodes net effects))
		(conn (catch 'glame-error
			     (lambda () (filter-connect rf "out" (car eff) "in"))
			     (lambda args #f))))
	   (if (pipe? conn)
	       (begin (nodes-connect (append eff (list ao))) #t)
	       (begin (apply nodes-delete eff) #f)))))
      (net-run net))))

(add-help 'play-eff '(filename effect ...)
	  "play a file with the effects applied. Effect may be a simple string or a list containing the string and additional parameters")

(define play-files
  (lambda (file . files)
    (let* ((net (net-new))
	   (render (net-add-node net "render"))
	   (aout (net-add-node net audio-out))
	   (left (filter-connect render "out" aout "in"))
	   (right (filter-connect render "out" aout "in")))
      (for-each
        (lambda (fname)
	  (let ((rf (net-add-node net read-file (list "filename" fname))))
	    (node-connect-n rf "out" render "in")))
	(cons file files))
      (filterpipe_set_sourceparam left "position" -1.57)
      (filterpipe_set_sourceparam right "position" 1.57)
      (net-run net))))

(add-help 'play-files '(file ...)
	  "play a set of files by mixing them together for stereo output")

(define mix-files
  (lambda (file . files)
    (let* ((net (net-new))
	   (render (net-add-node net "mix"))
	   (aout (net-add-node net audio-out))
	   (right (filter-connect render "out" aout "in")))
      (for-each
        (lambda (fname)
	  (let ((rf (net-add-node net read-file (list "filename" fname))))
	    (node-connect-n rf "out" render "in")))
	(cons file files))
      (net-run net))))

;
; load, process and save file
;

(define save-eff
  (lambda (fname oname . effects)
    (let* ((net (net-new))
	   (rf (net-add-node net read-file))
	   (wf (net-add-node net write-file)))
      (filter-set-param! rf "filename" fname)
      (filter-set-param! wf "filename" oname)
      (while-not-false
       (lambda ()
	 (let* ((eff (apply net-add-nodes net effects))
		(conn (catch 'glame-error
			     (lambda () (filter-connect rf "out" (car eff) "in"))
			     (lambda args #f))))
	   (if (eq? conn #f)
	       (begin (apply nodes-delete eff) #f)
	       (begin (nodes-connect (append eff (list wf))) #t)))))
      (net-run net (* 64 1024)))))

(add-help 'save-eff '(input-filename output-filename effect ...)
	  "apply the effects to the input and write output")


;
; read a file into n swapfiles / export n swapfiles to a file
;

(define file-to-swap
  (lambda (fname sf1 . sfiles)
    (let* ((net (net-new))
	   (rf (net-add-node net read-file (list "filename" fname))))
      (for-each
        (lambda (sf)
	  (nodes-connect (list rf (net-add-node net
						"swapfile-out"
						(list "filename" sf)
						(list "offset" 0)
						(list "flags" 3)))))
	(cons sf1 sfiles))
      (net-run net))))

(add-help 'file-to-swap '(filename swapfile1 ...)
	  "Import a file into the specified swapfiles")


(define swap-to-file
  (lambda (fname sf1 . sfiles)
    (let* ((net (net-new))
	   (rf (net-add-node net "write-file")))
      (filter-set-param! rf "filename" fname)
      (for-each
        (lambda (sf)
	  (nodes-connect (list (net-add-node net
					     "swapfile-in" 
					     (list "offset" 0)
					     (list "filename" sf))
			       rf)))
	(cons sf1 sfiles))
      (net-run net))))

(add-help 'swap-to-file '(filename swapfile1 ...)
	  "Export the specified swapfiles to a file")


;;; Macro to create a new filternetwork
;;;
;;; (create-net ((node "name" ("param" expr)) ...)
;;;             ((input-node "port" "label" "desc") ...)
;;;             ((output-node "port" "label" "desc") ...)
;;;             ((param-node "param" "label" "desc") ...)
;;;             body-expr) 
;;; --> new filternetwork
;;;
;;;Example:
;;;(define n (create-net ((sw-in "swapfile-in" ("filename" (+ 0 0)))
;;;		          (ao "audio-out")
;;;		          (ech "echo" ("time" 1200)))
;;;		         () 
;;;		         () 
;;;		         () 
;;;		         (begin 
;;;			  (nodes-connect (list sw-in ech ao)))))

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
			   `(begin (filternetwork-add-param net 
							     ,(car first)
							     ,(cadr first)
							     ,(caddr first)
							     ,(cadddr first))
				   ,(lp (cdr params))))))
			 (let ((first (car output)))
			   `(begin (filternetwork-add-output net 
							     ,(car first)
							     ,(cadr first)
							     ,(caddr first)
							     ,(cadddr first))
				   ,(lp (cdr output))))))
		   (let ((first (car input)))
		     `(begin (filternetwork-add-input net 
						      ,(car first)
						      ,(cadr first)
						      ,(caddr first)
						      ,(cadddr first))
			     ,(lp (cdr input))))))
	    `(let ((,(caar nodes) (net-add-node net ,(cadar nodes))))
	       (node-set-params ,(caar nodes) 
				,@(map (lambda (e) 
					 `(list (quote ,(car e)) ,(cadr e)))
				       (cddar nodes)))
	       ,(lp (cdr nodes)))))))

;------------------------------------------------------------
;
; Big section of GPSM stuff --- mainzelm's task
;

; Adds as much plugin nodes and connections to node as possible.
; Evaluates to a list of ( pipe . node ) pairs.
(define (node-connect-nodes net node plugin)
  (letrec ((ncn (lambda ()
		  (let ((si (net-add-node net plugin)))
		    (catch 'glame-error
			   (lambda ()
			     (cons
			       (cons (filter-connect node "out" si "in") si)
			       (ncn)))
			   (lambda args
			     (filter-delete si)
			     '()))))))
    (ncn)))

(define (gpsm-import filename)
  (let* ((net (net-new))
	 (grp (gpsm-newgrp filename))
	 (rf (net-add-node net "read-file" (list "filename" filename)))
	 (pipe-so (node-connect-nodes net rf "swapfile-out"))
	 (swfile-pipe
	   (map
	     (lambda (pso)
	       (let ((swfile (gpsm-newswfile "bla")))
		 (filter-set-param! (cdr pso) "filename" (gpsm-swfile-filename swfile))
		 (gpsm-item-place grp swfile 0 (gpsm-item-vsize grp))
		 (cons swfile (car pso))))
	     pipe-so)))
    (net-run net)
    (for-each
     (lambda (sp)
       (gpsm-swfile-set-samplerate! (car sp) (pipe-samplerate (cdr sp)))
       (gpsm-swfile-set-position! (car sp) (pipe-position (cdr sp)))
       (gpsm-invalidate-swapfile (gpsm-swfile-filename (car sp))))
     swfile-pipe)
    grp))

(define gpsm-grp-find-item
  (lambda (grp hpos vpos)
    (if (and (= (gpsm-item-hposition grp) hpos)
	     (= (gpsm-item-vposition grp) vpos))
	grp
	(if (gpsm-item-swfile? grp)
	    #f
	    (gpsm-grp-find-item-loop (gpsm-grp-items grp)
				     (+ hpos (gpsm-item-hposition grp))
				     (+ vpos (gpsm-item-vposition grp)))))))
(define gpsm-grp-find-item-loop
  (lambda (list hpos vpos)
    (if (null? list)
	#f
	(let ((item (gpsm-grp-find-item (car list))))
	  (if item
	      item
	      (gpsm-grp-find-item-loop (cdr list)))))))



;------------------------------------------------------------
;
; Testing stuff - remember, there is a ~/.glame.scm file
; automatically loaded, too - so perhaps some of this stuff
; belongs there...
; The only testing procedure that is supposed to stay here
; forever is the test-latency one as it can be easily used
; for testing correct operation of the filter subsystem.
;

(define test-latency
  (lambda (depth)
    (let* ((net (net-new))
	   (nodes (apply net-add-nodes net (cons '("ping") (repeat-n '("null") depth)))))
      (nodes-connect (append nodes (list (car nodes))))
      (net-run net))))

(add-help 'test-latency '(number)
	  "Checks latency of a chain of number null filters")


;
; other stuff
;

;; Helper function, I just love mapconcat!
(define (mapconcat proc list sep)
  (let ((newlist (map proc list)))
    (apply string-append
           (cons (car newlist)
                 (map (lambda (elt)
                        (string-append sep elt))
                      (cdr newlist))))))

(define describe-filter-params
  (lambda (filter)
    (display ";; The parameters of ")
    (display (filter-name filter))
    (display
     (string-append
      " are:\n"
      (mapconcat
       (lambda (param)
         (string-append "; " (param-label param) ": " (param->string param)))
       (filter-params filter) "\n")
      "\n"))))

(add-help 'describe-filter-params '(filter)
          "Display a simple description of filters parameters.")


