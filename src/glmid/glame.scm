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
; play a soundfile
;

(define while1
  (lambda (f)
    (if (boolean? (f)) #t (while1 f))))

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
; The "real" scheme glame midlayer
;

