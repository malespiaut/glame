;----------------------------------------------------------------------
; The GLAME testsuite.
;
; Covers tests for the swapfile, plugin conformance and gpsm operations.
;
; Invoke parts by
; - (swtest-all) for the swapfile tests
; - (test-filter-all) for the plugin conformance tests
; - (test-gpsm-all) for the gpsm operation tests
;
; Or invoke the whole testsuite via
;   (test-glame)
;

(define (test-glame)
  (display "=== Checking swapfile subsystem ===") (newline)
  (swtest-all) (newline)
  (display "=== Checking plugin conformance ===") (newline)
  (test-filter-all) (newline)
  (display "=== Checking gpsm operations ===") (newline)
  (test-gpsm-all))



;----------------------------------------------------------------------
; Swapfile testsuite.
;

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
		    (sw-write fd "Hello World!")
		    (sw-ftruncate fd 5)
		    (sw-ftruncate fd 64)
		    (let ((res (sw-contents fd)))
		      (sw-close fd)
		      (sw-unlink 999)
		      res)))))
      (swtest "Truncate" test "Hello"))))

(define swtest-rw-simple
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd (sw-creat 999)))
		    (sw-write fd "Hello")
		    (sw-write fd "World!")
		    (sw-lseek fd 5 SEEK_SET)
		    (sw-write fd " World!")
		    (let ((res (sw-contents fd)))
		      (sw-close fd)
		      (sw-unlink 999)
		      res)))))
      (swtest "Simple read-write" test "Hello World!"))))

(define swtest-cut-aligned
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd (sw-creat 999 "Hallo " "Leute, " "wie gehts?")))
		    (sw-lseek fd 6 SEEK_SET)
		    (sw-sendfile SW_NOFILE fd 7 SWSENDFILE_CUT)
		    (let ((result (sw-contents fd)))
		      (sw-close fd)
		      (sw-unlink 999)
		      result)))))
      (swtest "Aligned cut" test "Hallo wie gehts?"))))

(define swtest-cut-unaligned
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd (sw-creat 999 "Hallo Leute, wie gehts?")))
		    (sw-lseek fd 6 SEEK_SET)
		    (sw-sendfile SW_NOFILE fd 7 SWSENDFILE_CUT)
		    (let ((result (sw-contents fd)))
		      (sw-close fd)
		      (sw-unlink 999)
		      result)))))
      (swtest "Unaligned cut" test "Hallo wie gehts?"))))

(define swtest-cut-tail
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd (sw-creat 999 "Hallo Leute, wie gehts?")))
		    (sw-lseek fd -6 SEEK_END)
		    (sw-sendfile SW_NOFILE fd 6 SWSENDFILE_CUT)
		    (let ((result (sw-contents fd)))
		      (sw-close fd)
		      (sw-unlink 999)
		      result)))))
      (swtest "Tail cut" test "Hallo Leute, wie "))))

(define swtest-cut-tail-aligned
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd (sw-creat 999 "Hallo " "Leute, wie " "gehts?")))
		    (sw-lseek fd -6 SEEK_END)
		    (sw-sendfile SW_NOFILE fd 6 SWSENDFILE_CUT)
		    (let ((result (sw-contents fd)))
		      (sw-close fd)
		      (sw-unlink 999)
		      result)))))
      (swtest "Aligned tail cut" test "Hallo Leute, wie "))))

(define swtest-cut-tail-unaligned
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd (sw-creat 999 "Hallo " "Leute," " wie gehts?")))
		    (sw-lseek fd -6 SEEK_END)
		    (sw-sendfile SW_NOFILE fd 6 SWSENDFILE_CUT)
		    (let ((result (sw-contents fd)))
		      (sw-close fd)
		      (sw-unlink 999)
		      result)))))
      (swtest "Unaligned tail cut" test "Hallo Leute, wie "))))

(define swtest-cut-head
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd (sw-creat 999 "Hallo Leute, wie gehts?")))
		    (sw-lseek fd 0 SEEK_SET)
		    (sw-sendfile SW_NOFILE fd 6 SWSENDFILE_CUT)
		    (let ((result (sw-contents fd)))
		      (sw-close fd)
		      (sw-unlink 999)
		      result)))))
      (swtest "Head cut" test "Leute, wie gehts?"))))

(define swtest-cut-head-aligned
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd (sw-creat 999 "Hallo " "Leute, wie" " gehts?")))
		    (sw-lseek fd 0 SEEK_SET)
		    (sw-sendfile SW_NOFILE fd 6 SWSENDFILE_CUT)
		    (let ((result (sw-contents fd)))
		      (sw-close fd)
		      (sw-unlink 999)
		      result)))))
      (swtest "Aligned head cut" test "Leute, wie gehts?"))))

(define swtest-cut-head-unaligned
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd (sw-creat 999 "Hallo Leute," " wie" " gehts?")))
		    (sw-lseek fd 0 SEEK_SET)
		    (sw-sendfile SW_NOFILE fd 6 SWSENDFILE_CUT)
		    (let ((result (sw-contents fd)))
		      (sw-close fd)
		      (sw-unlink 999)
		      result)))))
      (swtest "Unaligned head cut" test "Leute, wie gehts?"))))

(define swtest-insert-aligned
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd1 (sw-creat 998 "Hallo " "wie gehts?"))
			(fd2 (sw-creat 999 "Leute, ")))
		    (sw-lseek fd1 6 SEEK_SET)
		    (sw-lseek fd2 0 SEEK_SET)
		    (sw-sendfile fd1 fd2 7 SWSENDFILE_INSERT)
		    (let ((result (sw-contents fd1)))
		      (sw-close fd1)
		      (sw-close fd2)
		      (sw-unlink 998)
		      (sw-unlink 999)
		      result)))))
      (swtest "Aligned insert" test "Hallo Leute, wie gehts?"))))

(define swtest-insert-unaligned
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd1 (sw-creat 998 "Hallo wie gehts?"))
			(fd2 (sw-creat 999 "Bla Leute, Blubb")))
		    (sw-lseek fd1 6 SEEK_SET)
		    (sw-lseek fd2 4 SEEK_SET)
		    (sw-sendfile fd1 fd2 7 SWSENDFILE_INSERT)
		    (let ((result (sw-contents fd1)))
		      (sw-close fd1)
		      (sw-close fd2)
		      (sw-unlink 998)
		      (sw-unlink 999)
		      result)))))
      (swtest "Unaligned insert" test "Hallo Leute, wie gehts?"))))

(define swtest-filecow
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd1 (sw-creat 998 "Hallo wie gehts?"))
			(fd2 (sw-creat 999)))
		    (sw-lseek fd1 0 SEEK_SET)
		    (sw-ftruncate fd2 16)
		    (sw-lseek fd2 0 SEEK_SET)
		    (sw-sendfile fd2 fd1 16 0)
		    (sw-lseek fd2 4 SEEK_SET)
		    (sw-write fd2 "XXXXX")
		    (let ((result (string-append (sw-contents fd1) (sw-contents fd2))))
		      (sw-close fd1)
		      (sw-close fd2)
		      (sw-unlink 998)
		      (sw-unlink 999)
		      result)))))
      (swtest "File COW" test "Hallo wie gehts?HallXXXXX gehts?"))))

(define swtest-overwrite-aligned
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd1 (sw-creat 998 "Hallo Leute,"))
			(fd2 (sw-creat 999 " wie gehts?")))
		    (sw-ftruncate fd1 23)
		    (sw-lseek fd1 12 SEEK_SET)
		    (sw-lseek fd2 0 SEEK_SET)
		    (sw-sendfile fd1 fd2 11 0)
		    (let ((result (sw-contents fd1)))
		      (sw-close fd1)
		      (sw-close fd2)
		      (sw-unlink 998)
		      (sw-unlink 999)
		      result)))))
      (swtest "Aligned overwrite" test "Hallo Leute, wie gehts?"))))

(define swtest-overwrite-unaligned
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd1 (sw-creat 998 "Hallo Leute, wie"))
			(fd2 (sw-creat 999 " wie gehts?")))
		    (sw-ftruncate fd1 23)
		    (sw-lseek fd1 12 SEEK_SET)
		    (sw-lseek fd2 0 SEEK_SET)
		    (sw-sendfile fd1 fd2 11 0)
		    (let ((result (sw-contents fd1)))
		      (sw-close fd1)
		      (sw-close fd2)
		      (sw-unlink 998)
		      (sw-unlink 999)
		      result)))))
      (swtest "Unaligned overwrite" test "Hallo Leute, wie gehts?"))))

(define swtest-overwrite-unaligned-append
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd1 (sw-creat 998 "Hallo Leute, wie"))
			(fd2 (sw-creat 999 " wie gehts?")))
		    (sw-ftruncate fd1 20)
		    (sw-lseek fd1 12 SEEK_SET)
		    (sw-lseek fd2 0 SEEK_SET)
		    (sw-sendfile fd1 fd2 11 0)
		    (let ((result (sw-contents fd1)))
		      (sw-close fd1)
		      (sw-close fd2)
		      (sw-unlink 998)
		      (sw-unlink 999)
		      result)))))
      (swtest "Unaligned overwrite with append" test "Hallo Leute, wie gehts?"))))

(define swtest-shared-shrink
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd1 (sw-creat 998 "Hallo Leute, wie gehts?"))
			(fd2 (sw-creat 999)))
		    (sw-sendfile fd2 fd1 23 0)
		    (sw-ftruncate fd2 22)
		    (let ((result (sw-contents fd2)))
		      (sw-close fd1)
		      (sw-close fd2)
		      (sw-unlink 998)
		      (sw-unlink 999)
		      result)))))
      (swtest "Shared file shrink" test "Hallo Leute, wie gehts"))))

(define swtest-shared-grow
  (lambda ()
    (let ((test (lambda ()
		  (let ((fd1 (sw-creat 998 "Hallo Leute, wie gehts?"))
			(fd2 (sw-creat 999)))
		    (sw-sendfile fd2 fd1 23 0)
		    (sw-ftruncate fd2 24)
		    (let ((result (sw-contents fd2)))
		      (sw-close fd1)
		      (sw-close fd2)
		      (sw-unlink 998)
		      (sw-unlink 999)
		      result)))))
      (swtest "Shared file grow" test "Hallo Leute, wie gehts?"))))

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
	 (swtest-filecow)
	 (swtest-shared-shrink)
	 (swtest-shared-grow)
	 (swtest-overwrite-aligned)
	 (swtest-overwrite-unaligned)
	 (swtest-overwrite-unaligned-append))))





;----------------------------------------------------------------------
; Plugin conformance testsuite.
;

(define test-filter-rate
  (lambda (in out msg)
    (if (not (= (pipe-samplerate in) (pipe-samplerate out)))
	(throw (string->symbol (string-append msg ": samplerate error"))))))
(define test-filter-position
  (lambda (in out msg)
    (if (not (= (pipe-position in) (pipe-position out)))
	(throw (string->symbol (string-append msg ": position error"))))))

(define test-filter
  (lambda (filter . what)
    (display "testing ") (display filter)
    (catch #t
	   (lambda ()
	     (let* ((net (net-new))
		    (in (net-add-node net "const"))
		    (f (net-add-node net filter))
		    (out (net-add-node net "drop"))
		    (inp (filter-connect in "out" f "in"))
		    (outp (filter-connect f "out" out "in")))
	       (for-each (lambda (m) (m inp outp "initial")) what)
	       (filter-set-param! in "rate" 10)
	       (for-each (lambda (m) (m inp outp "rate change")) what)
	       (filter-set-param! in "position" 1.0)
	       (for-each (lambda (m) (m inp outp "position change")) what)
	       (pipe-delete outp)
	       (set! outp (filter-connect f "out" out "in"))
	       (for-each (lambda (m) (m inp outp "reconnect")) what)
	       (filter-delete net)
	       (display " - passed.") (newline)))
	   (lambda args
	     (display " - failed (") (display (symbol->string (car args))) (display ").") (newline)))))

(define test-filter-all
  (lambda ()
    (test-filter "null" test-filter-rate test-filter-position)
    (test-filter "one2n" test-filter-rate test-filter-position)
    (test-filter "buffer" test-filter-rate test-filter-position)
    (test-filter "mix" test-filter-rate)
    (test-filter "render" test-filter-rate)
    (test-filter "volume-adjust" test-filter-rate test-filter-position)
    (test-filter "delay" test-filter-rate test-filter-position)
    (test-filter "extend" test-filter-rate test-filter-position)
    (test-filter "repeat" test-filter-rate test-filter-position)
    (test-filter "mul" test-filter-rate test-filter-position)
    (test-filter "add" test-filter-rate test-filter-position)
    (test-filter "invert" test-filter-rate test-filter-position)
    (test-filter "echo" test-filter-rate test-filter-position)
    (test-filter "noisegate" test-filter-rate test-filter-position)
    (test-filter "highpass" test-filter-rate test-filter-position)
    (test-filter "lowpass" test-filter-rate test-filter-position)
    (test-filter "bandpass" test-filter-rate test-filter-position)
    (test-filter "bandpass_a" test-filter-rate test-filter-position)
    (test-filter "flanger" test-filter-rate test-filter-position)
    (test-filter "distortion" test-filter-rate test-filter-position)
    (test-filter "echo2" test-filter-rate)
    ))





;----------------------------------------------------------------------
; GPSM testsuite.
;

; gpsm testsuite helpers

; init a gpsm tree like (two horiz. sequenced 10-sample tracks)
;  (test-gpsm-spec-to-tree '((0 0 10) (10 0 10)))
; or (two horiz. stereo (vert. sequenced) 10-sample tracks)
;  (test-gpsm-spec-to-tree '((0 0 ((0 0 10) (0 1 10))) (10 0 ((0 0 10) (0 1 10))))
(define test-gpsm-init-tree-do-grp
  (lambda (item-spec)
    (let ((grp (gpsm-newgrp "foo")))
      (test-gpsm-init-tree-do-list grp item-spec))))
(define test-gpsm-init-tree-do-file
  (lambda (item-spec)
    (let* ((swfile (gpsm-newswfile "foo"))
	   (fd (sw-open (gpsm-swfile-filename swfile) (+ O_RDWR))))
      (sw-ftruncate fd (* SAMPLE_SIZE item-spec))
      (sw-close fd)
      (gpsm-invalidate-swapfile (gpsm-swfile-filename swfile))
      swfile)))
(define test-gpsm-init-tree-do-list
  (lambda (grp item-list)
    (let* ((hpos (caar item-list))
	   (vpos (cadar item-list))
	   (item-spec (caddar item-list))
	   (item (if (list? item-spec)
		     (test-gpsm-init-tree-do-grp item-spec)
		     (test-gpsm-init-tree-do-file item-spec))))
      (gpsm-item-place grp item hpos vpos)
      (if (null? (cdr item-list))
	  grp
	  (test-gpsm-init-tree-do-list grp (cdr item-list))))))
(define test-gpsm-spec-to-tree
  (lambda (item-list)
    (test-gpsm-init-tree-do-grp item-list)))
(define test-gpsm-tree-to-spec
  (lambda (grp)
    (map (lambda (item)
	   (list (gpsm-item-hposition item)
		 (gpsm-item-vposition item) 
		 (if (gpsm-grp? item)
		     (test-gpsm-tree-to-spec item)
		     (gpsm-item-hsize item))))
	 (reverse (gpsm-grp-items grp)))))


; the gpsm tests

(define test-gpsm-hbox-insert
  (lambda (hbox hpos vpos size)
    (let ((file (test-gpsm-init-tree-do-file size)))
      (gpsm-hbox-insert hbox file hpos vpos)
      hbox)))
	   

; main gpsm testing routines

(define test-gpsm
  (lambda (test-name test-init test-expect test-proc . test-params)
    (display test-name)
    (let ((init (test-gpsm-spec-to-tree test-init)))
      (if (equal? (test-gpsm-tree-to-spec
		   (apply test-proc (cons init test-params)))
		  test-expect)
	  (display " - passed.")
	  (display " - FAILED!"))
      (newline)
      (gpsm-item-destroy init))))

(define test-gpsm-all
  (lambda ()
    (let ((t (test-gpsm-spec-to-tree '((0 0 10) (0 1 ((0 0 20)))))))
      (if (not (equal? (test-gpsm-tree-to-spec t) '((0 0 10) (0 1 ((0 0 20))))))
	  (begin
	    (display "Tree producer / analyzer mismatch!") (newline)
	    (gpsm-item-destroy t)
	    (throw 'glame-error))
	  (gpsm-item-destroy t)))
    (test-gpsm "hbox insert 1"
	       '((0 0 10) (10 0 10))
	       '((0 0 10) (10 0 5) (15 0 10))
	       test-gpsm-hbox-insert 10 0 5)
    (test-gpsm "hbox insert 2"
	       '((0 0 ((0 0 10) (0 1 10))) (10 0 ((0 0 10) (0 1 10))))
	       '((0 0 ((0 0 10) (0 1 10))) (10 1 5) (15 0 ((0 0 10) (0 1 10))))
	       test-gpsm-hbox-insert 10 1 5)
))


