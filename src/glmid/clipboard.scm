
(define (empty-clipboard) 
  #f)

(define (clipboard-empty! clipboard)
  (gpsm-item-destroy clipboard))


(define (clipboard-empty? clipboard)
  (not clipboard))

(define (clipboard-get clipboard)
  (if (clipboard-empty? clipboard)
      #f
      (let ((grp (gpsm-newgrp "clipboard")))
	(for-each (lambda (item)
		    (let ((sw-file (gpsm-swfile-cow item)))
		      (gpsm-item-place grp sw-file 0 (gpsm-item-vsize grp))))
		  (gpsm-grp-items clipboard))
	grp)))

(define (clipboard-can-paste? clipboard item)
  (and (not (clipboard-empty? clipboard-empty?))
       (not item) ;;; ???
       (if (gpsm-swfile? item)
	   (= (gpsm-grp-nr-items clipboard) 0)
	   (and (satisfies-all? gpsm-swfile? (gpsm-grp-items grp))
		(= (gpsm-grp-nr-items clipboard)
		   (gpsm-grp-nr-items item))))))


(define (copy-one dest source pos size extra-flags)
  (if dest
      (copy-one-with-dest dest source pos size extra-flags)
      (copy-one-nofile source pos size extra-flags)))

(define (copy-one-with-dest dest source pos size extra-flags)
  (let ((sdf (sw-open (gpsm-swfile-filename source) O_RDONLY))
	(dfd (sw-open (gpsm-swfile-filename dest) O_WRONLY))
	(start (max 0 pos))
	(length (min (- (gpsm-item-hsize source) start)
		     (+ size (min 0 pos)))))
    (catch 'glame-error
	   (lambda ()
	     (sw-lseek sdf (size->sample-size start) SEEK_SET)
	     (sw_lseek dfd (size->sample-size (- (min 0 pos))) SEEK_SET)
	     (sw-sendfile dfd sfd (size->sample-size length) 
			  (logior SWSENDFILE_INSERT extra-flags))
	     (gpsm-invalidate-swapfile (gpsm-swfile-filename dest))
	     (if (= extra-flags SWSENDFILE_CUT)
		 (gpsm-notify-swapfile-cut (gpsm-swfile-filename source) start length))
	     (sw-ftruncate dfd (size->sample-size size))
	     (sw-close dfd)
	     (sw-close sdf))
	   (lambda error-string
	     (sw-ftruncate dfd 0)
	     (sw-close dfd)
	     (sw-close sfd)
	     (apply throw 'glame-error error-string)))))
    
(define (copy-one-nofile dest source pos size extra-flags)
  (let ((sdf (sw-open (gpsm-swfile-filename source) O_RDONLY))
	(start (max 0 pos))
	(length (min (- (gpsm-item-hsize source) start)
		     (+ size (min 0 pos)))))
    (catch 'glame-error
	   (lambda ()
	     (sw-lseek sdf (size->sample-size start) SEEK_SET)
	     (sw-sendfile SW_NOFILE sfd (size->sample-size length) 
			  (logior SWSENDFILE_INSERT extra-flags))
	     (if (= extra-flags SWSENDFILE_CUT)
		 (gpsm-notify-swapfile-cut (gpsm-swfile-filename source) start length))
	     (sw-close sdf))
	   (lambda error-string
	     (sw-close sfd)
	     (apply throw 'glame-error error-string)))))

(define (paste-one dest source pos)
  (let ((sfd (sw-open (gpsm-swfile-filename source) O_RDONLY))
	(dfs (sw-open (gpsm-swfile-filename dest O_WRONLY))))
    (catch 
     (lambda ()
       (if (< pos o)
	   (let ((zdf (open-new-file O_RDWR))
		 (size (size->sample-size (- pos))))
	     (catch 
	      (lambda ()
		(sw-ftruncate zdf size)
		(sw-sendfile dfd zdf size SWSENDFILE_INSERT))
	      (lambda errno-string
		(sw-close zdf)
		(apply throw 'glame-error errno-string)))))
       (sw-lseek dfd (size->sample-size (abs pos)) SEEK_SET)
       (sw-sendfile dfd sfd (size->sample-size (gpsm-item-hsize source)) SWSENDFILE_INSERT)
       (gpsm-notify-swapfile-insert (gpsm-swfile-filename dest) 
				    (min (gpsm-item-hsize dest)
					 (max 0 pos))
				    (+ (- (min 0 pos))
				       (gpsm-item-hsize source)
				       (max 0 (- pos (gpsm-item-hsize dest)))))
       (sw-close dfd)
       (sw-close sfd))
     (lambda errno-string
       (sw-close dfd)
       (sw-close sfd)
       (apply throw 'glame-error errno-string)))))
    
				
	     

(define (size->sample-size size)
  (* size SAMPLE_SIZE))

(define (open-new-file flags)
  (let lp ((name "opennewfilename"))
    (catch
     (lambda ()
       (let ((fd (sw-open name (logior flags O_CREAT O_EXCL))))
	 (sw-unlink name)
	 fd))
     (lambda err
       (lp (string-append name (number->string (random 42))))))))

(define (clipboard-delete item pos size)
  (check-clipboard-args item pos size "clipboard-delete")
  (if (gpsm-swfile? item)
      (copy-one #f item pos size SWSENDFILE_CUT))
  (copy-item-grp-to-swfile item swfile SWSENDFILE_CUT))

(define (clipboard-cut item pos size)
  (clipboard-cut-copy item pos size 'cut))

(define (clipboard-copy item pos size)
  (clipboard-cut-copy item pos size 'copy))

(define (clipboard-cut-copy item pos size mode)
  (let ((grp (gpsm-newgrp "clipboard")))
    (catch 'glame-error
	   (lambda ()
	     (let ((mk-swfile  (lambda (item)
				 (let ((swfile (gpsm-newswfile "track")))
				   (gpsm-swfile-set-samplerate! swfile 
								(gpsm-swfile-samplerate item))
				   (gpsm-swfile-set-position! swfile
							      (gpsm-swfile-position item))
				   (gpsm-item-place grp swfile 0 (gpsm-item-vsize grp))
				   swfile)))
		   (sendfile-mode (if (eq? mode 'cut) SWSENDFILE_CUT 0)))
	       (if (gpsm-swfile? item)
		   (copy-one (mk-swfile item) item pos size sendfile-mode))
	       (copy-item-grp-to-swfile item mk-swfile sendfile-mode)
	       grp))
	   (lambda errs
	     (gpsm-item-destroy grp)
	     (apply throw 'glame-error errs)))))

(define (copy-item-grp-to-swfile item mk-swfile sendfile-mode)
  (for-each
   (lambda (it)
     (if (not (gpsm-swfile? it))
	 (throw 'glame-error "not a swfile in item"))
     (copy-one (mk-swfile it) it (- pos (gpsm-item-hposition it)) size sendfile-mode))
   (gpsm-grp-items item)))

(define (check-clipboard-args item pos size who)
  (if (or (< pos 0)
	  (< size 0))
      (throw 'glame-error "invalid arguments to" who))
  (if (and (gpsm-grp? item)
	   (= (gpsm-grp-nr-items item) 0))
      (throw 'glame-error "invalid item in" who)))

(define (clipboard-paste clipboard item pos)
  (if (not (and item (>= pos 0) (clipboard-can-paste? clipboard item)))
      (throw 'glame-error "invalid args to clipboard-paste"))
  (if (gpsm-swfile? item)
      (paste-one item source pos)
      ;;; richi: darf man das so machen?
      (let lp ((clipboard clipboard)
	       (item item))
	(if (not (or (gpsm-grp-empty? clipboard)
		     (gpsm-grp-empty? item)))
	    (let ((source (gpsm-grp-first clipboard))
		  (dest (gpsm-grp-first item)))
	      (paste-one dest source (- pos (gpsm-item-hposition dest)))
	      (lp (gpsm-grp-next clipboard)
		  (gpsm-grp-next item)))))))

;;; belongs to somewhere else
(define (gpsm-grp-nr-items grp)
  (length (gpsm-grp-items grp)))

(define (gpsm-grp-first grp)
  (car (gpsm-grp-items grp)))

(define (gpsm-grp-next grp)
  (cdr (gpsm-grp-items grp)))

(define (gpsm-grp-empty? grp)
  (null? (gpsm-grp-items grp)))

