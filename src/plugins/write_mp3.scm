(let ((newplugin (glame_plugin_define (let* ((net (filter-new))
	(pipe_out (filter-add-node net (filter-new (plugin_get "pipe_out")) "pipe_out")))
   ((lambda (pdb)
  (let* ((params (map (lambda (param)
		         (cons (param-label param) param))
		       pdb))
	  (pairs (map (lambda (spec)
		        (let ((p (assoc (car spec) params)))
			     (if p
			       (cons (cdr p) (cdr spec))
			       (throw 'glame-error))))
                     (list (append (list "cmd" "lame -r -x - ") (list (cons "_desc" "command string")))
        ))))
        (for-each
          (lambda (p)
            (param-set! (car p) (cadr p))
            (for-each
              (lambda (prop)
                (set-property! (car p) (car prop) (cdr prop)))
              (cddr p)))
          pairs)))
 (filter-params pipe_out))
   (for-each
     (lambda (p)
       (set-property! pipe_out (car p) (cdr p)))
     (list (cons "canvas_y" "-8.0") (cons "canvas_x" "-11.0")))
   (filternetwork_add_input net pipe_out "in" "in" "in")
   (filternetwork_add_param net pipe_out "tail" "filename" "(null)")
   net)
 "write_mp3")
)) (if (filter? newplugin) newplugin
      (begin
         (plugin_set newplugin PLUGIN_CATEGORY "Output")
	 (plugin_set newplugin PLUGIN_LABEL "MP3 writer (lame)"))))
