;
; feedback echo2 macro filter
;
(let ((plugin (glame_plugin_define
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
	(if (filter_p plugin)
	   plugin
	   (begin
	      (plugin_set plugin PlUGIN_PIXMAP "echo.png")
              (plugin_set plugin PLUGIN_DESCRIPTION "echo as macro filter")
	      (plugin_set plugin PLUGIN_CATEGORY "Effects"))))
