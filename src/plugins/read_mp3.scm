; mp3 reader using the pipe-in filter and mpg123
; NOTE - this is broken on ix86 somehow (libc bug?)

(let ((plugin (glame_create_plugin
		(create-net ((p "pipe-in"))
	    		()
	    		((p "out" "out" "output"))
	    		((p "tail" "filename" "filename"))
	    		(filternode_set_param p "cmd" "mpg123 -q -s "))
		"read-mp3")))
	(plugin_set plugin PLUGIN_DESCRIPTION "mp3-reader")
	(plugin_set plugin PLUGIN_CATEGORY "Input"))
