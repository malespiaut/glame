(let ((plugin (glame_plugin_define
	(create-net ((fft "fft")
		(res "fft_resample")
		(ifft "ifft"))
	((fft "in" "in" "resample in"))
	((ifft "out" "out" "resample out"))
	((res "frequency" "frequency" "new sample frequency"))
	(begin (filternode_set_param net "frequency" 44100)
		(filternode_set_param fft "blocksize" 2048); frequency accuracy = 43 Hz
		(nodes-connect (list fft res ifft))))
	"Resample")))
	(if (filter? plugin)
	   plugin
	   (begin
		(plugin_set plugin PLUGIN_DESCRIPTION "FFT Resample macro filter")
		(plugin_set plugin PLUGIN_CATEGORY "Effects"))))
