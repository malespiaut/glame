(let ((plugin (glame_plugin_define
	(create-net ((one2n "one2n")
		(fft "fft")
		(fft_bandpass "fft_bandpass")
		(ifft "ifft")
		(mix "mix"))
	       ((one2n "in" "in" "bassboost in"))
	       ((mix "out" "out" "bassboost out"))
	       ((fft_bandpass "band maximum" "band maximum" "boost frequency limit")
	        (fft_bandpass "gain" "gain" "gain of passband in dB"))
	       (begin (filternode_set_param net "band maximum" 1000)
		      (filternode_set_param net "gain" 3); amplification by 3dB
		      (filternode_set_param fft "blocksize" 2048)
		      (nodes-connect (list one2n fft fft_bandpass ifft mix))
		      (nodes-connect (list one2n mix))))
	"Bass Boost")))
	(if (filter? plugin)
	   plugin
	   (begin
		(plugin-set! plugin PLUGIN_DESCRIPTION "Bass Boost macro filter")
		(plugin-set! plugin PLUGIN_CATEGORY "Effects"))))
