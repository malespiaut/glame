/* Wheeeeeee --- here generate references to symbols
 * whose objects we dont want to go away at (static)
 * link time...
 */

static void blafoobar()
{
	/* basic */
	drop_register();
	one2n_register();
	buffer_register();

	/* basic_sample */
mix_register();
render_register();
volume_adjust_register();
delay_register();
extend_register();
repeat_register();

	/* audio_io */
aio_generic_register_input();
aio_generic_register_output();
audio_out_register();
audio_in_register();

	/* file_io */
read_file_register();
write_file_register();
file_io_register();

	/* swapfile_io */
swapfile_in_register();
swapfile_out_register();
}

