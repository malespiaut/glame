/* builtins.c
 * This file is automagically generated. Don't edit. */

extern int basic_register();
extern int basic_sample_register();
extern int tutorial_register();
extern int track_io_register();
extern int audio_io_register();
extern int file_io_register();
extern int debug_register();
extern int waveform_register();
extern int echo_register();
extern int garrison_register();
extern int nold_register();
extern int basic_midi_register();
extern int midi_io_register();
extern int midi_debug_register();
extern int maggy_register();

int builtins_register()
{
	if (basic_register() == -1) return -1;
	if (basic_sample_register() == -1) return -1;
	if (tutorial_register() == -1) return -1;
	if (track_io_register() == -1) return -1;
	if (audio_io_register() == -1) return -1;
	if (file_io_register() == -1) return -1;
	if (debug_register() == -1) return -1;
	if (waveform_register() == -1) return -1;
	if (garrison_register() == -1) return -1;
	if (nold_register() == -1) return -1;
	if (basic_midi_register() == -1) return -1;
	if (midi_io_register() == -1) return -1;
	if (midi_debug_register() == -1) return -1;
	if (maggy_register() == -1) return -1;
	return 0;
}
