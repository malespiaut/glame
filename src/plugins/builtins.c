#include <glplugin.h>

/* hand-made, stupid registrations.
 * at least until those plugin sets issues are resolved. */

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

static int nonplugins_register()
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

int builtins_register()
{
	if (nonplugins_register() == -1)
		return -1;

	/* "register" the plugins from the core */
	plugin_get("fooobar");

	return 0;
}
