#include <guile/gh.h>
#include <glame_hash.h>
#include <swapfile.h>
#include <glplugin.h>

extern int glscript_init();

/* builtin plugins */
PLUGIN_SET(glamebuiltins, "basic basic_sample track_io audio_io file_io waveform basic_midi midi_io midi_debug rms arithmetic")

static int plugins_register()
{
	/* add default plugin path - and "debug path" (first -> last) */
	plugin_add_path(PKGLIBDIR);
	plugin_add_path("./plugins/.libs");

	/* first the builtin ones */
	plugin_get("glamebuiltins");

	/* then all the plugins in the (default) plugin path */
	/* FIXME - by hand for now. */
	plugin_get("echo");
	plugin_get("pan");
        plugin_get("debug");
        plugin_get("tutorial");
        plugin_get("garrison");
        plugin_get("nold");
        plugin_get("maggy");
        plugin_get("noisegate");
	plugin_get("pipe");
	plugin_get("iir");

	return 0;
}

static void glame_cleanup()
{
	swap_close();
}

int glame_init()
{
	if (hash_alloc() == -1)
		return -1;
	if (plugins_register() == -1)
		return -1;
	atexit(glame_cleanup);
	return 0;
}

static void init_after_guile(int argc, char **argv)
{
	if (glscript_init() == -1)
		exit(1);
	((void (*)(void))argv[1])();
}

int glame_init_with_guile(void (*main)(void))
{
#ifdef HAVE_GUILE
	char *argv[2];
#endif

	if (glame_init() == -1)
		return -1;
#ifdef HAVE_GUILE
	argv[0] = NULL;
	argv[1] = (void *)main;
	gh_enter(0, argv, init_after_guile);
#endif

	/* not reached if HAVE_GUILE */
	main();
	return 0;
}
