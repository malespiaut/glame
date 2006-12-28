/*
 * glconfig.c
 *
 * $Id: glconfig.c,v 1.5 2006/12/28 21:44:12 richi Exp $
 *
 * Copyright (C) 2001, 2002, 2004 Richard Guenther
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "glconfig.h"



void glame_config_load()
{
	glame_gh_safe_eval_str(
"(map (lambda (file)"
"        (if (file-exists? file)"
"           (begin"
#ifdef DEBUG
"              (display (string-append \"loading \" file)) (newline)"
#endif
"              (load file))))"
"  `("
"    ,(string-append (getenv \"HOME\") \"/.glamerc\")))");
}

void glame_config_sync()
{
	glame_gh_safe_eval_str(
"(with-output-to-file (string-append (getenv \"HOME\") \"/.glamerc\")"
"                     (lambda () (display \"(define glame-config\n'\")"
"                                (display (map (lambda (p) (if (string? (cdr p)) (cons (car p) (string-append \"\\\"\" (cdr p) \"\\\"\")) p)) glame-config))"
"                                (display \")\n\")))");
}


SCM glame_config_get(const char *key, SCM s_default)
{
	SCM s_gcg;
	SCM s_key;

	/* FIXME: catch. */
	s_gcg = gh_lookup("glame-config-get");
	s_key = scm_makfrom0str(key);
	if (SCM_UNBNDP(s_default))
		return gh_call1(s_gcg, s_key);
	return gh_call2(s_gcg, s_key, s_default);
}

void glame_config_set(const char *key, SCM s_value)
{
	SCM s_gcs;
	SCM s_key;

	/* FIXME: catch. */
	s_gcs = gh_lookup("glame-config-set!");
	s_key = scm_makfrom0str(key);
	gh_call2(s_gcs, s_key, s_value);
}




char *glame_config_get_string_with_default(const char *key, const char *def)
{
	char cmd[256];
	int len;
	snprintf(cmd, 255, "(glame-config-get '%s \"%s\")", key, def);
	/* FIXME: execute with catch. */
	return glame_scm2newstr(gh_eval_str(cmd), &len);	
}

int glame_config_get_string(const char *key, char **value)
{
	char cmd[256];
	int len;
	SCM s_res;
	snprintf(cmd, 255, "(glame-config-get '%s)", key);
	/* FIXME: execute with catch. */
	s_res = glame_gh_safe_eval_str(cmd);
	if (!gh_string_p(s_res))
		return -1;
	*value = glame_scm2newstr(s_res, &len);
	return 0;
}

void glame_config_set_string(const char *key, const char *value)
{
	char cmd[256];
	snprintf(cmd, 255, "(glame-config-set! '%s \"%s\")", key, value);
	glame_gh_safe_eval_str(cmd);
}

long glame_config_get_long_with_default(const char *key, long def)
{
	char cmd[256];
	snprintf(cmd, 255, "(glame-config-get '%s %li)", key, def);
	/* FIXME: execute with catch. */
	return glame_scm2long(gh_eval_str(cmd));	
}

int glame_config_get_long(const char *key, long *value)
{
	char cmd[256];
	SCM s_res;
	snprintf(cmd, 255, "(glame-config-get '%s)", key);
	/* FIXME: execute with catch. */
	s_res = glame_gh_safe_eval_str(cmd);
	if (!gh_number_p(s_res)
	    || !gh_exact_p(s_res))
		return -1;
	*value = glame_scm2long(s_res);
	return 0;
}

void glame_config_set_long(const char *key, long value)
{
	char cmd[256];
	snprintf(cmd, 255, "(glame-config-set! '%s %li)", key, value);
	glame_gh_safe_eval_str(cmd);
}

double glame_config_get_double_with_default(const char *key, double def)
{
	char cmd[256];
	snprintf(cmd, 255, "(glame-config-get '%s %.9f)", key, def);
	/* FIXME: execute with catch. */
	return gh_scm2double(gh_eval_str(cmd));
}

int glame_config_get_double(const char *key, double *value)
{
	char cmd[256];
	SCM s_res;
	snprintf(cmd, 255, "(glame-config-get '%s)", key);
	/* FIXME: execute with catch. */
	s_res = glame_gh_safe_eval_str(cmd);
	if (!gh_number_p(s_res))
		return -1;
	*value = gh_scm2double(s_res);
	return 0;
}

void glame_config_set_double(const char *key, double value)
{
	char cmd[256];
	snprintf(cmd, 255, "(glame-config-set! '%s %.12f)", key, value);
	glame_gh_safe_eval_str(cmd);
}

