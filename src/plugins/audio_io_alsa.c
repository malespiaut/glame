/*
 * audio_io_alsa.c
 * $Id: audio_io_alsa.c,v 1.7 2001/11/11 14:34:20 richi Exp $
 *
 * Copyright (C) 2001 Richard Guenther, Alexander Ehlert, Daniel Kobras
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

#ifdef ALSA_H_IN_SYS
#include <sys/asoundlib.h>
#else
#include <alsa/asoundlib.h>
#endif

/* Dispatch to plugin version suitable to ALSA API of the week. This hack
 * sucks but even more so does ALSA. [dk]
 */
#if (SND_LIB_MAJOR == 0) && (SND_LIB_MINOR < 9)
#include "audio_io_alsa_v050.c"
#else
#include "audio_io_alsa_v090.c"
#endif

