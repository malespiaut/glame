#ifndef _MIDI_H
#define _MIDI_H

/*
 * midi.h
 *
 * Copyright (C) 2000 Jim Garrison
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
 */

#define	MIDI_BYTE_STATUS(ev)		(ev.b[0])
#define	MIDI_BYTE_DATA1(ev)		(ev.b[1])
#define	MIDI_BYTE_DATA2(ev)		(ev.b[2])
#define	MIDI_BYTE_FLAGS(ev)		(ev.flags)

#define	MIDI_EVENT_IS_SYSTEM(ev)	((MIDI_BYTE_STATUS(ev) & 0xF0) == 0xF0)
#define	MIDI_EVENT_IS_CHANNEL(ev)	((MIDI_BYTE_STATUS(ev) & 0xF0) != 0xF0)

#define MIDI_EVENT_COMMAND(ev)		(MIDI_BYTE_STATUS(ev) & 0xF0)
#define MIDI_EVENT_CHANNEL(ev)		(MIDI_BYTE_STATUS(ev) & 0x0F)

#define MIDI_EVENT_FLAG_NOQUEUE		1

#endif
