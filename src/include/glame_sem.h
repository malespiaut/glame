#ifndef _GLAME_SEM_H
#define _GLAME_SEM_H

/*
 * glame_sem.h
 *
 * Copyright (C) 2000 Daniel Kobras, Richard Guenther
 *
 * $Id: glame_sem.h,v 1.1 2000/06/25 14:53:00 richi Exp $
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <errno.h>

#if !defined(HAVE_SEMUN)
/* according to X/OPEN we have to define it ourselves */
union semun {
	int val;                    /* value for SETVAL */
	struct semid_ds *buf;       /* buffer for IPC_STAT, IPC_SET */
	unsigned short int *array;  /* array for GETALL, SETALL */
	struct seminfo *__buf;      /* buffer for IPC_INFO */
};
#endif


#ifdef SEMCTL_OK
#define glame_semctl semctl
#else
static inline int glame_semctl(int semid, int semnum, int cmd,
			       union semun arg)
{
        /* Some braindead systems (can you say IRIX?) define semctl to
	 * take a vararg as union semun arg. Unfortunately this breaks
	 * silently with gcc. So we install a workaround for this.
	 * We support SETVAL for now. All cmds ignoring the arg
	 * should be fine anyway (default). */
	int val;
	struct sembuf sop;

	switch (cmd) {
	case SETVAL:
		/* translate to semop */
		val = semctl(semid, semnum, GETVAL, arg);
		sop.sem_num = semnum;
		sop.sem_op = arg.val - val;
		sop.sem_flg = IPC_NOWAIT;
		return semop(semid, &sop, 1);
		break;
	case IPC_STAT:
	case IPC_SET:
	case GETALL:
		/* unfixable */
		break;
	case SETALL:
		/* FIXME */
		break;
	case IPC_RMID:
	case GETNCNT:
	case GETPID:
	case GETVAL:
	case GETZCNT:
		return semctl(semid, semnum, cmd, arg);
	}
}
#endif


/* We provide auto-retry on EINTR here. */
static inline int glame_semop(int semid, struct sembuf *sops,
			      unsigned int nsops)
{
	int res;
	while ((res = semop(semid, sops, nsops)) == -1) {
		if(errno != EINTR)
			break;
	}
	return res;
}


/* Other sem calls just name-wrapped. */
#define glame_semget semget


#endif
