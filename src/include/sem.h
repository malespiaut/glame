#ifndef _SEM_H
#define _SEM_H

/*
 * sem.h
 *
 * Copyright (C) 2000 Daniel Kobras
 *
 * $Id: sem.h,v 1.2 2000/05/04 14:40:46 nold Exp $
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

/*
 * Parents beware! Trying to use semctl portably will cause nightmares,
 * cancer and bad breath. This is the best solution we've found so far.
 */ 

static inline int semctl_compat_zero(int semid, int semnum, int cmd)
{
	union semun tmp;
	
	tmp.val = 0;
	
	return semctl(semid, semnum, cmd, tmp);
}

static inline int sem_op(int semid, int semnum, int val)
{
	int res;
	struct sembuf sop;

	sop.sem_num = semnum;
	sop.sem_op = val;
	sop.sem_flg = (val > 0) ? IPC_NOWAIT : 0;
	while(((res=semop(semid, &sop, 1)) == -1) && (val < 0))
		if(errno != EINTR)
			break;
	return res;
}

static inline int sem_remove(int semid)
{
	return semctl_compat_zero(semid, 0, IPC_RMID);
}

static inline int sem_zero(int semid, int semnum)
{
	return semctl_compat_zero(semid, semnum, SETVAL);
}

static inline int sem_get(int semid, int semnum)
{
	return semctl_compat_zero(semid, semnum, GETVAL);
}

#endif
