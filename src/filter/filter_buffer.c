/*
 * filter_buffer.c
 * $Id: filter_buffer.c,v 1.3 2000/02/01 13:40:22 richi Exp $
 *
 * Copyright (C) 1999, 2000 Richard Guenther
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

#include <sys/types.h>
#include <sys/wait.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <pthread.h>
#include "filter.h"
#include "util.h"


#if defined(__GNU_LIBRARY__) && !defined(_SEM_SEMUN_UNDEFINED)
/* union semun is defined by including <sys/sem.h> */
#else
#if !defined(_NO_XOPEN4)
/* according to X/OPEN we have to define it ourselves */
union semun {
	int val;                    /* value for SETVAL */
	struct semid_ds *buf;       /* buffer for IPC_STAT, IPC_SET */
	unsigned short int *array;  /* array for GETALL, SETALL */
	struct seminfo *__buf;      /* buffer for IPC_INFO */
};
#endif
#endif


/* first semaphore (0) is used as reference counter for the whole
 * set - if it is zero, the set is unused */
#define FBSEM_INIT 10000
#define FBSEM_CNT 32

static pthread_mutex_t fb_mutex = PTHREAD_MUTEX_INITIALIZER;
static int fb_semid = -1;
static int fb_semnum = 0;


static int fb_getsem(int *sem, int *num)
{
	struct sembuf sop;

	pthread_mutex_lock(&fb_mutex);
	if (fb_semid == -1
	    || fb_semnum == FBSEM_CNT-1) {
		if ((fb_semid = semget(IPC_PRIVATE, FBSEM_CNT,
				       IPC_CREAT|0600)) == -1)
			goto err;
		fb_semnum = 0;
	}
	*sem = fb_semid;
	*num = ++fb_semnum;
	/* Work around an IRIX/gcc bug wrt to semctl.
	 * old code:
	if (semctl(fb_semid, fb_semnum, SETVAL,
		   (union semun){FBSEM_INIT}) != FBSEM_INIT)
		goto err;
	 */
	sop.sem_num = fb_semnum;
	sop.sem_op = FBSEM_INIT;
	sop.sem_flg = 0;
	if (semop(fb_semid, &sop, 1) == -1)
	        goto err;
	sop.sem_num = 0;
	sop.sem_op = 1;
	sop.sem_flg = 0;
	if (semop(fb_semid, &sop, 1) == -1)
		goto err;
	pthread_mutex_unlock(&fb_mutex);

	return 0;

 err:
	pthread_mutex_unlock(&fb_mutex);

	return -1;
}

static int fb_tryputsem(int sem, int num)
{
	struct sembuf sop;

	/* fast path first, unlocked */
	sop.sem_num = num;
	sop.sem_op = -FBSEM_INIT;
	sop.sem_flg = IPC_NOWAIT;
	if (semop(sem, &sop, 1) == -1)
		return 0;

	pthread_mutex_lock(&fb_mutex);
	sop.sem_num = num;
	sop.sem_op = -FBSEM_INIT;
	sop.sem_flg = IPC_NOWAIT;
	if (semop(sem, &sop, 1) == -1)
		goto not_freed;
	sop.sem_num = 0;
	sop.sem_op = -1;
	sop.sem_flg = 0;
	semop(sem, &sop, 1); /* cannot fail */
	sop.sem_num = 0;
	sop.sem_op = 0;
	sop.sem_flg = IPC_NOWAIT;
	if (semop(sem, &sop, 1) == 0) {
		if (sem == fb_semnum)
			fb_semnum = -1;
		semctl(sem, 0, IPC_RMID, (union semun)0); /* cannot fail */
	}
	pthread_mutex_unlock(&fb_mutex);
	return 1;

 not_freed:
	pthread_mutex_unlock(&fb_mutex);
	return 0;
}


int fbuf_ref(filter_buffer_t *fb)
{
	struct sembuf sop;

	if (!fb)
		return 0;

	sop.sem_num = fb->semnum;
	sop.sem_op = -1;
	sop.sem_flg = 0; /* SEM_UNDO? */
	return semop(fb->semid, &sop, 1);
}
int fbuf_unref(filter_buffer_t *fb)
{
	struct sembuf sop;

	if (!fb)
		return 0;

	sop.sem_num = fb->semnum;
	sop.sem_op = 1;
	sop.sem_flg = 0; /* SEM_UNDO? */
	semop(fb->semid, &sop, 1); /* cannot fail */
	if (fb_tryputsem(fb->semid, fb->semnum))
		free(fb);
	return 0;
}

static filter_buffer_t *fbuf_create(SAMPLE *buf, int size)
{
	filter_buffer_t *fb;

	if (!(fb = ALLOC(filter_buffer_t)))
		return NULL;
	if (fb_getsem(&fb->semid, &fb->semnum) == -1) {
		free(fb);
		return NULL;
	}
	fb->size = size;
	fb->buf = buf;

	fbuf_ref(fb);

	return fb;
}

filter_buffer_t *fbuf_alloc(int size)
{
	SAMPLE *buf;

	if (!(buf = (SAMPLE *)malloc(sizeof(SAMPLE)*size)))
		return NULL;
	return fbuf_create(buf, size);
}

filter_buffer_t *fbuf_lock(filter_buffer_t *fb)
{
	filter_buffer_t *fbcopy;
	struct sembuf sop;

	sop.sem_num = fb->semnum;
	sop.sem_op = -(FBSEM_INIT-1);
	sop.sem_flg = IPC_NOWAIT;
	if (semop(fb->semid, &sop, 1) == 0)
		return fb;

	if (!(fbcopy = fbuf_alloc(fb->size)))
		return NULL;
	memcpy(fbcopy->buf, fb->buf, sizeof(SAMPLE)*fb->size);
	sop.sem_num = fbcopy->semnum;
	sop.sem_op = -(FBSEM_INIT-1);
	sop.sem_flg = 0;
	if (semop(fbcopy->semid, &sop, 1) == -1)
		return NULL; /* !?? cannot happen! */

	/* release reference to old buffer */
	fbuf_unref(fb);

	return fbcopy;
}

int fbuf_unlock(filter_buffer_t *fb)
{
	struct sembuf sop;

	sop.sem_num = fb->semnum;
	sop.sem_op = (FBSEM_INIT-1);
	sop.sem_flg = 0;
	if (semop(fb->semid, &sop, 1) == -1)
		return -1; /* !?? cannot happen! */

	return 0;
}



/* ok, this is supposed to get a new buffer address from the
 * pipe. BLOCKING! - i.e. the caller can do a simple
 * for (i=0; i<cnt; i++)
 *        buf[i] = get_buffer(pipe[i]);
 * and is guaranteed to have all buffers updated.
 * a NULL return-value is special (EOF mark). remember, the EOF
 * has to be forwarded to all output-channels!
 * NULL is also a good error-condition marker - EOF lets the
 * filter quit anyway.
 * as pipe-writes are atomic, pipe-reads (of this low quantity)
 * are atomic, too (??).
 */
filter_buffer_t *fbuf_get(filter_pipe_t *p)
{
	filter_buffer_t *fbuf;
	int res;

	do {
		res = read(p->dest_fd, &fbuf, sizeof(filter_buffer_t *));
	} while (res == -1 && errno == EINTR);

	return res == -1 ? NULL : fbuf;
}

/* send one buffer (address) along the specified pipe.
 * as pipe-writes are atomic, this is rather simple
 */
int fbuf_queue(filter_pipe_t *p, filter_buffer_t *fbuf)
{
	int res;

	do {
		res = write(p->source_fd, &fbuf, sizeof(filter_buffer_t *));
	} while (res == -1 && errno == EINTR);

	return res;
}
