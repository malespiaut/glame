#ifndef _PMAP_H
#define _PMAP_H

/*
 * pmap.h
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

/* Cached/shared file mapping functionality.
 *
 * The idea is to have identical shared mappings share their
 * virtual mapping. Also caching of such mappings is provided
 * so that implicit msync() on munmap() is avoided, if the
 * mapping is reused again.
 * So its like having mmap() and munmap() having recursive,
 * reference-counting semantics - this is good for threads
 * independetly using a backing store in a shared manner and
 * for saving virtual memory space. Optimized zero-mapping
 * and map-discard is also provided.
 *
 * General usage is to init the pmap subsystem by specifying
 * the amount of (continous) virtual memory space you want
 * to have available - i.e. not used by the pmap cache.
 */


/* Init the pmap subsystem. 0 is returned on success, -1 on failure.
 * The parameter specifies the amount of virtual memory to keep free,
 * i.e. indirectly the maximum size of the mmap cache. */
int pmap_init(size_t minfree);

/* pmap_close unmaps all remaining mappings and cleans up after the
 * pmap subsystem. */
void pmap_close();


/* pmap_map works like mmap() - see mmap(2) for detailed parameter
 * description.
 * Note that MAP_SHARED mappings may share the virtual mapping
 * with another user of the same type (length/prot/flags/fd/offset). */
void *pmap_map(void *start, size_t length, int prot,
	       int flags, int fd, off_t offset);

/* pmap_zeromap works like pmap_map() but zeros the resulting mapping
 * in an efficient way. */
void *pmap_zeromap(void *start, size_t length, int prot,
		   int flags, int fd, off_t offset);


/* Unlike munmap() pmam_unmap does not support partial unmaps of
 * a mapped area, i.e. start has to be the beginning of a previously
 * pmap_mapped area. */
int pmap_unmap(void *start);

/* Unlike pmap_unmap, pmap_discard discards a mapping completely,
 * throwing away any possible dirty data. For an effect different
 * to pmap_unmap you have to be the last user of the mapping. */
int pmap_discard(void *start);


/* pmap_uncache tries to unmap any possible chached mapping of the
 * specified type. Note that only unused mappings can be unmapped
 * this way. */
int pmap_uncache(size_t size, int prot, int flags, int fd, off_t offset);


/* Try to drain the cache as much as possible - unmapping all unused
 * mappings. Do this if you are short on virtual memory. */
void pmap_shrink();


#endif
