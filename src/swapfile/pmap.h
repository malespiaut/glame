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
 * the amount of virtual memory space you want the pmap cache
 * to use.
 */


/* Init the pmap subsystem. 0 is returned on success, -1 on failure.
 * The parameter specifies the amount of virtual memory to use for
 * the mmap cache. */
int pmap_init(size_t maxsize);

/* pmap_close unmaps all remaining mappings and cleans up after the
 * pmap subsystem. */
void pmap_close();


/* pmap_map works like mmap() - see mmap(2) for detailed parameter
 * description.
 * Note that MAP_SHARED mappings may share the virtual mapping
 * with another user of the same type (length/prot/flags/fd/offset)
 * and that flags other than MAP_SHARED and MAP_PRIVATE are not
 * supported. */
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


/* Try to drain the cache as much as possible - unmapping all unused
 * mappings. Do this if you are short on virtual memory. */
void pmap_shrink();


/* pmap_uncache tries to unmap any unused cached mapping of the specified
 * type. Note that size/offset are not treated exact, but all mappings
 * overlapping that area are affected by the operation.
 * pmap_uncache returns -1, if there were used mappings inside the specified
 * region, else 0. Note that this result is not exactly reliable, as at
 * return time another thread may have created another mapping inside the
 * specified area. You have to lock against this case yourself. */
int pmap_uncache(int fd, off_t offset, size_t size);

/* Like pmap_uncache pmap_invalidate unmaps any unused cached mapping
 * which can be specified like with pmap_uncache. But unlike pmap_uncache
 * it does prevent mappings that cannot be unmapped from being reused
 * (Note: not from _new_ mappings being created!). This is especially
 * useful to prevent mappings to be wrongly reused if closing the file
 * descriptor and reusing it with another file. */
void pmap_invalidate(int fd, off_t offset, size_t size);

/* Checks, if there are (cached or used) mappings with the specified
 * properties. Returns 1 if this is the case, else 0. */
int pmap_has_mappings(int fd, off_t offset, size_t size);


#endif
