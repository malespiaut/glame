#ifndef _TXN_H
#define _TXN_H

/*
 * txn.h
 *
 * $Id: txn.h,v 1.1 2000/09/21 09:27:10 richi Exp $
 * 
 * Copyright (C) 2000 Richard Guenther
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

#include <list.h>


/* General transaction support. Thread safety is ensured for
 * parallel operation on independend transactions. For parallel
 * operation on dependend transactions or on one transaction
 * you have to ensure that no parallel invocation of any API
 * function is done.
 * To use this subsystem from an integration point of view you
 * have to wrap your "operations" so they can create sub-transactions
 * to a parent transaction and provide necessary functions and data
 * to undo their work and clean up after them.
 */

typedef int tid_t;
#define TXN_NONE 0

struct txn;
struct txn_op;

/* Operation skeleton (extend for actual operation). */
struct txn_op {
	/* The undo method should undo the transaction txn and
	 * fill a txn_op for this reverse operation into dest
	 * (or dont do that, if dest is NULL - in this case
	 * its a txn_abort() operation which really should not
	 * fail!). Return 0 if everything went ok, else -1. */
	int (*undo)(struct txn *txn, struct txn *dest);

	/* The delete method should free any storage required
	 * by the txn_op and the txn_op itself. */
	void (*delete)(struct txn *txn);

	/* private stuff should follow */
};

/* Transaction record. */
struct txn {
	/* ID of the transaction and entry in the tid
	 * hashtable */
	struct txn **pprev_txn_hash;
	struct txn *next_txn_hash;
	tid_t id;

	/* parent transaction, list of childs of the
	 * parent - in case of root = current the
	 * list is the global list of independend
	 * transactions */
	struct txn *parent; /* do we need this? FIXME. */
	struct list_head list;

	/* root of the transaction chain - useful for 
	 * locking in nested transactions, i.e. resources
	 * really belong to the transaction root. */
	struct txn *root;


	/* the following two groups are mutually exclusive
	 * once one is used:
	 * - a child may be added if op == NULL (and active == NULL)
	 * - op may be used, if list_empty(childs)
	 */

	/* list of childs, active one (or NULL if self
	 * is active) */
	struct list_head childs;
	struct txn *active;

	/* operation record, NULL if grouping node (not
	 * leaf) */
	struct txn_op *op;
};



/* Start a new transaction as child of the provided parent transaction
 * (can be TXN_NONE if the transaction should be a independend one).
 * Returns a transaction id or -1 on error. On error either the specified
 * parent transaction does not exist, it has already an active child
 * (violates transaction may not cross) or there is insufficient memory
 * to allocate internal data structures. */
tid_t txn_start(tid_t parent);

/* End the specified transaction. Returns 0 on success and -1 on error.
 * On error either the specified transaction does not exist, it is
 * already ended or it has active child transactions. */
int txn_end(tid_t id);

/* Abort the specified transaction thereby aborting active child
 * transactions and undoing all previous work. txn_abort itself is
 * not undoable. Returns 0 on success, -1 on error which is usually
 * an invalid supplied transaction id or an inactive transaction. */
int txn_abort(tid_t id);


/* Undo applies the reverse transaction as a new transaction,
 * so txn_undo(txn_undo(id)) restores state after id, simply
 * deleting the returned tid prevents redo, deleting the original
 * id after undo prevents undo after redo. */
tid_t txn_undo(tid_t id);

/* Delete the specified (inactive) transaction and free all memory
 * associated with it. Returns 0 on success and -1 on error which
 * means you have supplied either an invalid or an active transaction. */
int txn_delete(tid_t id);


/* Abort/delete all active/inactive transactions. This is mainly for
 * cleanup purposes before program end or the like. Note that this is
 * not thread-safe by design, but you have to ensure proper locking
 * yourself or hope to be lucky... (which is usually ok in case
 * of normal program termination). */
void txn_abort_and_delete_all();


/* Interface for writing operations.
 */

struct txn *txn_get_struct(tid_t id);


#endif
