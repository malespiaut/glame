/*
 * txn.c
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <pthread.h>
#include "hash.h"
#include "txn.h"


/* Global state.
 */
static txnid_t _txn_id = 0;
static LIST_HEAD(_txn_list);


/* Convenience.
 */
#define _txn_is_active(t) (((t)->parent && (t)->parent->active == (t)) || (t)->active)
#define _txn_is_unused(t) (!(t)->op && list_empty(&(t)->childs))
#define txn_foreach_child_backward(transaction, childvar) for ((childvar) = list_entry((transaction)->childs.prev, struct txn, list); &(childvar)->list != &(transaction)->childs; (childvar) = list_entry((childvar)->list.prev, struct txn, list))


/* Transaction id hash.
 */

/* Locking - can be defined to nops if no threads are used */
static pthread_mutex_t _txn_hash_mutex = PTHREAD_MUTEX_INITIALIZER;
#define LOCK pthread_mutex_lock(&_txn_hash_mutex)
#define UNLOCK pthread_mutex_unlock(&_txn_hash_mutex)

/* The hash itself, autogenerated by hash.h */
HASH(txn, struct txn, 8,
     (txn->id == id),
     (id),
     (txn->id),
     txnid_t id)


/* Internal API using struct txn.
 */

struct txn *_txn_start(struct txn *parent);
void _txn_end(struct txn *t);
void _txn_abort(struct txn *t);
void _txn_delete(struct txn *t);
void __txn_undo(struct txn *t);
struct txn *_txn_undo(struct txn *undo, struct txn *t);

/* Its assumed that starting a new transaction in the
 * domain of parent is legal. */
struct txn *_txn_start(struct txn *parent)
{
	struct txn *t;

	if (!(t = (struct txn *)malloc(sizeof(struct txn))))
		return NULL;
	memset(t, 0, sizeof(struct txn));

	/* init struct */
	hash_init_txn(t);
	t->id = ++_txn_id;
	t->parent = parent;
	INIT_LIST_HEAD(&t->list);
	if (parent)
		t->root = parent->root;
	else
		t->root = t;
	INIT_LIST_HEAD(&t->childs);
	t->active = NULL;
	t->op = NULL;
	
	/* link to parent (or global txn list) and hash */
	if (parent) {
		list_add(&t->list, &parent->childs);
		parent->active = t;
	} else {
		list_add(&t->list, &_txn_list);
	}
	LOCK;
	hash_add_txn(t);
	UNLOCK;

	return t;
}

/* Its assumed that ending the transaction is legal. */
void _txn_end(struct txn *t)
{
	if (t->parent)
		t->parent->active = NULL;
}


/* Never fail _txn_undo, not using transactions. We have to
 * PANIC on failure. */
void __txn_undo(struct txn *t)
{
	struct txn *ct;

	/* recursively undo transactions - backwards! */
	txn_foreach_child_backward(t, ct)
		__txn_undo(ct);

	/* undo self */
	if (t->op)
		if (t->op->undo(t, NULL) == -1)
			PANIC("Undo failed for txn_abort()!");
}

/* Its assumed that aborting t is ok, i.e. t is active.
 * _txn_abort may not fail. */
void _txn_abort(struct txn *t)
{
	/* recursively abort active child transaction */
	if (t->active)
		_txn_abort(t->active);

	/* undo and delete self */
	__txn_undo(t);
	_txn_delete(t);
}


/* Its assumed it is safe to delete the transaction,
 * i.e. transactions is not active. */
void _txn_delete(struct txn *t)
{
	struct txn *ct;

	/* remove from parent list and unhash */
	LOCK;
	hash_remove_txn(t);
	UNLOCK;
	list_del(&t->list);

	/* recursively delete child transactions */
	while ((ct = list_gethead(&t->childs, struct txn, list)))
		_txn_delete(ct);

	/* delete self */
	if (t->op)
		t->op->del(t);

	free(t);
}

/* Its assumed, the transaction is an ended one. */
struct txn *_txn_undo(struct txn *undo, struct txn *t)
{
	struct txn *ct, *ut, *uct;

	/* start undo transaction */
	if (!(ut = _txn_start(undo)))
		return NULL;

	/* recursively create undo transactions - backwards! */
	txn_foreach_child_backward(t, ct) {
		if (!(uct = _txn_undo(ut, ct))) {
			_txn_abort(ut);
			return NULL;
		}
	}

	/* undo self */
	if (t->op)
		t->op->undo(t, ut);

	/* end undo transaction */
	_txn_end(ut);

	return ut;
}



/* User visible API using txnid_t.
 */

txnid_t txn_start(txnid_t parent)
{
	struct txn *pt, *t;

	if (parent != TXN_NONE) {
	        LOCK;
		pt = hash_find_txn(parent);
		UNLOCK;
		if (!pt || pt->op || pt->active)
			return -1;
	} else {
		pt = NULL;
	}

	if (!(t = _txn_start(pt)))
		return -1;

	return t->id;
}

int txn_end(txnid_t id)
{
	struct txn *t;

	LOCK;
	t = hash_find_txn(id);
	UNLOCK;
	if (!t)
		return -1;
	/* transaction already ended? */
	if (t->parent && !(t->parent->active == t))
		return -1;
	/* transaction not complete? */
	if (t->active)
		return -1;

	/* Special case empty transaction - we can just delete it,
	 * but warn the user, as this is not really valid usage. */
	if (_txn_is_unused(t)) {
		/* This is _txn_end() - on an invalid transaction. */
		if (t->parent)
			t->parent->active = NULL;
		/* This is _txn_delete() - on an invalid transaction. */
		LOCK;
		hash_remove_txn(t);
		UNLOCK;
		list_del(&t->list);
		free(t);
		return 0;
	}

	/* Now its ok to end the transaction. */
	_txn_end(t);

	return 0;
}

int txn_abort(txnid_t id)
{
	struct txn *t;

	LOCK;
	t = hash_find_txn(id);
	UNLOCK;
	if (!t)
		return -1;
	if (!_txn_is_active(t))
		return -1;

	_txn_abort(t);

	return 0;
}


txnid_t txn_undo(txnid_t id)
{
	struct txn *t, *ut;

	LOCK;
	t = hash_find_txn(id);
	UNLOCK;
	if (!t)
		return -1;
	if (_txn_is_active(t))
		return -1;

	if (!(ut = _txn_undo(NULL, t)))
		return -1;

	return ut->id;
}


int txn_delete(txnid_t id)
{
	struct txn *t;

	LOCK;
	t = hash_find_txn(id);
	UNLOCK;
	if (!t)
		return -1;
	if (_txn_is_active(t))
		return -1;

	/* delete transactions */
	_txn_delete(t);

	return 0;
}


void txn_abort_and_delete_all()
{
    	struct txn *t;
	int i;

    	/* This is very brute force - but it should work.
	 * In case of inproper global locking by the caller
	 * the worst thing that can happen is incomplete
	 * operation. */
        for (i=0; i<(1<<8); i++) {
	    do {
		LOCK;
		t = txn_hash_table[i];
		UNLOCK;
		if (!t)
		    continue;
		if (_txn_is_active(t))
		    _txn_abort(t);
		else
		    _txn_delete(t);
	    } while (t);
	}
}


int txn_finish(txnid_t id, struct txn_op *ops)
{
	struct txn *t;

	LOCK;
	t = hash_find_txn(id);
	UNLOCK;
	if (!t)
		return -1;

	/* Transaction needs to be completely unused to be finished. */
	if (!_txn_is_unused(t))
		return -1;

	/* Finish transaction. */
	t->op = ops;

	/* Delete transaction, if it has no parent. */
	if (!t->parent)
		_txn_delete(t);

	return 0;
}



struct ui_txn_op {
	struct txn_op op;
	const char *message;
};

static int ui_undo(struct txn *txn, struct txn *dest);
static void ui_delete(struct txn *txn);

int txn_finish_unimplemented(txnid_t id, const char *message)
{
	struct ui_txn_op *op;

	if (!(op = (struct ui_txn_op *)malloc(sizeof(struct ui_txn_op))))
		return -1;
	op->op.undo = ui_undo;
	op->op.del = ui_delete;
	op->message = message;
	if (txn_finish(id, &op->op) == -1) {
		free(op);
		return -1;
	}
	return 0;
}

static int ui_undo(struct txn *txn, struct txn *dest)
{
	struct ui_txn_op *op = (struct ui_txn_op *)txn->op;

	fprintf(stderr, "Issued undo/abort of unimplemented transaction.\n");
	fprintf(stderr, "%s\n", op->message);
	fprintf(stderr, "Forcing SIGSEGV so you can debug this.\n");
	*((int *)0) = 0;

	return -1;
}

static void ui_delete(struct txn *txn)
{
	free(txn->op);
}
