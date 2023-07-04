#ifndef AWS_COMMON_HASH_TABLE_H
#define AWS_COMMON_HASH_TABLE_H

/**
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0.
 */

#include <stdlib.h>

/**
 * Hash table data structure. This module provides an automatically resizing
 * hash table implementation for general purpose use. The hash table stores a
 * mapping between void * keys and values; it is expected that in most cases,
 * these will point to a structure elsewhere in the heap, instead of inlining a
 * key or value into the hash table element itself.
 *
 * Currently, this hash table implements a variant of robin hood hashing, but
 * we do not guarantee that this won't change in the future.
 *
 * Associated with each hash function are four callbacks:
 *
 *   hash_fn - A hash function from the keys to a uint64_t. It is critical that
 *      the hash function for a key does not change while the key is in the hash
 *      table; violating this results in undefined behavior. Collisions are
 *      tolerated, though naturally with reduced performance.
 *
 *   equals_fn - An equality comparison function. This function must be
 *      reflexive and consistent with hash_fn.
 *
 *   destroy_key_fn, destroy_value_fn - Optional callbacks invoked when the
 *      table is cleared or cleaned up and at the caller's option when an
 * element is removed from the table. Either or both may be set to NULL, which
 *      has the same effect as a no-op destroy function.
 *
 * This datastructure can be safely moved between threads, subject to the
 * requirements of the underlying allocator. It is also safe to invoke
 * non-mutating operations on the hash table from multiple threads. A suitable
 * memory barrier must be used when transitioning from single-threaded mutating
 * usage to multithreaded usage.
 */
struct hash_table_state; /* Opaque pointer */
struct aws_hash_table {
    struct hash_table_state *p_impl;
};

/**
 * Removes every element from the hash map. destroy_fn will be called for
 * each element.
 */
void aws_hash_table_clear(struct aws_hash_table *map);

/**
 * Inserts a new element at key, with the given value. If another element
 * exists at that key, the old element will be overwritten; both old key and
 * value objects will be destroyed.
 *
 * If was_created is non-NULL, *was_created is set to 0 if an existing
 * element was found, or 1 is a new element was created.
 *
 * Returns AWS_OP_SUCCESS if an item was found or created.
 * Raises AWS_ERROR_OOM if hash table expansion was required and memory
 */
int aws_hash_table_put(struct aws_hash_table *map, const void *key, void *value,
                       int *was_created);

#endif /* AWS_COMMON_HASH_TABLE_H */