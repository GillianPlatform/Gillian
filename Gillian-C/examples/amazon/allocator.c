/**
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0.
 */

#include "allocator.h"
#include "error.h"

/*@ pred default_allocator(allocator) {
  (allocator -> struct aws_allocator {
    funptr(s_default_malloc);
    funptr(s_default_free);
    funptr(s_default_realloc);
    funptr(s_default_calloc);
    NULL
    })
}
*/

void *s_default_malloc(struct aws_allocator *allocator, size_t size) {
    (void)allocator;
    return malloc(size);
}

void s_default_free(struct aws_allocator *allocator, void *ptr) {
    (void)allocator;
    free(ptr);
}

void *s_default_calloc(struct aws_allocator *allocator, size_t num,
                       size_t size) {
    (void)allocator;
    return calloc(num, size);
}

/* This function is the only one unchanged from the original implementation */
void *s_default_realloc(struct aws_allocator *allocator, void *ptr,
                        size_t oldsize, size_t newsize) {
    (void) allocator;
    (void) oldsize;

    if (newsize == 0) {
        free(ptr);
        return NULL;
    }

    if (newsize <= oldsize) {
        return ptr;
    }

    void *new_mem = s_default_malloc(allocator, newsize);
    memcpy(new_mem, ptr, oldsize);
    s_default_free(allocator, ptr);
    return new_mem;
}

void *aws_mem_acquire(struct aws_allocator *allocator, size_t size) {
    // AWS_FATAL_PRECONDITION(allocator != NULL);
    // AWS_FATAL_PRECONDITION(allocator->mem_acquire != NULL);
    /* Protect against
     * https://wiki.sei.cmu.edu/confluence/display/c/MEM04-C.+Beware+of+zero-length+allocations
     */
    // AWS_FATAL_PRECONDITION(size != 0);

    void *mem = allocator->mem_acquire(allocator, size);
    if (!mem) {
         aws_raise_error(AWS_ERROR_OOM);
    }
    return mem;
}

void aws_mem_release(struct aws_allocator *allocator, void *ptr) {
    if (ptr != NULL) {
        allocator->mem_release(allocator, ptr);
    }
}