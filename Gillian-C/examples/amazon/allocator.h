#ifndef AWS_COMMON_ALLOCATOR_H
#define AWS_COMMON_ALLOCATOR_H
/**
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0.
 */

#include <stdlib.h>

/* Allocator structure. An instance of this will be passed around for anything
 * needing memory allocation */
struct aws_allocator {
    void *(*mem_acquire)(struct aws_allocator *allocator, size_t size);
    void (*mem_release)(struct aws_allocator *allocator, void *ptr);
    /* Optional method; if not supported, this pointer must be NULL */
    void *(*mem_realloc)(struct aws_allocator *allocator, void *oldptr,
                         size_t oldsize, size_t newsize);
    /* Optional method; if not supported, this pointer must be NULL */
    void *(*mem_calloc)(struct aws_allocator *allocator, size_t num,
                        size_t size);
    void *impl;
};

void *s_default_malloc(struct aws_allocator *allocator, size_t size);
void s_default_free(struct aws_allocator *allocator, void *ptr);
void *s_default_calloc(struct aws_allocator *allocator, size_t num,
                       size_t size);
void *s_default_realloc(struct aws_allocator *allocator, void *ptr,
                        size_t oldsize, size_t newsize);

void *aws_mem_acquire(struct aws_allocator *allocator, size_t size);

void aws_mem_release(struct aws_allocator *allocator, void *ptr);

#endif /* AWS_COMMON_ALLOCATOR_H */