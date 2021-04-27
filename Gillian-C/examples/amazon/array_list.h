#ifndef AWS_COMMON_ARRAY_LIST_H
#define AWS_COMMON_ARRAY_LIST_H

/**
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0.
 */

#include "allocator.h"
#include <stdlib.h>

struct aws_array_list {
    struct aws_allocator *alloc;
    size_t current_size;
    size_t length;
    size_t item_size;
    void *data;
};

/**
 * Pushes the memory pointed to by val onto the end of internal list
 */
int aws_array_list_push_back(struct aws_array_list *list, const void *val);

int aws_array_list_get_at_ptr(const struct aws_array_list *list, void **val,
                              size_t index);

void aws_array_list_clear(struct aws_array_list *list);
#endif /* AWS_COMMON_ARRAY_LIST_H */