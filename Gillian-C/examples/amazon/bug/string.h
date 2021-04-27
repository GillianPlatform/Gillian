#ifndef AWS_COMMON_STRING_H
#define AWS_COMMON_STRING_H
/**
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0.
 */

#include "../allocator.h"
#include "../base.h"

struct aws_string {
    struct aws_allocator *const allocator;
    const size_t len;
    /* give this a storage specifier for C++ purposes. It will likely be larger after init. */
    const uint8_t bytes[1];
};

/**
 * Allocate a new string with the same contents as array.
 */
struct aws_string *aws_string_new_from_array(struct aws_allocator *allocator,
                                             const uint8_t *bytes, size_t len);

#endif // AWS_COMMON_STRING_H