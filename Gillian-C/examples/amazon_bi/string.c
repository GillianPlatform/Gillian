/**
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0.
 */
#include "string.h"

struct aws_string *aws_string_new_from_array(const uint8_t *bytes,
                                             size_t length) {
    // AWS_PRECONDITION(allocator);
    // AWS_PRECONDITION(AWS_MEM_IS_READABLE(bytes, length));
    size_t malloc_size;
    if (aws_add_size_checked(sizeof(struct aws_string) + 1, length,
                             &malloc_size)) {
        return NULL;
    }
    struct aws_string *str = malloc(malloc_size);
    if (!str) {
        return NULL;
    }

    /* Fields are declared const, so we need to copy them in like this */
    *(size_t *)(&str->len) = length;
    if (length > 0) {
        memcpy((void *)str->bytes, bytes, length);
    }
    *(uint8_t *)&str->bytes[length] = '\0';
    // AWS_RETURN_WITH_POSTCONDITION(str, aws_string_is_valid(str));
    return str;
}

void aws_string_destroy(struct aws_string *str) {
    if (str) {
        free(str);
    }
}