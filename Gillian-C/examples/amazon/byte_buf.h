#ifndef AWS_COMMON_BYTE_BUF_H
#define AWS_COMMON_BYTE_BUF_H
/**
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0.
 */

#include "allocator.h"
#include "base.h"
#include <stdint.h>
#include <stdlib.h>

/**
 * Represents a length-delimited binary string or buffer. If byte buffer points
 * to constant memory or memory that should otherwise not be freed by this
 * struct, set allocator to NULL and free function will be a no-op.
 *
 * This structure used to define the output for all functions that write to a
 * buffer.
 *
 * Note that this structure allocates memory at the buffer pointer only. The
 * struct itself does not get dynamically allocated and must be either
 * maintained or copied to avoid losing access to the memory.
 */
struct aws_byte_buf {
    size_t len;
    uint8_t *buffer;
    size_t capacity;
    struct aws_allocator *allocator;
};

/**
 * Represents a movable pointer within a larger binary string or buffer.
 *
 * This structure is used to define buffers for reading.
 */
struct aws_byte_cursor {
    size_t len;
    uint8_t *ptr;
};

void aws_byte_buf_clean_up(struct aws_byte_buf *buf);

struct aws_byte_cursor
aws_byte_cursor_advance(struct aws_byte_cursor *const cursor,
                        const size_t length);

bool aws_byte_cursor_read(struct aws_byte_cursor *cur, void *dest,
                          const size_t length);

bool aws_byte_cursor_read_u8(struct aws_byte_cursor *cur, uint8_t *var);

bool aws_byte_cursor_read_be16(struct aws_byte_cursor *cur, uint16_t *var);

bool aws_byte_cursor_read_be32(struct aws_byte_cursor *cur, uint32_t *var);

bool aws_byte_cursor_read_and_fill_buffer(struct aws_byte_cursor *cur,
                                          struct aws_byte_buf *dest);

int aws_byte_buf_init(struct aws_byte_buf *buf, struct aws_allocator *allocator,
                      size_t capacity);

#endif /* AWS_COMMON_BYTE_BUF_H */