/**
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0.
 */

/*
 This file corresponds to the following commit:
 https://github.com/awslabs/aws-c-common/tree/bb797381f3468e6f076e53eddbb399a99f54f67b
*/

#include "byte_buf.h"

int aws_byte_buf_init(struct aws_byte_buf *buf, size_t capacity) {

    buf->buffer = (capacity == 0) ? NULL : malloc(capacity);
    if (capacity != 0 && buf->buffer == NULL) {
        AWS_ZERO_STRUCT(*buf);
        return 1;
    }

    buf->len = 0;
    buf->capacity = capacity;
    // AWS_POSTCONDITION(aws_byte_buf_is_valid(buf));
    return 0;
}

void aws_byte_buf_clean_up(struct aws_byte_buf *buf) {
    // AWS_PRECONDITION(aws_byte_buf_is_valid(buf));
    if (buf->buffer) {
        free((void *)buf->buffer);
    }
    buf->buffer = NULL;
    buf->len = 0;
    buf->capacity = 0;
}

/**
 * Tests if the given aws_byte_cursor has at least len bytes remaining. If so,
 * *buf is advanced by len bytes (incrementing ->ptr and decrementing ->len),
 * and an aws_byte_cursor referring to the first len bytes of the original *buf
 * is returned. Otherwise, an aws_byte_cursor with ->ptr = NULL, ->len = 0 is
 * returned.
 *
 * Note that if len is above (SIZE_MAX / 2), this function will also treat it as
 * a buffer overflow, and return NULL without changing *buf.
 */
struct aws_byte_cursor
aws_byte_cursor_advance(struct aws_byte_cursor *const cursor,
                        const size_t length) {
    struct aws_byte_cursor rv;
    if (cursor->len > 2147483647 || length > 2147483647 ||
        length > cursor->len) {
        rv.ptr = NULL;
        rv.len = 0;
    } else {
        rv.ptr = cursor->ptr;
        rv.len = length;

        cursor->ptr += length;
        cursor->len -= length;
    }
    return rv;
}

bool aws_byte_cursor_read(struct aws_byte_cursor *cur, void *dest,
                          const size_t length) {
    // AWS_PRECONDITION(aws_byte_cursor_is_valid(cur));
    // AWS_PRECONDITION(AWS_MEM_IS_WRITABLE(dest, len));
    if (length == 0) {
        return 1;
    }

    struct aws_byte_cursor slice = aws_byte_cursor_advance(cur, length);

    if (slice.ptr) {
        memcpy(dest, slice.ptr, length);
        // AWS_POSTCONDITION(aws_byte_cursor_is_valid(cur));
        // AWS_POSTCONDITION(AWS_MEM_IS_READABLE(dest, len));
        return 1;
    }
    // AWS_POSTCONDITION(aws_byte_cursor_is_valid(cur));
    return 0;
}

/**
 * Reads a single byte from cursor, placing it in *var.
 *
 * On success, returns true and updates the cursor pointer/length accordingly.
 * If there is insufficient space in the cursor, returns false, leaving the
 * cursor unchanged.
 */

bool aws_byte_cursor_read_u8(struct aws_byte_cursor *cur, uint8_t *var) {
    // AWS_PRECONDITION(aws_byte_cursor_is_valid(cur));
    // AWS_PRECONDITION(AWS_MEM_IS_WRITABLE(var, 1));
    bool rv = aws_byte_cursor_read(cur, var, 1);
    // AWS_POSTCONDITION(aws_byte_cursor_is_valid(cur));
    return rv;
}

/**
 * Reads a 16-bit value in network byte order from cur, and places it in host
 * byte order into var.
 *
 * On success, returns true and updates the cursor pointer/length accordingly.
 * If there is insufficient space in the cursor, returns false, leaving the
 * cursor unchanged.
 */
bool aws_byte_cursor_read_be16(struct aws_byte_cursor *cur, uint16_t *var) {
    // AWS_PRECONDITION(aws_byte_cursor_is_valid(cur));
    // AWS_PRECONDITION(AWS_OBJECT_PTR_IS_WRITABLE(var));
    bool rv = aws_byte_cursor_read(cur, var, 2);

    if (aws_cryptosdk_likely(rv)) {
        *var = aws_ntoh16(*var);
    }

    // AWS_POSTCONDITION(aws_byte_cursor_is_valid(cur));
    return rv;
}

/**
 * Reads a 32-bit value in network byte order from cur, and places it in host
 * byte order into var.
 *
 * On success, returns true and updates the cursor pointer/length accordingly.
 * If there is insufficient space in the cursor, returns false, leaving the
 * cursor unchanged.
 */
bool aws_byte_cursor_read_be32(struct aws_byte_cursor *cur, uint32_t *var) {
    // AWS_PRECONDITION(aws_byte_cursor_is_valid(cur));
    // AWS_PRECONDITION(AWS_OBJECT_PTR_IS_ WRITABLE(var));
    bool rv = aws_byte_cursor_read(cur, var, 4);

    if (aws_cryptosdk_likely(rv)) {
        *var = aws_ntoh32(*var);
    }

    // AWS_POSTCONDITION(aws_byte_cursor_is_valid(cur));
    return rv;
}

/**
 * Reads as many bytes from cursor as size of buffer, and copies them to buffer.
 *
 * On success, returns true and updates the cursor pointer/length accordingly.
 * If there is insufficient space in the cursor, returns false, leaving the
 * cursor unchanged.
 */
bool aws_byte_cursor_read_and_fill_buffer(struct aws_byte_cursor *cur,
                                          struct aws_byte_buf *dest) {
    // AWS_PRECONDITION(aws_byte_cursor_is_valid(cur));
    // AWS_PRECONDITION(aws_byte_buf_is_valid(dest));
    if (aws_byte_cursor_read(cur, dest->buffer, dest->capacity)) {
        dest->len = dest->capacity;
        // AWS_POSTCONDITION(aws_byte_cursor_is_valid(cur));
        // AWS_POSTCONDITION(aws_byte_buf_is_valid(dest));
        return true;
    }
    // AWS_POSTCONDITION(aws_byte_cursor_is_valid(cur));
    // AWS_POSTCONDITION(aws_byte_buf_is_valid(dest));
    return false;
}