/**
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0.
 */

/*
 This file corresponds to the following commit:
 https://github.com/awslabs/aws-c-common/tree/bb797381f3468e6f076e53eddbb399a99f54f67b
*/

#include "byte_buf.h"

// A valid byte cursor is a structure that contains { len; ptr },
// where ptr points to an array of size len (and types uint8_t)
/*@
    pred nounfold valid_aws_byte_cursor_ptr(+cur, length, buffer: List, alpha) {
        (cur -> struct aws_byte_cursor { long(0); buffer }) *
        (length == 0) * (alpha == nil);

        (cur -> struct aws_byte_cursor { long(length); buffer }) * (0 <# length) *
        ARRAY(buffer, char, length, alpha) * (length == len alpha) *
        (length <=# 2147483647)
    }

    lemma valid_aws_byte_cursor_ptr_facts(cur, length, buffer, alpha) {
        hypothesis:
            valid_aws_byte_cursor_ptr(#cur, #length, #buffer, #alpha)

        conclusions:
            valid_aws_byte_cursor_ptr(#cur, #length, #buffer, #alpha) *
            (#length == len #alpha) * (#length <=# 2147483647)

        proof:
            unfold valid_aws_byte_cursor_ptr(#cur, #length, #buffer, #alpha)
    }
*/

// Valid byte buffers
/*@
    pred valid_aws_byte_buf(+length, +capacity, +buffer, +allocator, content) {
        (length == 0) * (capacity == 0) * (buffer == NULL) * (content == []);

        (0 <# capacity) * (length <=# capacity) * (0 <=# length) *
        MARRAY(buffer, char, capacity, #full_data) *
        (len #full_data == capacity) *
        (#full_data == content @ #rest) *
        (len content == length) * (len #rest == capacity - length) *
        (not (allocator == NULL))
    }

    pred empty_aws_byte_buf(+length, +capacity, +buffer, +allocator) {
        (length == 0) * (capacity == 0) * (buffer == NULL) *
        (allocator == NULL)
    }

    pred valid_aws_byte_buf_fields(+fields, length, capacity, buffer, allocator, content) {
        (fields == [ long(length), buffer, long(capacity), allocator ]) *
        valid_aws_byte_buf(length, capacity, buffer, allocator, content)
    }

    pred empty_aws_byte_buf_fields(+fields) {
        (fields ==  [ long(length), buffer, long(capacity), allocator ]) *
        empty_aws_byte_buf(length, capacity, buffer, allocator)
    }

    pred empty_aws_byte_buf_ptr(+buf) {
        (buf -> struct aws_byte_buf {
            long(#length);
            #buffer;
            long(#capacity);
            #allocator
        }) *
        empty_aws_byte_buf(#length, #capacity, #buffer, #allocator)
    }

    pred nounfold valid_aws_byte_buf_ptr(+buf, length, capacity, buffer, allocator, content) {
        (buf -> struct aws_byte_buf {
            long(length);
            buffer;
            long(capacity);
            allocator
        }) *
        valid_aws_byte_buf(length, capacity, buffer, allocator, content)
    }
*/

// aws_byte_buf_init(buf, allocator, capacity) initialises an
// empty buffer at buf with the given capacity
/*@
    spec aws_byte_buf_init(buf, allocator, capacity) {
        requires:
            (buf == #buf) * (allocator == #allocator) *
            (capacity == long(#capacity)) *
            (0 <=# #capacity) *
            empty_aws_byte_buf_ptr(#buf) * default_allocator(#allocator)

        ensures:
            valid_aws_byte_buf_ptr(#buf, 0, #capacity, #buffer, #allocator, []) *
            default_allocator(#allocator) *
            (ret == int(0))
    }
*/
int aws_byte_buf_init(struct aws_byte_buf *buf, struct aws_allocator *allocator,
                      size_t capacity) {
    // AWS_PRECONDITION(buf);
    // AWS_PRECONDITION(allocator);

    GILLIAN("if (! (#capacity = 0)) { apply IntegerLtPlusOneLe(0, #capacity) } ");

    buf->buffer = (capacity == 0) ? NULL : aws_mem_acquire(allocator, capacity);
    if (capacity != 0 && buf->buffer == NULL) {
        AWS_ZERO_STRUCT(*buf);
        return 1;
    }

    buf->len = 0;
    buf->capacity = capacity;
    buf->allocator = allocator;
    // AWS_POSTCONDITION(aws_byte_buf_is_valid(buf));
    return 0;
}

// aws_byte_buf_clean_up(buf) clears the byte buffer at buf
/*@
    spec aws_byte_buf_clean_up(buf) {
        requires:
            (buf == #buf) *
            valid_aws_byte_buf_ptr(#buf, #length, #capacity, #buffer, #allocator, #content) *
            default_allocator(#allocator)

        ensures:
            empty_aws_byte_buf_ptr(#buf) *
            default_allocator(#allocator)

    OR

        requires:
            (buf == #buf) *
            valid_aws_byte_buf_ptr(#buf, #length, #capacity, #buffer, NULL, #content)

        ensures:
            empty_aws_byte_buf_ptr(#buf)
    }
*/
void aws_byte_buf_clean_up(struct aws_byte_buf *buf) {
    // AWS_PRECONDITION(aws_byte_buf_is_valid(buf));
    if (buf->allocator && buf->buffer) {
        aws_mem_release(buf->allocator, (void *)buf->buffer);
    }
    buf->allocator = NULL;
    buf->buffer = NULL;
    buf->len = 0;
    buf->capacity = 0;
}

// Predicates describing what it means for a byte
// buffer read to be valid or invalid
/*@
    pure pred valid_read(read_len, cursor_len) {
        (read_len <=# cursor_len) *
        (read_len <=# 2147483647) *
        (cursor_len <=# 2147483647)
    }

    pure pred invalid_read(read_len, cursor_len) {
        cursor_len <# read_len;
        2147483647 <# read_len;
        2147483647 <# cursor_len
    }
*/

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
// There's an additional argument to that spec since
// that is how CompCert handles struct passing
/*@
    spec aws_byte_cursor_advance(_res, cursor, length) {
        requires:
            (_res == #res) * (cursor == #cursor) * (length == long(#length)) *
            (0 <=# #length) * ARRAY(#res, long, 2, #trash) *
            valid_aws_byte_cursor_ptr(#cursor, #cur_len, #buffer, #content) *
            ((0 <# #length) || (not (#buffer == NULL)))

        ensures:
            invalid_read(#length, #cur_len) *
            valid_aws_byte_cursor_ptr(#res, 0, NULL, nil) *
            valid_aws_byte_cursor_ptr(#cursor, #cur_len, #buffer, #content);

            valid_read(#length, #cur_len) *
            valid_aws_byte_cursor_ptr(#res, #length, #buffer, #data) *
            valid_aws_byte_cursor_ptr(#cursor, #rest_len, #buffer p+ #length, #rest) *
            (#length == len #data) *
            (#content == #data @ #rest) *
            (#rest_len == (#cur_len - #length))
    }
*/
struct aws_byte_cursor
aws_byte_cursor_advance(struct aws_byte_cursor *const cursor,
                        const size_t length) {
    struct aws_byte_cursor rv;
    if (cursor->len > 2147483647 || length > 2147483647||
        length > cursor->len) {
        rv.ptr = NULL;
        rv.len = 0;
    } else {
        rv.ptr = cursor->ptr;
        rv.len = length;

        cursor->ptr += length;
        cursor->len -= length;
        GILLIAN("branch (#length == #cur_len)");
        GILLIAN("branch (#length == 0)");
    }
    return rv;
}

// aws_byte_cursor_read(cur, dest, length) reads length bytes starting
// from the position pointed by cur and copies the result into dest
/*@
    spec aws_byte_cursor_read(cur, dest, length) {
        requires:
            (cur == #cur) * (dest == #dest) * (length == long(#length)) *
            valid_aws_byte_cursor_ptr(#cur, #cur_length, #buffer, #content) *
            writable_memory(#dest, #length, #trash)

        ensures:
            (#length == 0) *
            valid_aws_byte_cursor_ptr(#cur, #cur_length, #buffer, #content) *
            writable_memory(#dest, 0, []) * (#trash == []) *
            (ret == TRUE);

            (0 <# #length) *
            valid_read(#length, #cur_length) *
            ARRAY(#buffer, char, #length, #data) *
            valid_aws_byte_cursor_ptr(#cur, len #rest, #buffer p+ #length, #rest) *
            ARRAY(#dest, char, #length, #data) *
            (#length == len #data) *
            (#content == #data @ #rest) *
            (len #rest == (#cur_length - #length)) *
            (ret == TRUE);

            (0 <# #length) *
            invalid_read(#length, #cur_length) *
            valid_aws_byte_cursor_ptr(#cur, #cur_length, #buffer, #content) *
            writable_memory(#dest, #length, #trash) *
            (ret == FALSE)
    }
*/
bool aws_byte_cursor_read(struct aws_byte_cursor *cur, void *dest,
                          const size_t length) {
    GILLIAN("apply valid_aws_byte_cursor_ptr_facts(#cur, #cur_length, #buffer, #content)");
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
/*@
    spec aws_byte_cursor_read_u8(cur, var) {
        requires:
            (cur == #cur) * (var == #var) *
            valid_aws_byte_cursor_ptr(#cur, #cur_length, #buffer, #content) *
            writable_memory(#var, 1, #trash)

        ensures:
            invalid_read(1, #cur_length) *
            valid_aws_byte_cursor_ptr(#cur, #cur_length, #buffer, #content) *
            writable_memory(#var, 1, #trash) *
            (ret == FALSE);

            valid_read(1, #cur_length) *
            (#content == [ #u ] @ #rest) *
            ARRAY(#buffer, char, 1, [ #u ]) *
            valid_aws_byte_cursor_ptr(#cur, len #rest, #buffer p+ 1, #rest) *
            ARRAY(#var, char, 1, [ #u ]) *
            isByte(#u) *
            (ret == TRUE)
    }
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
/*@
    spec aws_byte_cursor_read_be16(cur, var) {
        requires:
            (cur == #cur) * (var == #var) *
            valid_aws_byte_cursor_ptr(#cur, #cur_length, #buffer, #content) *
            writable_memory(#var, 2, #trash)

        ensures:
            invalid_read(2, #cur_length) *
            valid_aws_byte_cursor_ptr(#cur, #cur_length, #buffer, #content) *
            writable_memory(#var, 2, #trash) *
            (ret == FALSE);

            valid_read(2, #cur_length) *
            (#content == [ #b0, #b1 ] @ #rest) *
            ARRAY(#buffer, char, 2, [ #b0, #b1 ]) *
            valid_aws_byte_cursor_ptr(#cur, len #rest, #buffer p+ 2, #rest) *
            (#read_value == (#b0 * 256) + #b1) *
            isByte(#b0) * isByte(#b1) *
            ARRAY(#var, int16, 1, [ #read_value ]) *
            (ret == TRUE)
    }
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
/*@
    spec aws_byte_cursor_read_be32(cur, var) {
        requires:
            (cur == #cur) * (var == #var) *
            valid_aws_byte_cursor_ptr(#cur, #cur_length, #buffer, #content) *
            writable_memory(#var, 4, #trash)

        ensures:
            invalid_read(4, #cur_length) *
            valid_aws_byte_cursor_ptr(#cur, #cur_length, #buffer, #content) *
            writable_memory(#var, 4, #trash) *
            (ret == FALSE);

            valid_read(4, #cur_length) *
            (#content == [ #b0, #b1, #b2, #b3 ] @ #rest) *
            ARRAY(#buffer, char, 4, [ #b0, #b1, #b2, #b3 ]) *
            (#rest_len == #cur_length - 4) *
            valid_aws_byte_cursor_ptr(#cur, #rest_len, #buffer p+ 4, #rest) *
            (#read_value == (#b0 * 16777216) + (#b1 * 65536) + (#b2 * 256) + #b3) *
            isByte(#b0) * isByte(#b1) * isByte(#b2) * isByte(#b3) *
            (#var -> int(#read_value)) * (ret == TRUE)
    }
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
/*@
    spec aws_byte_cursor_read_and_fill_buffer(cur, dest) {
        requires:
            (cur == #cur) * (dest == #dest) *
            valid_aws_byte_cursor_ptr(#cur, #cur_length, #cur_buffer, #cur_content) *
            valid_aws_byte_buf_ptr(#dest, #dest_length, #dest_capacity, #dest_buffer, #dest_alloc, #dest_content)

        ensures:
            (#dest_capacity == 0) *
            valid_aws_byte_buf_ptr(#dest, 0, 0, NULL, #dest_alloc, []) *
            valid_aws_byte_cursor_ptr(#cur, #cur_length, #cur_buffer, #cur_content) *
            (ret == TRUE);

            (0 <# #dest_capacity) *
            valid_read(#dest_capacity, #cur_length) *
            (#cur_content == #consumed @ #rest) *
            (len #consumed == #dest_capacity) *
            (len #rest == #cur_length - #dest_capacity) *
            valid_aws_byte_buf_ptr(#dest, #dest_capacity, #dest_capacity, #dest_buffer, #dest_alloc, #consumed) *
            valid_aws_byte_cursor_ptr(#cur, len #rest, #cur_buffer p+ #dest_capacity, #rest) *
            ARRAY(#cur_buffer, char, #dest_capacity, #consumed) *
            (ret == TRUE);

            (0 <# #dest_capacity) *
            invalid_read(#dest_capacity, #cur_length) *
            valid_aws_byte_cursor_ptr(#cur, #cur_length, #cur_buffer, #cur_content) *
            valid_aws_byte_buf_ptr(#dest, #dest_length, #dest_capacity, #dest_buffer, #dest_alloc, #dest_content) *
            (ret == FALSE)

    }
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