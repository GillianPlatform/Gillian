/*
 * Copyright 2018 Amazon.com, Inc. or its affiliates. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"). You may not use
 * this file except in compliance with the License. A copy of the License is
 * located at
 *
 *     http://aws.amazon.com/apache2.0/
 *
 * or in the "license" file accompanying this file. This file is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied. See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef AWS_CRYPTOSDK_PRIVATE_HEADER_H
#define AWS_CRYPTOSDK_PRIVATE_HEADER_H

#include <aws/common/byte_buf.h>
#include <aws/common/hash_table.h>
#include "aws/cryptosdk/header.h"
#include "aws/cryptosdk/materials.h"  // struct aws_cryptosdk_edk

struct aws_cryptosdk_hdr {
    struct aws_allocator *alloc;

    uint16_t alg_id;

    uint32_t frame_len;

    struct aws_byte_buf iv, auth_tag, message_id, alg_suite_data;

    // aws_string * -> aws_string *
    struct aws_hash_table enc_ctx;
    struct aws_array_list edk_list;

    // number of bytes of header except for IV and auth tag,
    // i.e., exactly the bytes that get authenticated
    size_t auth_len;
};

enum aws_cryptosdk_hdr_type {
    // Only one data type is currently defined.
    AWS_CRYPTOSDK_HEADER_TYPE_CUSTOMER_AED = 0x80
};

enum aws_cryptosdk_hdr_content_type {
    AWS_CRYPTOSDK_HEADER_CTYPE_NONFRAMED = 0x01,
    AWS_CRYPTOSDK_HEADER_CTYPE_FRAMED    = 0x02
};

/**
 * Initializes the header datastructure; on return, all fields are zeroed,
 * except for enc_ctx and edk_tbl, which are empty.
 */
int aws_cryptosdk_hdr_init(struct aws_cryptosdk_hdr *hdr, struct aws_allocator *alloc);

/**
 * Frees all memory which has been allocated to hdr object and zeroizes hdr.
 * This method is idempotent - that is, it can safely be called multiple times
 * on the same header without an intervening init; however it cannot be called
 * on an _uninitialized_ header.
 */
void aws_cryptosdk_hdr_clean_up(struct aws_cryptosdk_hdr *hdr);

/**
 * Resets the header to the same state as it would have after hdr_init
 */
void aws_cryptosdk_hdr_clear(struct aws_cryptosdk_hdr *hdr);

/**
 * Reads raw header data from src and populates hdr with all of the information about the
 * message. hdr must have been initialized with aws_cryptosdk_hdr_init.
 *
 * This function will clear the header before parsing, and will leave the header in a cleared
 * state on failure.
 */
int aws_cryptosdk_hdr_parse(struct aws_cryptosdk_hdr *hdr, struct aws_byte_cursor *cursor);

/**
 * Reads information from already parsed hdr object and determines how many bytes are
 * needed to serialize.
 *
 * Returns number of bytes, or zero if hdr was not parsed correctly.
 *
 * Warning: running this on a hdr which has not already been run through
 * aws_cryptosdk_hdr_parse_init (or which has been zeroized) can result in a seg fault.
 */
int aws_cryptosdk_hdr_size(const struct aws_cryptosdk_hdr *hdr);

/**
 * Attempts to write a parsed header.
 *
 * The number of bytes written to outbuf is placed at *bytes_written. If outbuf is too
 * small, returns AWS_OP_ERR, sets the error code to AWS_ERROR_SHORT_BUFFER, and zeroizes
 * the output buffer.
 *
 * Using aws_cryptosdk_hdr_size to determine how much memory to allocate to outbuf ahead
 * of time guarantees that the short buffer error will not occur.
 */
int aws_cryptosdk_hdr_write(const struct aws_cryptosdk_hdr *hdr, size_t *bytes_written, uint8_t *outbuf, size_t outlen);

/**
 * Returns number of bytes in auth tag for known algorithms, -1 for unknown algorithms.
 */
int aws_cryptosdk_private_algorithm_taglen(uint16_t alg_id);

/**
 * Returns number of bytes in IV for known algorithms, -1 for unknown algorithms.
 */
int aws_cryptosdk_private_algorithm_ivlen(uint16_t alg_id);

/**
 * Returns the total length of statically sized fields for known header
 * versions, or -1 for unknown header versions.
 */
int aws_cryptosdk_private_header_version_static_fields_len(uint8_t header_version);

/**
 * Returns true for known algorithms, or false for unknown algorithms.
 */
bool aws_cryptosdk_algorithm_is_known(uint16_t alg_id);

/**
 * Returns true for key-committing algorithms, or false otherwise.
 */
bool aws_cryptosdk_algorithm_is_committing(uint16_t alg_id);

#endif  // AWS_CRYPTOSDK_PRIVATE_HEADER_H