/*
 * Copyright 2018 Amazon.com, Inc. or its affiliates. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"). You may not
 * use this file except in compliance with the License. A copy of the License is
 * located at
 *
 *     http://aws.amazon.com/apache2.0/
 *
 * or in the "license" file accompanying this file. This file is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
 * express or implied. See the License for the specific language governing
 * permissions and limitations under the License.
 */

#include "allocator.h"
#include "array_list.h"
#include "byte_buf.h"
#include "ec.h"
#include "edk.h"
#include "error.h"
#include "hash_table.h"
#include "base.h"
#include <stdint.h>
#include <stdlib.h>

#define MESSAGE_ID_LEN 16

/**
 * @ingroup session
 * Known algorithm suite names.
 * For more information, see the <a
 * href="https://docs.aws.amazon.com/encryption-sdk/latest/developer-guide/algorithms-reference.html">Algorithms
 * Reference</a>.
 */
enum aws_cryptosdk_alg_id {
    ALG_AES256_GCM_HKDF_SHA512_COMMIT_KEY_ECDSA_P384 = 0x0578,
    ALG_AES256_GCM_HKDF_SHA512_COMMIT_KEY = 0x0478,
    ALG_AES256_GCM_IV12_TAG16_HKDF_SHA384_ECDSA_P384 = 0x0378,
    ALG_AES192_GCM_IV12_TAG16_HKDF_SHA384_ECDSA_P384 = 0x0346,
    ALG_AES128_GCM_IV12_TAG16_HKDF_SHA256_ECDSA_P256 = 0x0214,
    ALG_AES256_GCM_IV12_TAG16_HKDF_SHA256 = 0x0178,
    ALG_AES192_GCM_IV12_TAG16_HKDF_SHA256 = 0x0146,
    ALG_AES128_GCM_IV12_TAG16_HKDF_SHA256 = 0x0114,
    ALG_AES256_GCM_IV12_TAG16_NO_KDF = 0x0078,
    ALG_AES192_GCM_IV12_TAG16_NO_KDF = 0x0046,
    ALG_AES128_GCM_IV12_TAG16_NO_KDF = 0x0014
};

enum aws_cryptosdk_hdr_content_type {
    AWS_CRYPTOSDK_HEADER_CTYPE_NONFRAMED = 0x01,
    AWS_CRYPTOSDK_HEADER_CTYPE_FRAMED = 0x02
};

enum aws_cryptosdk_hdr_type {
    // Only one data type is currently defined.
    AWS_CRYPTOSDK_HEADER_TYPE_CUSTOMER_AED = 0x80
};

enum aws_cryptosdk_hdr_version {
    AWS_CRYPTOSDK_HEADER_VERSION_1_0 = 0x01,
};

struct aws_cryptosdk_hdr {
    struct aws_allocator *alloc;

    uint16_t alg_id;

    uint32_t frame_len;

    struct aws_byte_buf iv, auth_tag;

    uint8_t message_id[MESSAGE_ID_LEN];

    // aws_string * -> aws_string *
    struct aws_hash_table enc_ctx;
    struct aws_array_list edk_list;

    // number of bytes of header except for IV and auth tag,
    // i.e., exactly the bytes that get authenticated
    size_t auth_len;
};

int aws_cryptosdk_hdr_parse(struct aws_cryptosdk_hdr *hdr,
                            struct aws_byte_cursor *pcursor);
int aws_cryptosdk_enc_ctx_deserialize(struct aws_allocator *alloc,
                                      struct aws_hash_table *enc_ctx,
                                      struct aws_byte_cursor *cursor);