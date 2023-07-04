/*
 * Copyright 2018 Amazon.com, Inc. or its affiliates. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 20 (the "License"). You may not
 * use this file except in compliance with the License. A copy of the License is
 * located at
 *
 *     http://aws.amazon.com/apache20/
 *
 * or in the "license" file accompanying this file. This file is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
 * express or implied. See the License for the specific language governing
 * permissions and limitations under the License.
 */

/*
 This file corresponds to the following commit of the encryption sdk:
 https://github.com/aws/aws-encryption-sdk-c/tree/22151f59f1a3128ecef877ddc491c20f6754af4e
*/

#include "header.h"
#include "base.h"

int is_known_type(uint8_t content_type) {
    switch (content_type) {
    case AWS_CRYPTOSDK_HEADER_CTYPE_FRAMED:
    case AWS_CRYPTOSDK_HEADER_CTYPE_NONFRAMED:
        return 1;
    default:
        return 0;
    }
}

int aws_cryptosdk_algorithm_is_known(uint16_t alg_id) {
    switch (alg_id) {
    case ALG_AES256_GCM_HKDF_SHA512_COMMIT_KEY_ECDSA_P384:
    case ALG_AES256_GCM_HKDF_SHA512_COMMIT_KEY:
    case ALG_AES256_GCM_IV12_TAG16_HKDF_SHA384_ECDSA_P384:
    case ALG_AES192_GCM_IV12_TAG16_HKDF_SHA384_ECDSA_P384:
    case ALG_AES128_GCM_IV12_TAG16_HKDF_SHA256_ECDSA_P256:
    case ALG_AES256_GCM_IV12_TAG16_HKDF_SHA256:
    case ALG_AES192_GCM_IV12_TAG16_HKDF_SHA256:
    case ALG_AES128_GCM_IV12_TAG16_HKDF_SHA256:
    case ALG_AES256_GCM_IV12_TAG16_NO_KDF:
    case ALG_AES192_GCM_IV12_TAG16_NO_KDF:
    case ALG_AES128_GCM_IV12_TAG16_NO_KDF:
        return 1;
    default:
        return 0;
    }
}

int aws_cryptosdk_algorithm_ivlen(uint16_t alg_id) {
    if (aws_cryptosdk_algorithm_is_known(alg_id)) {
        // all known algorithms have an IV length of 12 bytes
        return 12;
    }

    return -1;
}

int aws_cryptosdk_algorithm_taglen(uint16_t alg_id) {
    if (aws_cryptosdk_algorithm_is_known(alg_id)) {
        // all known algorithms have a tag length of 16 bytes
        return 16;
    }

    return -1;
}

void aws_cryptosdk_hdr_clear(struct aws_cryptosdk_hdr *hdr) {
    hdr->alg_id = 0;
    hdr->frame_len = 0;

    aws_byte_buf_clean_up(&hdr->iv);
    aws_byte_buf_clean_up(&hdr->auth_tag);

    memset(&hdr->message_id, 0, sizeof(hdr->message_id));

    aws_cryptosdk_enc_ctx_clear(&hdr->enc_ctx);
    aws_cryptosdk_edk_list_clear(&hdr->edk_list);

    hdr->auth_len = 0;
}

int parse_edk(struct aws_cryptosdk_edk *edk, struct aws_byte_cursor *cur) {

    uint16_t field_len;

    memset(edk, 0, sizeof(*edk));

    if (!aws_byte_cursor_read_be16(cur, &field_len))
        goto SHORT_BUF;
    if (aws_byte_buf_init(&edk->provider_id, field_len))
        goto MEM_ERR;
    if (!aws_byte_cursor_read_and_fill_buffer(cur, &edk->provider_id))
        goto SHORT_BUF;

    if (!aws_byte_cursor_read_be16(cur, &field_len))
        goto SHORT_BUF;
    if (aws_byte_buf_init(&edk->provider_info, field_len))
        goto MEM_ERR;
    if (!aws_byte_cursor_read_and_fill_buffer(cur, &edk->provider_info))
        goto SHORT_BUF;

    if (!aws_byte_cursor_read_be16(cur, &field_len))
        goto SHORT_BUF;
    if (aws_byte_buf_init(&edk->ciphertext, field_len))
        goto MEM_ERR;
    if (!aws_byte_cursor_read_and_fill_buffer(cur, &edk->ciphertext))
        goto SHORT_BUF;

    return AWS_OP_SUCCESS;

SHORT_BUF:
    aws_cryptosdk_edk_clean_up(edk);
    return aws_raise_error(AWS_ERROR_SHORT_BUFFER);

MEM_ERR:
    aws_cryptosdk_edk_clean_up(edk);
    // The _init function should have already raised an AWS_ERROR_OOM
    return AWS_OP_ERR;
}

int aws_cryptosdk_hdr_parse(struct aws_cryptosdk_hdr *hdr,
                            struct aws_byte_cursor *pcursor) {

    struct aws_byte_cursor cur = *pcursor;

    aws_cryptosdk_hdr_clear(hdr);

    uint8_t bytefield;

    if (!aws_byte_cursor_read_u8(&cur, &bytefield)) {
        goto SHORT_BUF;
    }

    if (aws_cryptosdk_unlikely(bytefield != AWS_CRYPTOSDK_HEADER_VERSION_1_0))
        goto PARSE_ERR;

    if (!aws_byte_cursor_read_u8(&cur, &bytefield)) {
        goto SHORT_BUF;
    }

    if (aws_cryptosdk_unlikely(bytefield !=
                               AWS_CRYPTOSDK_HEADER_TYPE_CUSTOMER_AED))
        goto PARSE_ERR;

    uint16_t alg_id;
    if (!aws_byte_cursor_read_be16(&cur, &alg_id)) {
        goto SHORT_BUF;
    }

    if (aws_cryptosdk_unlikely(!aws_cryptosdk_algorithm_is_known(alg_id))) {
        goto PARSE_ERR;
    }

    hdr->alg_id = alg_id;

    if (!aws_byte_cursor_read(&cur, hdr->message_id, MESSAGE_ID_LEN)) {
        goto SHORT_BUF;
    }

    uint16_t aad_len;
    if (!aws_byte_cursor_read_be16(&cur, &aad_len)) {
        goto SHORT_BUF;
    }

    if (aad_len) {
        struct aws_byte_cursor aad = aws_byte_cursor_advance(&cur, aad_len);
        if (!aad.ptr)
            goto SHORT_BUF;
        // Note that, even if this fails with SHORT_BUF, we report a parse
        // error, since we know we have enough data (according to the aad
        // length field).
        if (aws_cryptosdk_enc_ctx_deserialize(&hdr->enc_ctx, &aad))
            goto PARSE_ERR;
        if (aad.len) {
            // trailing garbage after the aad block
            goto PARSE_ERR;
        }
    };
    uint16_t edk_count;
    if (!aws_byte_cursor_read_be16(&cur, &edk_count))
        goto SHORT_BUF;
    if (!edk_count)
        goto PARSE_ERR;

    for (uint16_t i = 0; i < edk_count; ++i) {
        struct aws_cryptosdk_edk edk;

        if (parse_edk(&edk, &cur)) {
            goto RETHROW;
        }

        aws_array_list_push_back(&hdr->edk_list, &edk);
    }

    uint8_t content_type;
    if (!aws_byte_cursor_read_u8(&cur, &content_type))
        goto SHORT_BUF;

    if (aws_cryptosdk_unlikely(!is_known_type(content_type)))
        goto PARSE_ERR;

    uint32_t reserved; // must be zero
    if (!aws_byte_cursor_read_be32(&cur, &reserved))
        goto SHORT_BUF;
    if (reserved)
        goto PARSE_ERR;

    uint8_t iv_len;
    if (!aws_byte_cursor_read_u8(&cur, &iv_len))
        goto SHORT_BUF;

    if (iv_len != aws_cryptosdk_algorithm_ivlen(alg_id))
        goto PARSE_ERR;

    uint32_t frame_len;
    if (!aws_byte_cursor_read_be32(&cur, &frame_len))
        goto SHORT_BUF;

    if ((content_type == AWS_CRYPTOSDK_HEADER_CTYPE_NONFRAMED &&
         frame_len != 0) ||
        (content_type == AWS_CRYPTOSDK_HEADER_CTYPE_FRAMED && frame_len == 0))
        goto PARSE_ERR;

    hdr->frame_len = frame_len;

    // cur.ptr now points to end of portion of header that is authenticated
    hdr->auth_len = cur.ptr - pcursor->ptr;

    if (aws_byte_buf_init(&hdr->iv, iv_len))
        goto MEM_ERR;
    if (!aws_byte_cursor_read_and_fill_buffer(&cur, &hdr->iv))
        goto SHORT_BUF;

    size_t tag_len = aws_cryptosdk_algorithm_taglen(alg_id);
    if (aws_byte_buf_init(&hdr->auth_tag, tag_len))
        goto MEM_ERR;
    if (!aws_byte_cursor_read_and_fill_buffer(&cur, &hdr->auth_tag))
        goto SHORT_BUF;

    *pcursor = cur;

    return AWS_OP_SUCCESS;

SHORT_BUF:
    aws_cryptosdk_hdr_clear(hdr);
    return aws_raise_error(AWS_ERROR_SHORT_BUFFER);

PARSE_ERR:

    aws_cryptosdk_hdr_clear(hdr);
    return aws_raise_error(AWS_CRYPTOSDK_ERR_BAD_CIPHERTEXT);

MEM_ERR:
RETHROW:

    aws_cryptosdk_hdr_clear(hdr);
    return AWS_OP_ERR;
}