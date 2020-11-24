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
// #include <aws/common/byte_buf.h>
// #include <aws/common/math.h>
// #include <aws/common/string.h>
// #include <aws/cryptosdk/cipher.h>
// #include <aws/cryptosdk/edk.h>
// #include <aws/cryptosdk/error.h>
// #include <aws/cryptosdk/private/cipher.h>
// #include <aws/cryptosdk/private/compiler.h>
// #include <aws/cryptosdk/private/enc_ctx.h>
// #include <aws/cryptosdk/private/header.h>
#include <string.h>  // memcpy
#include "header.h"

bool aws_cryptosdk_algorithm_is_known(uint16_t alg_id) {
    switch (alg_id) {
        case ALG_AES256_GCM_HKDF_SHA512_COMMIT_KEY:
        case ALG_AES256_GCM_HKDF_SHA512_COMMIT_KEY_ECDSA_P384:
        case ALG_AES256_GCM_IV12_TAG16_HKDF_SHA384_ECDSA_P384:
        case ALG_AES192_GCM_IV12_TAG16_HKDF_SHA384_ECDSA_P384:
        case ALG_AES128_GCM_IV12_TAG16_HKDF_SHA256_ECDSA_P256:
        case ALG_AES256_GCM_IV12_TAG16_HKDF_SHA256:
        case ALG_AES192_GCM_IV12_TAG16_HKDF_SHA256:
        case ALG_AES128_GCM_IV12_TAG16_HKDF_SHA256:
        case ALG_AES256_GCM_IV12_TAG16_NO_KDF:
        case ALG_AES192_GCM_IV12_TAG16_NO_KDF:
        case ALG_AES128_GCM_IV12_TAG16_NO_KDF: return true;
        default: return false;
    }
}

static int aws_cryptosdk_header_version_is_known(uint8_t header_version) {
    switch (header_version) {
        case AWS_CRYPTOSDK_HEADER_VERSION_1_0:
        case AWS_CRYPTOSDK_HEADER_VERSION_2_0: return 1;
        default: return 0;
    }
}

int aws_cryptosdk_private_algorithm_taglen(uint16_t alg_id) {
    if (aws_cryptosdk_algorithm_is_known(alg_id)) {
        // all known algorithms have a tag length of 16 bytes
        return 16;
    }

    return -1;
}

int aws_cryptosdk_private_algorithm_ivlen(uint16_t alg_id) {
    if (aws_cryptosdk_algorithm_is_known(alg_id)) {
        // all known algorithms have an IV length of 12 bytes
        return 12;
    }

    return -1;
}

int aws_cryptosdk_header_version_static_fields_len(uint8_t header_version) {
    switch (header_version) {
        case AWS_CRYPTOSDK_HEADER_VERSION_1_0:
            return 1    // version
                   + 1  // type
                   + 2  // alg ID
                   + 2  // AAD length
                   + 2  // EDK count
                   + 1  // content type
                   + 4  // reserved
                   + 1  // IV length
                   + 4  // frame length
                ;
        case AWS_CRYPTOSDK_HEADER_VERSION_2_0:
            return 1    // version
                   + 2  // alg ID
                   + 2  // AAD length
                   + 2  // EDK count
                   + 1  // content type
                   + 4  // frame length
                ;
        default: return -1;
    }
}

static int is_known_type(uint8_t content_type) {
    switch (content_type) {
        case AWS_CRYPTOSDK_HEADER_CTYPE_FRAMED:
        case AWS_CRYPTOSDK_HEADER_CTYPE_NONFRAMED: return 1;
        default: return 0;
    }
}

bool aws_cryptosdk_algorithm_is_committing(uint16_t alg_id) {
    switch (alg_id) {
        case ALG_AES256_GCM_HKDF_SHA512_COMMIT_KEY:
        case ALG_AES256_GCM_HKDF_SHA512_COMMIT_KEY_ECDSA_P384: return true;
        default: return false;
    }
}

int aws_cryptosdk_hdr_init(struct aws_cryptosdk_hdr *hdr, struct aws_allocator *alloc) {
    aws_secure_zero(hdr, sizeof(*hdr));

    if (aws_cryptosdk_enc_ctx_init(alloc, &hdr->enc_ctx)) {
        return AWS_OP_ERR;
    }

    if (aws_cryptosdk_edk_list_init(alloc, &hdr->edk_list)) {
        aws_cryptosdk_enc_ctx_clean_up(&hdr->enc_ctx);
        return AWS_OP_ERR;
    }

    hdr->alloc = alloc;

    return AWS_OP_SUCCESS;
}

void aws_cryptosdk_hdr_clear(struct aws_cryptosdk_hdr *hdr) {
    /* hdr->alloc is preserved */
    hdr->alg_id    = 0;
    hdr->frame_len = 0;

    aws_byte_buf_clean_up(&hdr->iv);
    aws_byte_buf_clean_up(&hdr->auth_tag);
    aws_byte_buf_clean_up(&hdr->message_id);
    aws_byte_buf_clean_up(&hdr->alg_suite_data);

    aws_cryptosdk_edk_list_clear(&hdr->edk_list);
    aws_cryptosdk_enc_ctx_clear(&hdr->enc_ctx);

    hdr->auth_len = 0;
}

void aws_cryptosdk_hdr_clean_up(struct aws_cryptosdk_hdr *hdr) {
    if (!hdr->alloc) {
        // Idempotent cleanup
        return;
    }
    if (hdr->iv.allocator) aws_byte_buf_clean_up(&hdr->iv);
    if (hdr->auth_tag.allocator) aws_byte_buf_clean_up(&hdr->auth_tag);
    if (hdr->message_id.allocator) aws_byte_buf_clean_up(&hdr->message_id);
    if (hdr->alg_suite_data.allocator) aws_byte_buf_clean_up(&hdr->alg_suite_data);

    aws_cryptosdk_edk_list_clean_up(&hdr->edk_list);
    aws_cryptosdk_enc_ctx_clean_up(&hdr->enc_ctx);

    aws_secure_zero(hdr, sizeof(*hdr));
}

static inline int parse_edk(
    struct aws_allocator *allocator, struct aws_cryptosdk_edk *edk, struct aws_byte_cursor *cur) {
    uint16_t field_len;

    memset(edk, 0, sizeof(*edk));

    if (!aws_byte_cursor_read_be16(cur, &field_len)) goto SHORT_BUF;
    if (aws_byte_buf_init(&edk->provider_id, allocator, field_len)) goto MEM_ERR;
    if (!aws_byte_cursor_read_and_fill_buffer(cur, &edk->provider_id)) goto SHORT_BUF;

    if (!aws_byte_cursor_read_be16(cur, &field_len)) goto SHORT_BUF;
    if (aws_byte_buf_init(&edk->provider_info, allocator, field_len)) goto MEM_ERR;
    if (!aws_byte_cursor_read_and_fill_buffer(cur, &edk->provider_info)) goto SHORT_BUF;

    if (!aws_byte_cursor_read_be16(cur, &field_len)) goto SHORT_BUF;
    if (aws_byte_buf_init(&edk->ciphertext, allocator, field_len)) goto MEM_ERR;
    if (!aws_byte_cursor_read_and_fill_buffer(cur, &edk->ciphertext)) goto SHORT_BUF;

    return AWS_OP_SUCCESS;

SHORT_BUF:
    aws_cryptosdk_edk_clean_up(edk);
    return aws_raise_error(AWS_ERROR_SHORT_BUFFER);
MEM_ERR:
    aws_cryptosdk_edk_clean_up(edk);
    // The _init function should have already raised an AWS_ERROR_OOM
    return AWS_OP_ERR;
}

int aws_cryptosdk_hdr_parse(struct aws_cryptosdk_hdr *hdr, struct aws_byte_cursor *pcursor) {
    struct aws_byte_cursor cur = *pcursor;

    aws_cryptosdk_hdr_clear(hdr);

    uint8_t header_version;
    if (!aws_byte_cursor_read_u8(&cur, &header_version)) goto SHORT_BUF;
    if (aws_cryptosdk_unlikely(!aws_cryptosdk_header_version_is_known(header_version))) goto PARSE_ERR;

    if (header_version == AWS_CRYPTOSDK_HEADER_VERSION_1_0) {
        uint8_t message_type;
        if (!aws_byte_cursor_read_u8(&cur, &message_type)) goto SHORT_BUF;
        if (aws_cryptosdk_unlikely(message_type != AWS_CRYPTOSDK_HEADER_TYPE_CUSTOMER_AED)) goto PARSE_ERR;
    }

    uint16_t alg_id;
    if (!aws_byte_cursor_read_be16(&cur, &alg_id)) goto SHORT_BUF;
    if (aws_cryptosdk_unlikely(!aws_cryptosdk_algorithm_is_known(alg_id))) goto PARSE_ERR;
    const struct aws_cryptosdk_alg_properties *alg_props = aws_cryptosdk_alg_props(alg_id);
    // Prevent header format confusion in case it's inconsistent with alg ID
    if (aws_cryptosdk_unlikely(alg_props->msg_format_version != header_version)) goto PARSE_ERR;
    hdr->alg_id = alg_id;

    size_t message_id_len = aws_cryptosdk_private_algorithm_message_id_len(alg_props);
    if (aws_byte_buf_init(&hdr->message_id, hdr->alloc, message_id_len)) goto MEM_ERR;
    if (!aws_byte_cursor_read_and_fill_buffer(&cur, &hdr->message_id)) goto SHORT_BUF;

    uint16_t aad_len;
    if (!aws_byte_cursor_read_be16(&cur, &aad_len)) goto SHORT_BUF;

    if (aad_len) {
        struct aws_byte_cursor aad = aws_byte_cursor_advance_nospec(&cur, aad_len);

        // Note that, even if this fails with SHORT_BUF, we report a parse error, since we know we
        // have enough data (according to the aad length field).
        if (aws_cryptosdk_enc_ctx_deserialize(hdr->alloc, &hdr->enc_ctx, &aad)) goto PARSE_ERR;
        if (aad.len) {
            // trailing garbage after the aad block
            goto PARSE_ERR;
        }
    }

    uint16_t edk_count;
    if (!aws_byte_cursor_read_be16(&cur, &edk_count)) goto SHORT_BUF;
    if (!edk_count) goto PARSE_ERR;

    for (uint16_t i = 0; i < edk_count; ++i) {
        struct aws_cryptosdk_edk edk;

        if (parse_edk(hdr->alloc, &edk, &cur)) {
            goto RETHROW;
        }

        aws_array_list_push_back(&hdr->edk_list, &edk);
    }

    uint8_t content_type;
    if (!aws_byte_cursor_read_u8(&cur, &content_type)) goto SHORT_BUF;

    if (aws_cryptosdk_unlikely(!is_known_type(content_type))) goto PARSE_ERR;

    uint32_t reserved;  // must be zero
    uint8_t iv_len;
    if (header_version == AWS_CRYPTOSDK_HEADER_VERSION_1_0) {
        if (!aws_byte_cursor_read_be32(&cur, &reserved)) goto SHORT_BUF;
        if (reserved) goto PARSE_ERR;

        if (!aws_byte_cursor_read_u8(&cur, &iv_len)) goto SHORT_BUF;
        if (iv_len != aws_cryptosdk_private_algorithm_ivlen(alg_id)) goto PARSE_ERR;
    }

    uint32_t frame_len;
    if (!aws_byte_cursor_read_be32(&cur, &frame_len)) goto SHORT_BUF;

    if ((content_type == AWS_CRYPTOSDK_HEADER_CTYPE_NONFRAMED && frame_len != 0) ||
        (content_type == AWS_CRYPTOSDK_HEADER_CTYPE_FRAMED && frame_len == 0))
        goto PARSE_ERR;
    hdr->frame_len = frame_len;

    if (header_version == AWS_CRYPTOSDK_HEADER_VERSION_2_0) {
        const struct aws_cryptosdk_alg_properties *alg_props = aws_cryptosdk_alg_props(alg_id);
        if (alg_props->alg_suite_data_len) {
            if (aws_byte_buf_init(&hdr->alg_suite_data, hdr->alloc, alg_props->alg_suite_data_len)) goto MEM_ERR;
            if (!aws_byte_cursor_read_and_fill_buffer(&cur, &hdr->alg_suite_data)) goto SHORT_BUF;
        }
    }

    // cur.ptr now points to end of portion of header that is authenticated
    hdr->auth_len = cur.ptr - pcursor->ptr;

    if (header_version == AWS_CRYPTOSDK_HEADER_VERSION_1_0) {
        if (aws_byte_buf_init(&hdr->iv, hdr->alloc, iv_len)) goto MEM_ERR;
        if (!aws_byte_cursor_read_and_fill_buffer(&cur, &hdr->iv)) goto SHORT_BUF;
    }

    size_t tag_len = aws_cryptosdk_private_algorithm_taglen(alg_id);
    if (aws_byte_buf_init(&hdr->auth_tag, hdr->alloc, tag_len)) goto MEM_ERR;
    if (!aws_byte_cursor_read_and_fill_buffer(&cur, &hdr->auth_tag)) goto SHORT_BUF;

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
    return AWS_OP_ERR;  // Error code will already have been raised in aws_mem_acquire
}

/*
 * Declaring a struct which is initialized to zero does not technically guarantee that the
 * padding bytes will all be zero, according to the C spec, though in practice they generally
 * are. Since we are comparing all of the bytes of the struct, using this union guarantees
 * that even the padding bytes will be zeroes in zero.hdr. It also allows us to fetch an
 * arbitrary array of zero.bytes up to the length of the struct.
 */
static const union {
    uint8_t bytes[sizeof(struct aws_cryptosdk_hdr)];
    struct aws_cryptosdk_hdr hdr;
} zero = { { 0 } };

static size_t saturating_add(size_t a, size_t b) {
    size_t c = a + b;
    if (c < a) {
        c = SIZE_MAX;
    }
    return c;
}

int aws_cryptosdk_hdr_size(const struct aws_cryptosdk_hdr *hdr) {
    if (!memcmp(hdr, &zero.hdr, sizeof(struct aws_cryptosdk_hdr))) return 0;
    const struct aws_cryptosdk_alg_properties *alg_props = aws_cryptosdk_alg_props(hdr->alg_id);
    if (!alg_props) return 0;
    int static_fields_len = aws_cryptosdk_header_version_static_fields_len(alg_props->msg_format_version);
    if (static_fields_len == -1) return 0;

    size_t idx;
    size_t edk_count          = aws_array_list_length(&hdr->edk_list);
    size_t dynamic_fields_len = hdr->message_id.len + hdr->alg_suite_data.len;
    size_t authtag_len        = aws_cryptosdk_private_authtag_len(alg_props);
    size_t bytes              = static_fields_len + dynamic_fields_len + authtag_len;
    size_t aad_len;

    if (aws_cryptosdk_enc_ctx_size(&aad_len, &hdr->enc_ctx)) {
        return 0;
    }
    bytes += aad_len;

    for (idx = 0; idx < edk_count; ++idx) {
        void *vp_edk = NULL;
        struct aws_cryptosdk_edk *edk;

        aws_array_list_get_at_ptr(&hdr->edk_list, &vp_edk, idx);
        assert(vp_edk);

        edk = vp_edk;
        // 2 bytes for each field's length header * 3 fields
        bytes = saturating_add(bytes, 6);
        bytes = saturating_add(bytes, edk->provider_id.len);
        bytes = saturating_add(bytes, edk->provider_info.len);
        bytes = saturating_add(bytes, edk->ciphertext.len);
    }

    return bytes == SIZE_MAX ? 0 : bytes;
}
static void init_aws_byte_buf_raw(struct aws_byte_buf *buf) {
    buf->allocator = NULL;
    buf->buffer    = NULL;
    buf->len       = 0;
    buf->capacity  = 0;
}
int aws_cryptosdk_hdr_write(
    const struct aws_cryptosdk_hdr *hdr, size_t *bytes_written, uint8_t *outbuf, size_t outlen) {
    struct aws_byte_buf output = aws_byte_buf_from_array(outbuf, outlen);
    output.len                 = 0;

    const struct aws_cryptosdk_alg_properties *alg_props = aws_cryptosdk_alg_props(hdr->alg_id);
    if (!alg_props) goto INVALID_HEADER;
    enum aws_cryptosdk_hdr_version header_version = alg_props->msg_format_version;

    if (!aws_byte_buf_write_u8(&output, header_version)) goto WRITE_ERR;
    if (header_version == AWS_CRYPTOSDK_HEADER_VERSION_1_0) {
        if (!aws_byte_buf_write_u8(&output, AWS_CRYPTOSDK_HEADER_TYPE_CUSTOMER_AED)) goto WRITE_ERR;
    }
    if (!aws_byte_buf_write_be16(&output, hdr->alg_id)) goto WRITE_ERR;
    if (!aws_byte_buf_write_from_whole_cursor(
            &output, aws_byte_cursor_from_array(hdr->message_id.buffer, hdr->message_id.len)))
        goto WRITE_ERR;

    // TODO - unify everything on byte_bufs when the aws-c-common refactor lands
    // See: https://github.com/awslabs/aws-c-common/pull/130
    struct aws_byte_buf aad_length_field;
    init_aws_byte_buf_raw(&aad_length_field);

    if (!aws_byte_buf_advance(&output, &aad_length_field, 2)) goto WRITE_ERR;

    size_t old_len = output.len;
    if (aws_cryptosdk_enc_ctx_serialize(aws_default_allocator(), &output, &hdr->enc_ctx)) goto WRITE_ERR;

    if (!aws_byte_buf_write_be16(&aad_length_field, (uint16_t)(output.len - old_len))) goto WRITE_ERR;

    size_t edk_count = aws_array_list_length(&hdr->edk_list);
    if (!aws_byte_buf_write_be16(&output, (uint16_t)edk_count)) goto WRITE_ERR;

    for (size_t idx = 0; idx < edk_count; ++idx) {
        void *vp_edk = NULL;

        aws_array_list_get_at_ptr(&hdr->edk_list, &vp_edk, idx);
        assert(vp_edk);

        const struct aws_cryptosdk_edk *edk = vp_edk;

        if (!aws_byte_buf_write_be16(&output, (uint16_t)edk->provider_id.len)) goto WRITE_ERR;
        if (!aws_byte_buf_write_from_whole_cursor(
                &output, aws_byte_cursor_from_array(edk->provider_id.buffer, edk->provider_id.len)))
            goto WRITE_ERR;

        if (!aws_byte_buf_write_be16(&output, (uint16_t)edk->provider_info.len)) goto WRITE_ERR;
        if (!aws_byte_buf_write_from_whole_cursor(
                &output, aws_byte_cursor_from_array(edk->provider_info.buffer, edk->provider_info.len)))
            goto WRITE_ERR;

        if (!aws_byte_buf_write_be16(&output, (uint16_t)edk->ciphertext.len)) goto WRITE_ERR;
        if (!aws_byte_buf_write_from_whole_cursor(
                &output, aws_byte_cursor_from_array(edk->ciphertext.buffer, edk->ciphertext.len)))
            goto WRITE_ERR;
    }

    if (!aws_byte_buf_write_u8(
            &output, hdr->frame_len ? AWS_CRYPTOSDK_HEADER_CTYPE_FRAMED : AWS_CRYPTOSDK_HEADER_CTYPE_NONFRAMED))
        goto WRITE_ERR;

    if (header_version == AWS_CRYPTOSDK_HEADER_VERSION_1_0) {
        if (!aws_byte_buf_write(&output, zero.bytes, 4)) goto WRITE_ERR;  // v01 reserved field
        if (!aws_byte_buf_write_u8(&output, (uint8_t)hdr->iv.len)) goto WRITE_ERR;
    }

    if (!aws_byte_buf_write_be32(&output, hdr->frame_len)) goto WRITE_ERR;

    if (!aws_byte_buf_write_from_whole_cursor(
            &output, aws_byte_cursor_from_array(hdr->alg_suite_data.buffer, hdr->alg_suite_data.len)))
        goto WRITE_ERR;

    if (header_version == AWS_CRYPTOSDK_HEADER_VERSION_1_0) {
        if (!aws_byte_buf_write_from_whole_cursor(&output, aws_byte_cursor_from_array(hdr->iv.buffer, hdr->iv.len)))
            goto WRITE_ERR;
    }
    if (!aws_byte_buf_write_from_whole_cursor(
            &output, aws_byte_cursor_from_array(hdr->auth_tag.buffer, hdr->auth_tag.len)))
        goto WRITE_ERR;

    *bytes_written = output.len;
    return AWS_OP_SUCCESS;

INVALID_HEADER:
    aws_secure_zero(outbuf, outlen);
    *bytes_written = 0;
    return aws_raise_error(AWS_CRYPTOSDK_ERR_BAD_STATE);

WRITE_ERR:
    aws_secure_zero(outbuf, outlen);
    *bytes_written = 0;
    return aws_raise_error(AWS_ERROR_SHORT_BUFFER);
}