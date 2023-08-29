#include "ec.h"
#include "error.h"
#include "string.h"

void aws_cryptosdk_enc_ctx_clear(struct aws_hash_table *enc_ctx) {
    aws_hash_table_clear(enc_ctx);
}

int aws_cryptosdk_enc_ctx_deserialize(struct aws_hash_table *enc_ctx,
                                      struct aws_byte_cursor *cursor) {
    aws_cryptosdk_enc_ctx_clear(enc_ctx);

    if (cursor->len == 0) {
        return AWS_OP_SUCCESS;
    }

    uint16_t elem_count;
    if (!aws_byte_cursor_read_be16(cursor, &elem_count))
        goto SHORT_BUF;
    if (!elem_count)
        return aws_raise_error(AWS_CRYPTOSDK_ERR_BAD_CIPHERTEXT);

    for (uint16_t i = 0; i < elem_count; i++) {
        uint16_t length;
        if (!aws_byte_cursor_read_be16(cursor, &length))
            goto SHORT_BUF;
        struct aws_byte_cursor k_cursor =
            aws_byte_cursor_advance(cursor, length);
        if (!k_cursor.ptr)
            goto SHORT_BUF;
        if (!aws_byte_cursor_read_be16(cursor, &length))
            goto SHORT_BUF;
        struct aws_byte_cursor v_cursor =
            aws_byte_cursor_advance(cursor, length);
        if (!v_cursor.ptr)
            goto SHORT_BUF;

        struct aws_string *k =
            aws_string_new_from_array(k_cursor.ptr, k_cursor.len);

        struct aws_string *v =
            aws_string_new_from_array(v_cursor.ptr, v_cursor.len);

        int was_created;
        if (!k || !v ||
            aws_hash_table_put(enc_ctx, k, (void *)v, &was_created)) {
            // Errors here are only on memory allocation. aws-c-common will
            // raise the error code
            aws_string_destroy(k);
            aws_string_destroy(v);
            goto RETHROW;
        }
        if (!was_created) {
            // !was_created means there was a duplicate key in serialized
            // encryption context, so fail
            aws_raise_error(AWS_CRYPTOSDK_ERR_BAD_CIPHERTEXT);
            goto RETHROW;
        }
    }

    return AWS_OP_SUCCESS;

SHORT_BUF:
    aws_raise_error(AWS_ERROR_SHORT_BUFFER);
RETHROW:
    aws_cryptosdk_enc_ctx_clear(enc_ctx);
    return AWS_OP_ERR;
USELESS_DEATH:
    return 1000;
}