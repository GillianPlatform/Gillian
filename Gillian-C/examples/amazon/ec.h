#include "allocator.h"
#include "byte_buf.h"
#include "hash_table.h"

void aws_cryptosdk_enc_ctx_clear(struct aws_hash_table *enc_ctx);

int aws_cryptosdk_enc_ctx_deserialize(struct aws_allocator *alloc,
                                      struct aws_hash_table *enc_ctx,
                                      struct aws_byte_cursor *cursor);