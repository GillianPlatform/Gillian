/**
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0.
 */
#ifndef AWS_COMMON_STRING_H
#define AWS_COMMON_STRING_H
/**
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0.
 */

#include <stdlib.h>

#define AWS_OP_SUCCESS (0)
#define AWS_OP_ERR (-1)

/* We override & hardcode the values, because the spec comments don't get overriden by the preprocessor.
  We also reduce the amount of variables.
*/
enum aws_common_error {
    AWS_ERROR_SUCCESS = 0,
    AWS_ERROR_OOM = 1,
    AWS_ERROR_UNKNOWN = 2,
    AWS_ERROR_SHORT_BUFFER = 3,
    AWS_ERROR_OVERFLOW_DETECTED = 4,
    AWS_ERROR_UNSUPPORTED_OPERATION = 5,
    AWS_ERROR_INVALID_BUFFER_SIZE = 6,
    AWS_ERROR_INVALID_INDEX = 7,
    AWS_ERROR_LIST_EMPTY = 8,
    AWS_ERROR_DEST_COPY_TOO_SMALL = 9,
    AWS_ERROR_LIST_EXCEEDS_MAX_SIZE = 10,
    AWS_ERROR_INVALID_ARGUMENT = 11,
    AWS_ERROR_C_STRING_BUFFER_NOT_NULL_TERMINATED = 12,
    AWS_ERROR_DIVIDE_BY_ZERO = 13
};

enum aws_cryptosdk_err {
    /** The ciphertext was malformed or corrupt */
    AWS_CRYPTOSDK_ERR_BAD_CIPHERTEXT = 0x2000,
    /** A function was called on an object in the wrong state */
    AWS_CRYPTOSDK_ERR_BAD_STATE,
    /** No keyrings were able to decrypt the message in question */
    AWS_CRYPTOSDK_ERR_CANNOT_DECRYPT,
    /** An unknown internal error has occurred */
    AWS_CRYPTOSDK_ERR_CRYPTO_UNKNOWN,
    /** KMS returned an error */
    AWS_CRYPTOSDK_ERR_KMS_FAILURE,
    /** Caller attempted to exceed a hard limit */
    AWS_CRYPTOSDK_ERR_LIMIT_EXCEEDED,
    /** Caller attempted to use a name reserved by AWS */
    AWS_CRYPTOSDK_ERR_RESERVED_NAME,
    /** An unsupported format version was encountered on decrypt */
    AWS_CRYPTOSDK_ERR_UNSUPPORTED_FORMAT,
    AWS_CRYPTOSDK_ERR_END_RANGE = 0x2400
};

/* We use the same stubs as the cbmc stubs that are in the original aws-c-common repo. We don't need error handlers */

static int tl_last_error = 0;

// Various predicates for describing errors
/*@

    pred any_aws_last_error() {
        (error__tl_last_error -g> int(#trash))
    }

    pred aws_last_error_is(err) {
        (error__tl_last_error -g> int(err))
    }

    pred aws_last_error_is_SHORT_BUF() {
        aws_last_error_is(3)
    }

    pred aws_last_error_is_BAD_CIPHERTEXT() {
        aws_last_error_is(8192)
    }
*/

// We override the original aws_raise_error_private implementation to avoid
// error handler functions (unnecessary for the verification process).
void aws_raise_error_private(int err) { tl_last_error = err; }

int aws_raise_error(int err) {
    /*
     * Certain static analyzers can't see through the out-of-line call to aws_raise_error,
     * and assume that this might return AWS_OP_SUCCESS. We'll put the return inline just
     * to help with their assumptions.
     */
    aws_raise_error_private(err);

    return AWS_OP_ERR;
}

// We override the original aws_last_error implementation similarly
int aws_last_error(void) { return tl_last_error; }

/* Allocator structure. An instance of this will be passed around for anything
 * needing memory allocation */
struct aws_allocator {
    void *(*mem_acquire)(struct aws_allocator *allocator, size_t size);
    void (*mem_release)(struct aws_allocator *allocator, void *ptr);
    /* Optional method; if not supported, this pointer must be NULL */
    void *(*mem_realloc)(struct aws_allocator *allocator, void *oldptr,
                         size_t oldsize, size_t newsize);
    /* Optional method; if not supported, this pointer must be NULL */
    void *(*mem_calloc)(struct aws_allocator *allocator, size_t num,
                        size_t size);
    void *impl;
};

/*@ pred default_allocator(allocator) {
  (allocator -> struct aws_allocator {
    funptr(s_default_malloc);
    funptr(s_default_free);
    funptr(s_default_realloc);
    funptr(s_default_calloc);
    NULL
    })
}
*/

void *s_default_malloc(struct aws_allocator *allocator, size_t size) {
    (void)allocator;
    return malloc(size);
}

void s_default_free(struct aws_allocator *allocator, void *ptr) {
    (void)allocator;
    free(ptr);
}

void *s_default_calloc(struct aws_allocator *allocator, size_t num,
                       size_t size) {
    (void)allocator;
    return calloc(num, size);
}

/* This function is the only one unchanged from the original implementation */
void *s_default_realloc(struct aws_allocator *allocator, void *ptr,
                        size_t oldsize, size_t newsize) {
    (void) allocator;
    (void) oldsize;

    if (newsize == 0) {
        free(ptr);
        return NULL;
    }

    if (newsize <= oldsize) {
        return ptr;
    }

    void *new_mem = s_default_malloc(allocator, newsize);
    memcpy(new_mem, ptr, oldsize);
    s_default_free(allocator, ptr);
    return new_mem;
}

void *aws_mem_acquire(struct aws_allocator *allocator, size_t size) {
    // AWS_FATAL_PRECONDITION(allocator != NULL);
    // AWS_FATAL_PRECONDITION(allocator->mem_acquire != NULL);
    /* Protect against
     * https://wiki.sei.cmu.edu/confluence/display/c/MEM04-C.+Beware+of+zero-length+allocations
     */
    // AWS_FATAL_PRECONDITION(size != 0);

    void *mem = allocator->mem_acquire(allocator, size);
    if (!mem) {
         aws_raise_error(AWS_ERROR_OOM);
    }
    return mem;
}

void aws_mem_release(struct aws_allocator *allocator, void *ptr) {
    if (ptr != NULL) {
        allocator->mem_release(allocator, ptr);
    }
}

#ifndef AWS_BASE_GILLIAN
#define AWS_BASE_GILLIAN

#define aws_cryptosdk_unlikely(x) (x)
#define aws_cryptosdk_likely(x) (x)

#define aws_ntoh16(x) (x)
#define aws_ntoh32(x) (x)

#define true 1
#define false 0

#define GILLIAN(X) __builtin_annot(X)

typedef int bool;
typedef unsigned long size_t;

/*@
    import  `logic/EncryptionHeaderLogic`,
            `logic/ListLogic`,
            `logic/Utf8Logic`,
            `logic/ByteLogic`;
*/

/*
    Various useful predicates for memory management. For aws_mul_u64_checked, see
    https://github.com/awslabs/aws-c-common/blob/bb797381f3468e6f076e53eddbb399a99f54f67b/include/aws/common/math.fallback.inl#L30
    Our predicate describes the valid case.
*/
/*@
    pred aws_add_u64_checked(+a, +b, out) {
        (out == (a + b)) *
        (out <# 4611686018427387903)
    }

    pred aws_mul_u64_checked(+a, +b, out) {
        (out == (a * b)) *
        (out <# 4611686018427387903)
    }

    pred nounfold writable_memory(+pointer, +length, content) {
        (length == 0) * (content == []);
        (0 <# length) * ARRAY(pointer, char, length, content)
    }

    pred nounfold optBytes(+bytes, +length, content) {
        (length == 0) * (content == nil);
        (0 <# length) * ARRAY(bytes, char, length, content)
    }

    lemma optBytesConcat(lptr, llength, rptr, rlength) {
        hypothesis:
            optBytes(#lptr, #llength, #lcont) * optBytes(#rptr, #rlength, #rcont) *
            (#rptr == #lptr p+ #llength)

        conclusions:
            optBytes(#lptr, #llength + #rlength, #lcont @ #rcont)

        proof:
            unfold optBytes(#lptr, #llength, #lcont);
            unfold optBytes(#rptr, #rlength, #rcont);
            fold optBytes(#lptr, #llength + #rlength, #lcont @ #rcont)
    }
*/

int aws_add_size_checked(size_t a, size_t b, size_t *res) {
    if (a > 65535 || b > 65535) {
        GILLIAN("assert False");
        return 1;
    } else {
        *res = a + b;
        return 0;
    }
}

int aws_mul_size_checked(size_t a, size_t b, size_t *res) {
    if (a > 65535 || b > 65535) {
        GILLIAN("assert False");
        return 0;
    } else {
        *res = a * b;
        return 0;
    }
}


/**
 * Set each byte in the struct to zero.
 */
#define AWS_ZERO_STRUCT(object)                                                \
    do {                                                                       \
        memset(&(object), 0, sizeof(object));                                  \
    } while (0)

#endif

struct aws_string {
    struct aws_allocator *const allocator;
    const size_t len;
    /* give this a storage specifier for C++ purposes. It will likely be larger after init. */
    const uint8_t bytes[1];
};

/**
 * Allocate a new string with the same contents as array.
 */
struct aws_string *aws_string_new_from_array(struct aws_allocator *allocator,
                                             const uint8_t *bytes, size_t len);

#endif // AWS_COMMON_STRING_H
/*@
import `logic/StringStruct`;
*/

/*@
  pred valid_aws_string_ptr(+str, alloc, content) {
    m_struct_aws_string_exposing_pointer(str, alloc, long(#len), #bytes) *
    (0 <=# #len) *
    ARRAY(#bytes, char, #len + 1, #bytes_content) *
    (#rawContent == lsub(#bytes_content, 0, #len)) *
    (#bytes_content == #rawContent @ [ 0 ]) *
    toUtf8(#rawContent, content)
  }
*/

/*@ pred nounfold optBytes(+bytes, +length, content) {
  (length == 0) * (content == nil);
  (0 <# length) * ARRAY(bytes, char, length, content)
}
*/

// additional argument for how compcert handles struct passing
/*@ spec aws_string_new_from_array(allocator, bytes, length) {
  requires: (allocator == #alloc) *
            (bytes == #bytes) * (length == long(#len)) *
            (#len <=# 65535) *
            optBytes(#bytes, #len, #rawContent) *
            toUtf8(#rawContent, #strContent) *
            default_allocator(#alloc)
  ensures:  valid_aws_string_ptr(ret, #alloc, #strContent) *
            optBytes(#bytes, #len, #rawContent) *
            default_allocator(#alloc)
}
*/
struct aws_string *aws_string_new_from_array(struct aws_allocator *allocator,
                                             const uint8_t *bytes,
                                             size_t length) {
    // AWS_PRECONDITION(allocator);
    // AWS_PRECONDITION(AWS_MEM_IS_READABLE(bytes, length));
    size_t malloc_size;
    if (aws_add_size_checked(sizeof(struct aws_string) + 1, length,
                             &malloc_size)) {
        return NULL;
    }
    struct aws_string *str = aws_mem_acquire(allocator, malloc_size);
    if (!str) {
        return NULL;
    }

    /* Fields are declared const, so we need to copy them in like this */
    *(struct aws_allocator **)(&str->allocator) = allocator;
    *(size_t *)(&str->len) = length;
    if (length > 0) {
        memcpy((void *)str->bytes, bytes, length);
    }
    *(uint8_t *)&str->bytes[length] = '\0';
    // AWS_RETURN_WITH_POSTCONDITION(str, aws_string_is_valid(str));
    return str;
}
