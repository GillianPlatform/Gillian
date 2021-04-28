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

int aws_add_size_checked(size_t a, size_t b, size_t *res);
int aws_mul_size_checked(size_t a, size_t b, size_t *res);

/**
 * Set each byte in the struct to zero.
 */
#define AWS_ZERO_STRUCT(object)                                                \
    do {                                                                       \
        memset(&(object), 0, sizeof(object));                                  \
    } while (0)

#endif