#include "base.h"
#include <stdlib.h>

int aws_add_size_checked(size_t a, size_t b, size_t *res) {
    if (a > 65535 || b > 65535) {
        return 1;
    } else {
        *res = a + b;
        return 0;
    }
}

int aws_mul_size_checked(size_t a, size_t b, size_t *res) {
    if (a > 65535 || b > 65535) {
        return 0;
    } else {
        *res = a * b;
        return 0;
    }
}
