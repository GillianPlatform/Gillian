#include <gillian-c/gillian-c.h>

void incr(int *n) {
    goto inside;
    if (n != 0) {
    inside: { (*n)++; };
    }
}

int main() {
    int n = 0;
    incr(&n);
    ASSERT(n == 1);
}