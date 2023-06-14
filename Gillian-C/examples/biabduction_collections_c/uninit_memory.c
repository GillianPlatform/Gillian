#include "array_full.h"

typedef struct s {
    int a;
    int b;
    int c;
} S;

void f(S *s) {
    s->a = 8;
    s->b = 2;
}

int tesfn() {
    S s;
    f(&s);
    return 0;
}