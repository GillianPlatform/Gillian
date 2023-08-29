#include <gillian-c/gillian-c.h>
#include <stdlib.h>

/*@ spec init() {
  requires: emp
  ensures:  True
}
*/
int init() {
    char *p = (char *)malloc(8);
    if (p == NULL)
        return 0;
    for (int i = 0; i < 8; i++) {
        p[i] = 0;
    }
    long *k = (long *)p;
    ASSERT(*k == 0);
    return 0;
}

int f(int *x, int *y) { return *x + *y; }