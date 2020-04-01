#include "foo.h"

// Defined externally
extern int x;

int y = 6;
int z;

// Should only be visible internally
static int bar() { return 5; }

int main() {
    int a;
    a = x;
    a = y;
    a = z;
    a = foo();
    a = bar();
    return 0;
}
