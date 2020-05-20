#include "foo.h"

// Defined externally
extern int x;

int z;

// Should only be visible internally
static int y = 6;
static int bar() { return 5; }

int main() {
    int a;
    a = x;
    a = y;
    a = z;
    a = foo(); // Should be assigned 4
    a = bar(); // Should be assigned 5
    return 0;
}
