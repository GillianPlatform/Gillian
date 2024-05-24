#include <gillian-c/gillian-c.h>
#include <stdlib.h>

static int x;
static int y;

int assign_x() {
    x = 1;
    return 0;
}

int assign_y() {
    y = x - 1;
    return 0;
}

int main(void) {
    assign_x();
    assign_y();
    // int *z = malloc(sizeof(int));
    ASSERT(x > y);
    return 0;
}