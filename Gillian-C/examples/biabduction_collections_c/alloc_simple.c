#include <stdlib.h>

int allocate() {
    int *ptr = malloc(sizeof(int));
    *ptr = 8;
    return 0;
}

int allocate_can_fail() {
    int *ptr = malloc(sizeof(int));
    if (ptr == NULL) {
        return 1;
    }

    *ptr = 8;
    return 0;
}