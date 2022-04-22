#include <stdlib.h>
#define FALSE 0
#define TRUE 1

typedef struct ln {
    int data;
    struct ln *next;
} SLL;

SLL* listPrependV(SLL *x, int v);

// Some change within the header file