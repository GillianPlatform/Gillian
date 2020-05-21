#include <stdlib.h>
#define FALSE 0
#define TRUE 1

typedef struct bstn {
    int value;
    struct bstn *left;
    struct bstn *right;
} BST;

BST *make_node(int v);

int find_min(BST *t);
