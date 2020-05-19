#include "bst.h"

BST *a_make_node(int v) {
    BST *new_node = malloc(sizeof(BST));
    new_node->value = v;
    new_node->left = NULL;
    new_node->right = NULL;
    return new_node;
}
