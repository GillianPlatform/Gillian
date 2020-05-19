#include "bst.h"

BST *b_make_node_wrapper(int v) { return a_make_node(v); }

BST *c_insert(int v, BST *t) {
    BST *tmp;
    if (t == NULL) {
        return b_make_node_wrapper(v);
    }
    if (v < t->value) {
        tmp = c_insert(v, t->left);
        t->left = tmp;
    } else if (v > t->value) {
        tmp = c_insert(v, t->right);
        t->right = tmp;
    }
    return t;
}
