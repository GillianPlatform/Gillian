#include "bst.h"

BST *a_make_node(int v) {
    BST *new_node = malloc(sizeof(BST));
    new_node->value = v;
    new_node->left = NULL;
    new_node->right = NULL;
    return new_node;
}

int d_find_min(BST *t) {
    if (t->left == NULL) {
        return t->value;
    } else {
        return d_find_min(t->left);
    }
}

int peek_unsafe(int v, BST *t) { return t->value; }

int peek_left_unsafe(int v, BST *t) { return t->left->value; }

int peek_right_unsafe(int v, BST *t) { return t->right->value; }
