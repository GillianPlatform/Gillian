#include "bst_node.h"

BST *b_insert(int v, BST *t) {
    BST *tmp;
    if (t == NULL) {
        return a_makeNode(v);
    };
    if (v < t->value) {
        tmp = b_insert(v, t->left);
        t->left = tmp;
    } else if (v > t->value) {
        tmp = b_insert(v, t->right);
        t->right = tmp;
    };
    return t;
}

int c_find(int v, BST *t) {
    if (t == NULL) {
        return FALSE;
    } else if (v == t->value) {
        return TRUE;
    } else if (v < t->value) {
        return c_find(v, t->left);
    } else { /* the only last case is v > t->value */
        return c_find(v, t->right);
    }
}

int d_find_min(BST *t) {
    if (t->left == NULL) {
        return t->value;
    } else {
        return d_find_min(t->left);
    }
}

BST *e_remove(int v, BST *t) {
    BST *ret_node;
    if (t == NULL) {
        return NULL;
    } else if (v == t->value) {
        if (t->left == NULL) {
            ret_node = t->right;
            free(t);
            return ret_node;
        } else if (t->right == NULL) {
            ret_node = t->left;
            free(t);
            return ret_node;
        } else {
            int min = d_find_min(t->right);
            t->right = e_remove(min, t->right);
            t->value = min;
        }
    } else if (v < t->value) {
        t->left = e_remove(v, t->left);
    } else {
        t->right = e_remove(v, t->right);
    };
    return t;
}
