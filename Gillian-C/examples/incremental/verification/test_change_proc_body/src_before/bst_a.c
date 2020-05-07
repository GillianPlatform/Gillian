#include "bst.h"

/*@ spec insert(v, t) {
  requires: (v == int(#vv)) * (t == #t) * BST(#t, #K)
  ensures:  BST(ret, -u- (#K, -{ #vv }-))
}*/
BST *insert(int v, BST *t) {
    BST *tmp;
    if (t == NULL) {
        return make_node(v);
    }
    if (v < t->value) {
        tmp = insert(v, t->left);
        __builtin_annot("unfold BST(tmp, -u- (-{ #vv }-, #KL))");
        t->left = tmp;
    } else if (v > t->value) {
        tmp = insert(v, t->right);
        __builtin_annot("unfold BST(tmp, -u- (-{ #vv }-, #KR))");
        t->right = tmp;
    }
    return t;
}

/*@ spec find(v, t) {
  requires: (v == int(#vv)) * (t == #t) * BST(#t, #K) * (#vv --e-- #K)
  ensures:  BST(#t, #K) * (ret == TRUE)

  OR

  requires: (v == int(#vv)) * (t == #t) * BST(#t, #K) * (not (#vv --e-- #K))
  ensures:  BST(#t, #K) * (ret == FALSE)
} */
int find(int v, BST *t) {
    if (t == NULL) {
        return FALSE;
    } else if (v == t->value) {
        return TRUE;
    } else if (v < t->value) {
        return find(v, t->left);
    } else {
        return find(v, t->right);
    }
}
