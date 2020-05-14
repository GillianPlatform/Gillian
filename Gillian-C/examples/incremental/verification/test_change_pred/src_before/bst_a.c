#include "bst.h"

/*@ spec make_node(v) {
  requires: (v == #v) * (#v == int(#vv))
  ensures:  BST(ret, -{ #vv }-)
}*/
BST *make_node(int v) {
    BST *new_node = malloc(sizeof(BST));
    new_node->value = v;
    new_node->left = NULL;
    new_node->right = NULL;
    return new_node;
}

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
