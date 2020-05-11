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
    // Some change in the body
    return new_node;
}
