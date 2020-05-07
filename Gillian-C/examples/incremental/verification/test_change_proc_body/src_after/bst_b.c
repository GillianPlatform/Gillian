#include "bst.h"

/*@ pred BST(+x, K) {
  (x == NULL) * (K == -{ }-);

  x -m> struct bstn { int(#val); #left; #right } *
  (not (#left == NULL)) * (not (#right == NULL)) *
  BST(#right, #KR) * BST(#left, #KL) *
  (forall #z : Num. #z --e-- #KL => #z <# #val) *
  (forall #z : Num. #z --e-- #KR => #val <# #z) *
  (K == -u- (#KL, -{ #val }-, #KR));

  x -m> struct bstn { int(#val); NULL; #right } *
  (not (#right == NULL)) *
  BST(#right, #KR) *
  (forall #z : Num. #z --e-- #KR => #val <# #z) *
  (K == -u- (-{ #val }-, #KR));

  x -m> struct bstn { int(#val); #left; NULL } *
  (not (#left == NULL)) *
  BST(#left, #KL) *
  (forall #z : Num. #z --e-- #KL => #z <# #val) *
  (K == -u- (-{ #val }-, #KL));

  x -m> struct bstn { int(#val); NULL; NULL } *
  (K == -{ #val }-)
} */

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
