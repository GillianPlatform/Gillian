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
    return new_node;
}

/*@ spec find_min(t) {
  requires: (t == #t) * BST(#t, #K) * (not (#t == NULL))
  ensures:  BST(#t, #K) * (ret == int(#r)) * (#r --e-- #K) *
            (forall #x : Num. (#x --e-- #K) => (#r <=# #x))
} */
int find_min(BST *t) {
    __builtin_annot("unfold BST(#t, #K)");
    __builtin_annot(
        "assert [[exists #left]] t -m> struct bstn { #a; #left; #right }");
    __builtin_annot("unfold BST(#left, #someSet)");
    if (t->left == NULL) {
        return t->value;
    } else {
        return find_min(t->left);
    }
}
