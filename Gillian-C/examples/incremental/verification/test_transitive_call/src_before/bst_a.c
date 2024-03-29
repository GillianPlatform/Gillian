#include "bst.h"

/*@ pred BST(+x, K) {
  (x == NULL) * (K == -{ }-);

  x -m> struct bstn { int(#val); #left; #right } *
  (not (#left == NULL)) * (not (#right == NULL)) *
  BST(#right, #KR) * BST(#left, #KL) *
  (forall #z : Int. #z --e-- #KL => #z <# #val) *
  (forall #z : Int. #z --e-- #KR => #val <# #z) *
  (K == -u- (#KL, -{ #val }-, #KR));

  x -m> struct bstn { int(#val); NULL; #right } *
  (not (#right == NULL)) *
  BST(#right, #KR) *
  (forall #z : Int. #z --e-- #KR => #val <# #z) *
  (K == -u- (-{ #val }-, #KR));

  x -m> struct bstn { int(#val); #left; NULL } *
  (not (#left == NULL)) *
  BST(#left, #KL) *
  (forall #z : Int. #z --e-- #KL => #z <# #val) *
  (K == -u- (-{ #val }-, #KL));

  x -m> struct bstn { int(#val); NULL; NULL } *
  (K == -{ #val }-)
} */

// Functions without specs that remain unchanged
BST *make_node_wrapper_b(int v) { return make_node(v); }

BST *make_node_wrapper_a(int v) { return make_node_wrapper_b(v); }

/*@ spec insert(v, t) {
  requires: (v == int(#vv)) * (t == #t) * BST(#t, #K)
  ensures:  BST(ret, -u- (#K, -{ #vv }-))
}*/
BST *insert(int v, BST *t) {
    BST *tmp;
    if (t == NULL) {
        return make_node_wrapper_a(v);
    };
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
