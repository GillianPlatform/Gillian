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
