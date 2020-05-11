#include "bst.h"

/*@ pred BST(+x, K) {
  (x == NULL) * (K == -{ }-) * (1 == 1);

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

// Needed to stop CompCert from discarding the struct definition
int unused(BST *t) { return TRUE; }
