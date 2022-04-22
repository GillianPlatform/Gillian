#include <stdlib.h>
#define FALSE 0
#define TRUE 1

typedef struct bstn {
    int value;
    struct bstn *left;
    struct bstn *right;
} BST;

/*@ pred BST(+x, K) {
  (x == NULL) * (K == -{ }-);

  x -m> struct bstn { int(#val); #left; #right } *
  (not (#left == NULL)) * (not (#right == NULL)) *
  BST(#right, #KR) * BST(#left, #KL) *
  (forall #z : Int. (#z --e-- #KL) => (#z <# #val)) *
  (forall #z : Int. (#z --e-- #KR) => (#val <# #z)) *
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

/*@ spec makeNode(v) {
  requires: (v == #v) * (#v == int(#vv))
  ensures:  BST(ret, -{ #vv }-)
}*/
BST *makeNode(int v) {
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
        return makeNode(v);
    };
    if (v < t->value) {
        tmp = insert(v, t->left);
        __builtin_annot("unfold BST(tmp, -u- (-{ #vv }-, #KL))");
        t->left = tmp;
    } else if (v > t->value) {
        tmp = insert(v, t->right);
        __builtin_annot("unfold BST(tmp, -u- (-{ #vv }-, #KR))");
        t->right = tmp;
    };
    return t;
}

/*@ spec find(v, t) {
  requires: (v == int(#vv)) * (t == #t) * BST(#t, #K)
  ensures:  BST(#t, #K) * (#vv --e-- #K) * (ret == TRUE);
            BST(#t, #K) * (not (#vv --e-- #K)) * (ret == FALSE)
} */
int find(int v, BST *t) {
    if (t == NULL) {
        return FALSE;
    } else if (v == t->value) {
        return TRUE;
    } else if (v < t->value) {
        return find(v, t->left);
    } else { /* the only last case is v > t->value */
        return find(v, t->right);
    }
}

/*@ spec find_min(t) {
  requires: (t == #t) * BST(#t, #K) * (not (#t == NULL))
  ensures:  BST(#t, #K) * (ret == int(#r)) * (#r --e-- #K) *
            (forall #x : Int. (#x --e-- #K) => (#r <=# #x))
} */
int find_min(BST *t) {
    __builtin_annot("unfold BST(#t, #K)");
    __builtin_annot(
        "assert [[bind #left]] t -m> struct bstn { #a; #left; #right }");
    __builtin_annot("unfold BST(#left, #someSet)");
    if (t->left == NULL) {
        return t->value;
    } else {
        return find_min(t->left);
    }
}

/* spec remove(v, t) {
  requires: (v == int(#v)) * (t == #t) * BST(#t, #K)
  ensures:  (ret == #t_new) * BST(#t_new, #K_new) * (#K_new == #K -d- -{ #v }-)
} */
BST *remove(int v, BST *t) {
    BST *ret_node;
    if (t == NULL) {
        return NULL;
    } else if (v == t->value) {
        __builtin_annot("assert [[bind #left, #right]] t -m> struct bstn { "
                        "int(#v); #left; #right }");
        __builtin_annot("if (! (#left = NULL)) { unfold BST(#left, #KL) }");
        __builtin_annot("if (! (#right = NULL)) { unfold BST(#right, #KR) }");
        if (t->left == NULL) {
            ret_node = t->right;
            free(t);
            return ret_node;
        } else if (t->right == NULL) {
            ret_node = t->left;
            free(t);
            return ret_node;
        } else {
            int min = find_min(t->right);
            BST *trt = remove(min, t->right);
            __builtin_annot("unfold BST(trt, #NKR)");
            t->right = trt;
            t->value = min;
        }
    } else if (v < t->value) {
        BST *ntl = remove(v, t->left);
        __builtin_annot("unfold BST(ntl, #NKL)");
        t->left = ntl;
    } else {
        BST *ntr = remove(v, t->right);
        __builtin_annot("unfold BST(ntr, #NKR)");
        t->right = ntr;
    };
    return t;
}