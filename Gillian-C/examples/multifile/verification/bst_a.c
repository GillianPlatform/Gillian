#include "bst.h"

/*@ spec insert(v, t) {
  requires: (v == int(#vv)) * (t == #t) * BST(#t, #K)
  ensures:  BST(ret, -u- (#K, -{ #vv }-))
}*/
BST *insert(int v, BST *t) {
    BST *tmp;
    if (t == NULL) {
        return make_node(v);
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

/*@ spec remove(v, t) {
  requires: (v == int(#v)) * (t == #t) * BST(#t, #K)
  ensures:  (ret == #t_new) * BST(#t_new, #K_new) * (#K_new == #K -d- -{ #v }-)
} */
BST *remove(int v, BST *t) {
    BST *ret_node;
    if (t == NULL) {
        return NULL;
    } else if (v == t->value) {
        __builtin_annot("assert [[exists #left, #right]] t -m> struct bstn { "
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
    }
    return t;
}
