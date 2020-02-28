#include <stdlib.h>
#include "klee/klee.h"
#define TRUE 1
#define FALSE 0

typedef struct bstn {
    int value;
    struct bstn* left;
    struct bstn* right;
} BST;

BST* makeNode(int v) {
  BST* new_node = malloc(sizeof(BST));
  new_node->value = v;
  new_node->left = NULL;
  new_node->right = NULL;
  return new_node;
}


BST* insert(int v, BST* t) {
  if (t == NULL) {
    return makeNode(v);
  };
  if (v < t->value) {
    t->left = insert(v, t->left);
  } else if (v > t->value) {
    t->right = insert(v, t->right);
  };
  return t;
}


int find (int v, BST* t) {
  int ret;
  if (t == NULL) {
    ret = FALSE;
  } else if (v == t->value) {
    ret = TRUE;
  } else if (v < t->value) {
    ret = find(v, t->left);
  } else { /* the only last case is v > t->value */
    ret = find(v, t->right);
  }
  return ret;
}

int find_min (BST* t) {
  if (t->left == NULL) {
    return t->value;
  } else {
    return find_min(t->left);
  }
}

BST* remove(int v, BST* t) {
  BST* ret_node;
  if (t == NULL) {
    return NULL;
  } else if (v == t->value) {
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
      t->right = remove(min, t->right);
      t->value = min;
    }
  } else if (v < t->value) {
    t->left = remove(v, t->left);
  } else {
    t->right = remove(v, t->right);
  };
  return t;
}

int main()
{
    int a, b, e, f;
    klee_make_symbolic(&a, sizeof(a), "a");
    klee_make_symbolic(&b, sizeof(b), "b");
    klee_make_symbolic(&e, sizeof(e), "e");
    klee_make_symbolic(&f, sizeof(f), "f");
    klee_assume(e != a);
    klee_assume(e != b);
    klee_assume((f == a) || (f == b));
    BST* bst = makeNode(a);
    bst = insert(b, bst);
    klee_assert(!find(e, bst));
    klee_assert(find(f, bst));
    int c;
    klee_make_symbolic(&c, sizeof(c), "c");
    klee_assume(c > a);
    klee_assume(a < b);
    bst = insert(c, bst);
    klee_assert(find_min(remove(b, bst)) == a);
    return 0;
}
