#include <stdlib.h>
#define FALSE 0
#define TRUE 1

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
    BST* bst = makeNode(4);
    bst = insert(7, bst);
    bst = insert(3, bst);
    bst = insert(4, bst);
    int shouldBeFalse = find(10, bst);
    int shouldBeTrue = find(7, bst);
    int shouldBe3 = find_min(bst);
    bst = remove(3, bst);
    int shouldBe4 = find_min(bst);
    int finalShouldBeTrue =
      (shouldBeFalse == 0) && (shouldBeTrue == 1) && (shouldBe4 == 4) && (shouldBe3 == 3);
    return !finalShouldBeTrue;
}
