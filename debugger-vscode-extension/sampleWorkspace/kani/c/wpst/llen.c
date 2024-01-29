#include <stdlib.h>

typedef struct node {
    int val;
    struct node * next;
} node_t;

int llen(node_t* l) {
  if (l == NULL) {
    return 0;
  } else {
    node_t* t = l->next;
    return 1 + llen(t);
  }
}

node_t* build_list(int size) {
  node_t* l = NULL;
  for (int i = 0; i < size; i++) {
    node_t* t = malloc(sizeof(node_t));
    t->val = size - i;
    t->next = l;
    l = t;
  }
  return l;
}

int main() {
  int x = __nondet_int();
  __CPROVER_assume(x >= 2);
  __CPROVER_assume(x <= 3);
  node_t* l = build_list(x);
  int size = llen(l);
  __CPROVER_assert(size == 3, "size test");
}
