#include <stdlib.h>

typedef struct ln {
  int data;
  struct ln* next;
} SLL;

SLL* makeNode(int v) {
  SLL* r = malloc(sizeof(SLL));
  r->data = v;
  r->next = NULL;
  return r;
}

SLL* listPrepend(SLL* x, int v) {
  SLL* new_node = makeNode(v);
  new_node->next = x;
  return new_node;
}


SLL* insertOrder(SLL* l, int v) {
  if (l == NULL) {
    return makeNode(v);
  } else if (l->data == v) {
    return l;
  } else if (l->data < v) {
    SLL* rec = insertOrder(l->next, v);
    l->next = rec;
    return l;
  } else {
    return listPrepend(l, v);
  }
}

SLL* sort(SLL* l) {
  if (l == NULL) {
    return NULL;
  } else {
    SLL* rec = sort(l->next);
    return insertOrder(rec, l->data);
  }
}

int sorted(SLL* l) {
  if (l == NULL) {
    return 1;
  } else if (l->next == NULL) {
    return 1;
  } else {
    return (l->data < l->next->data) && sorted(l->next);
  }
}

int main() {
  int k1 = 4;
  int k2 = 1;
  int k3 = 9;
  int k4 = 7;
  SLL* l = makeNode(k1);
  l = listPrepend(l, k2);
  l = listPrepend(l, k3);
  l = listPrepend(l, k4);
  l = sort(l);
  int shouldBeTrue = sorted(l);
  return !shouldBeTrue; /* returns 0 */
}