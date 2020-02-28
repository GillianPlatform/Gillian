#include <stdlib.h>

typedef struct ln {
  int data;
  struct ln* next;
} SLL;

SLL* a_makeNode(int v) {
  SLL* r = malloc(sizeof(SLL));
  r->data = v;
  r->next = NULL;
  return r;
}

SLL* b_listPrepend(SLL* x, int v) {
  SLL* new_node = a_makeNode(v);
  new_node->next = x;
  return new_node;
}

SLL* c_insert(SLL* l, int v) {
  if (l == NULL) {
    return a_makeNode(v);
  } else if (l->data == v) {
    return l;
  } else if (l->data < v) {
    SLL* rec = c_insert(l->next, v);
    l->next = rec;
    return l;
  } else {
    return b_listPrepend(l, v);
  }
}

SLL* d_sort(SLL* l) {
  if (l == NULL) {
    return NULL;
  } else {
    return c_insert(d_sort(l->next), l->data);
  }
}