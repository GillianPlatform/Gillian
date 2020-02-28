#include <stdlib.h>

typedef struct dln {
  int data;
  struct dln* prev;
  struct dln* next;
} DLL;

DLL* a_makeNode(int x) {
    DLL *r = malloc(sizeof(DLL));
    r->data = x;
    r->next = NULL;
    r->prev = NULL;
    return r;
}

DLL* b_listConcat(DLL* x, DLL* y) {
  if (y == NULL) {
    return x;
  } else {
    if (x == NULL) {
      return y;
    } else {
      if (x->next == NULL) {
        x->next = y; y->prev = x; return x;
      } else {
        DLL* new_tail = b_listConcat(x->next, y);
        x->next = new_tail;
        return x;
      }
    }
  }
}

DLL* c_listPrepend(DLL* x, int v) {
  DLL* node_v = a_makeNode(v);
  if (x == NULL) {
    return node_v;
  } else {
    x->prev = node_v;
    node_v->next = x;
    return node_v;
  };
}

DLL* d_listAppend(DLL* x, int v) {
  return b_listConcat(x, a_makeNode(v));
}

DLL* e_listCopy(DLL *x) {
  if (x == NULL) {
    return NULL;
  } else {
    DLL* t = e_listCopy(x->next);
    return c_listPrepend(t, x->data);
  };
}

int f_peekUnsafe(DLL *x) {
  return x->data;
}

int f_peekNextUnsafe(DLL *x) {
  return x->next->data;
}

int f_PeekPreviousUnsafe(DLL *x) {
  return x->prev->data;
}