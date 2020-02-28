#include <stdlib.h>

typedef struct ln {
  int data;
  struct ln* next;
} SLL;

SLL* listAppend(SLL* x, int v) {
  if (x == NULL) {
    SLL* el = malloc(sizeof(SLL));
    el->data = v;
    el->next = NULL;
    return el;
  } else {
    SLL* tailp = listAppend(x->next, v);
    x->next = tailp;
    return x;
  };
}

SLL* listPrepend(SLL* x, SLL* z) {
  x->next = z;
  return x;
}

int listLength(SLL* x) {
  if (x == NULL) {
    return 0;
  } else {
    return 1 + listLength(x->next);
  };
}

void listDispose(SLL* x) {
  if (x == NULL) {
    return;
  } else {
    listDispose(x->next);
    free(x);
    return;
  };
}


SLL* listCopy(SLL* x) {
  SLL* r;
  if (x == NULL) {
    r = NULL;
  } else {
    SLL* t = listCopy(x->next);
    r = malloc(sizeof(SLL));
    r->data = x->data;
    r->next = t;
  };
  return r;
}

SLL* listConcat(SLL* x, SLL* y) {
  SLL* r;
  if (x == NULL) {
      r = y;
  } else {
    SLL* c = listConcat (x->next, y);
    x->next = c;
    r = x;
  };
  return r;
}