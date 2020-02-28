#include <stdlib.h>
#include <klee/klee.h>

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

SLL* listPrepend(SLL* x, int v) {
  SLL* new_node = malloc(sizeof(SLL));
  new_node->data = v;
  new_node->next = x;
  return new_node;
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

int sum(SLL* x) {
  if (x == NULL) {
    return 0;
  } else {
    return x->data + sum(x->next);
  }
}

int altern(SLL* x) {
  if (x == NULL) {
    return 0;
  } else {
    return x->data - altern(x->next);
  }
}

int main() {
  SLL* x = NULL;
  int a, b, c, d, e;
  klee_make_symbolic(&a, sizeof(a), "a");
  klee_make_symbolic(&b, sizeof(b), "b");
  klee_make_symbolic(&c, sizeof(c), "c");
  klee_make_symbolic(&d, sizeof(d), "d");
  klee_make_symbolic(&e, sizeof(e), "e");
  x = listAppend(x, a);
  x = listAppend(x, b);
  x = listAppend(x, c);
  x = listPrepend(x, d);
  x = listPrepend(x, e);
  SLL* y = listCopy(x);
  x = listConcat(x, y);
  klee_assert(altern(x) == 0);
  klee_assert(listLength(x) == 10);
  klee_assert(sum(x) == (a + b + c + d + e + a + b + c + d + e));
  return 0;
}