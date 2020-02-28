#include <stdlib.h>
#include <klee/klee.h>

typedef struct dln {
  int data;
  struct dln* prev;
  struct dln* next;
} DLL;

DLL* makeNode(int x) {
    DLL *r = malloc(sizeof(DLL));
    r->data = x;
    r->next = NULL;
    r->prev = NULL;
    return r;
}

DLL* listConcat(DLL* x, DLL* y) {
  if (y == NULL) {
    return x;
  } else {
    if (x == NULL) {
      return y;
    } else {
      if (x->next == NULL) {
        x->next = y; y->prev = x; return x;
      } else {
        DLL* new_tail = listConcat(x->next, y);
        x->next = new_tail;
        return x;
      }
    }
  }
}

DLL* listPrepend(DLL* x, int v) {
  DLL* node_v = makeNode(v);
  if (x == NULL) {
    return node_v;
  } else {
    x->prev = node_v;
    node_v->next = x;
    return node_v;
  };
}

DLL* listAppend(DLL* x, int v) {
  return listConcat(x, makeNode(v));
}

DLL* listCopy(DLL *x) {
  if (x == NULL) {
    return NULL;
  } else {
    DLL* t = listCopy(x->next);
    return listPrepend(t, x->data);
  };
}

int listLength(DLL* x) {
  if (x == NULL) {
    return 0;
  } else {
    return 1 + listLength(x->next);
  }
}

int sum(DLL* x) {
  if (x == NULL) {
    return 0;
  } else {
    return x->data + sum(x->next);
  }
}

int altern(DLL* x) {
  if (x == NULL) {
    return 0;
  } else {
    return x->data - altern(x->next);
  }
}

int reverseLength(DLL* x) {
  if (x == NULL) {
    return 0;
  } else {
    return 1 + reverseLength(x->prev);
  }
}

int reverseAltern(DLL* x) {
  if (x == NULL) {
    return 0;
  } else {
    return x->data - reverseAltern(x->prev);
  }
}

int reverseSum(DLL* x) {
  if (x == NULL) {
    return 0;
  } else {
    return x->data + reverseSum(x->prev);
  }
}

int main() {
  DLL* x = NULL;

  int a,b,c,d;
  klee_make_symbolic(&a, sizeof(a), "a");
  klee_make_symbolic(&b, sizeof(b), "b");
  klee_make_symbolic(&c, sizeof(c), "c");
  klee_make_symbolic(&d, sizeof(d), "d");
  klee_assume(a > 0);
  klee_assume(b > 0);
  klee_assume(c > 0);
  klee_assume(d > 0);
  klee_assume(a < 10);
  klee_assume(b < 10);
  klee_assume(c < 10);
  klee_assume(d < 10);
  x = listAppend(x, a);
  x = listAppend(x, b);
  DLL* last = x->next;
  x = listPrepend(x, c);
  x = listPrepend(x, d);
  x = listConcat(listCopy(x), x);
  klee_assert(sum(x) == (a + b + c + d) + a + b + c + d);
  klee_assert(reverseSum(last) == (a + b + c + d) + a + b + c + d);
  klee_assert(altern(x) == (d - c + a - b) + d - c + a + b);
  klee_assert(altern(x) == -reverseAltern(last));
  klee_assert(listLength(x) == reverseLength(last));
  return 0;
}