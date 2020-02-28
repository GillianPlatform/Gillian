#include <stdlib.h>

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
  x = listAppend(x, 3);
  x = listAppend(x, 4);
  DLL* last = x->next;
  x = listPrepend(x, 2);
  x = listPrepend(x, 1);
  x = listConcat(listCopy(x), x);
  int shouldBeTrue = ((sum(x) == reverseSum(last)) + (altern(x) == -reverseAltern(last)) + (listLength(x) == reverseLength(last)) == 3);
  return
    !shouldBeTrue; /* returns 3 */
}