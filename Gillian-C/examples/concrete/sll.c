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
  x = listAppend(x, 3);
  x = listAppend(x, 4);
  x = listAppend(x, 5);
  x = listPrepend(x, 2);
  x = listPrepend(x, 1);
  SLL* y = listCopy(x);
  x = listConcat(x, y);
  int shouldBeTrue = ((sum(x) == 30) + (altern(x) == 0) + (listLength(x) == 10)) == 3;
  return !shouldBeTrue; /* returns 3 */
}