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