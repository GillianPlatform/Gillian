#include "sll.h"

/*@ spec listDispose(x) {
  requires: list(#x, #alpha) * (x == #x)
  ensures:  emp
} */
void listDispose(SLL *x) {
    if (x == NULL) {
        return;
    } else {
        listDispose(x->next);
        free(x);
        return;
    };
}

/*@ spec listCopy(x) {
  requires: list(#x, #alpha) * (x == #x)
  ensures:  list(ret, #alpha) * list(#x, #alpha)
} */
SLL *listCopy(SLL *x) {
    SLL *r;
    if (x == NULL) {
        r = NULL;
    } else {
        SLL *t = listCopy(x->next);
        r = listPrependV(t, x->data);
    };
    return r;
}

/*@ spec listConcat(x, y) {
  requires: list(#x, #alpha) * (x == #x) * list(#y, #beta) * (y == #y)
  ensures:  list(ret, #alpha @ #beta)
} */
SLL *listConcat(SLL *x, SLL *y) {
    SLL *r;
    if (x == NULL) {
        r = y;
    } else {
        SLL *c = listConcat(x->next, y);
        __builtin_annot("assert [[bind #gamma]] list(c, #gamma)");
        __builtin_annot("unfold list(c, #gamma)");
        __builtin_annot("fold list(c, #gamma)");
        x->next = c;
        r = x;
    };
    return r;
}