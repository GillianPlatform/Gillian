#include "sll.h"

/*@ spec listPrepend(x, z) {
  requires: (x -m> struct ln { #head; NULL }) *
            (x == #v) *
            (z == #z) *
            list(#z, #alpha)
  ensures: list(ret, #head::#alpha)
} */
SLL *listPrepend(SLL *x, SLL *z) {
    __builtin_annot("unfold list(#z, #alpha)");
    x->next = z;
    return x;
}

/*@ spec listLength(x) {
  requires: list(#x, #alpha) * (x == #x)
  ensures:  list(#x, #alpha) * (ret == int(#r)) * (#r == len #alpha)
} */
int listLength(SLL *x) {
    if (x == NULL) {
        return 0;
    } else {
        return 1 + listLength(x->next);
    };
}

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
        __builtin_annot("unfold list (NULL, #beta)");
        __builtin_annot("unfold list(t, #beta)");
        r = malloc(sizeof(SLL));
        r->data = x->data;
        r->next = t;
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