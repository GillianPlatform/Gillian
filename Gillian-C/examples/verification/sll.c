#include <stdlib.h>

typedef struct ln {
    int data;
    struct ln *next;
} SLL;

/*@ pred list(+p, alpha) {
  (p -m> struct ln { #head; #tail } * (alpha == #head::#beta)) *
  list(#tail,#beta);
  (p == NULL) * (alpha == nil)
}

pred listSeg(+p, +q, alpha) {
    ( p == q ) * (alpha == nil);

    (p -m> struct ln { #head; #tail } * (alpha == #head::#beta)) *
    listSeg(#tail, q, #beta)
}

lemma lsegToList(p, alpha) {
    hypothesis: listSeg(#p, NULL, #alpha)
    conclusions: list(#p, #alpha)
    proof:
      unfold listSeg(#p, NULL, #alpha) [[bind #head: #head,
                                           #tail: #tail,
                                           #beta: #beta]];
      if (!(#p = NULL)) {
        apply lsegToList(#tail, #beta);
        fold list(#p, #alpha)
    }
}

lemma listSegAppend(p, q, alpha, a, end) {
    hypothesis: listSeg(#p, #q, #alpha) * (#q -m> struct ln { #a; #end })
    conclusions: listSeg(#p, #end, #alpha @ [#a])
    proof:
      unfold listSeg(#p, #q, #alpha)[[bind #head: #head,
                                           #tail: #tail,
                                           #beta: #beta]];
      if (!(#p = #q)) {
        apply listSegAppend(#tail, #q, #beta, #a, #end);
        fold listSeg(#p, #end, #alpha @ [#a])
    }
}
*/

/*@ spec listAppend(x, v) {
  requires: (x == #x) * list(#x, #alpha) * (v == #v) * (#v == int(#z))
  ensures:  list(ret, #alpha @ [ #v ])
} */
SLL *listAppend(SLL *x, int v) {
    if (x == NULL) {
        SLL *el = malloc(sizeof(SLL));
        el->data = v;
        el->next = NULL;
        return el;
    } else {
        SLL *tailp = listAppend(x->next, v);
        __builtin_annot("assert [[bind #t]] list(tailp, #t)");
        __builtin_annot("unfold list(tailp, #t)");
        __builtin_annot("fold list(tailp, #t)");
        x->next = tailp;
        return x;
    };
}

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

/*@ spec listPrependV(x, v) {
  requires: (x == #x) * list(#x, #alpha) * (v == #v) * (#v == int(#z))
  ensures: list(ret, #v::#alpha)
}
*/
SLL* listPrependV(SLL *x, int v) {
  SLL *el = malloc(sizeof(SLL));
  el->data = v;
  __builtin_annot("unfold list(#x, #alpha)");
  el->next = x;
  return el;
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