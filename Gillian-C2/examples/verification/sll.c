#include <stdlib.h>

typedef struct ln {
    int data;
    struct ln *next;
} SLL;

/*@ pred sll(+p, alpha) {
  (p -m> struct ln { #head; #tail } * (alpha == #head::#beta)) *
  sll(#tail, #beta) *
  i__is_int(len alpha);
  (p == NULL) * (alpha == nil)
}

pred lseg(+p, +q, alpha) {
    ( p == q ) * (alpha == nil);

    (p -m> struct ln { #head; #tail } * (alpha == #head::#beta)) *
    lseg(#tail, q, #beta) * i__is_int(len alpha)
}

lemma lseg_to_list(p, alpha) {
    hypothesis: lseg(#p, NULL, #alpha)
    conclusions: sll(#p, #alpha)
    proof:
      unfold lseg(#p, NULL, #alpha) [[bind #head: #head,
                                           #tail: #tail,
                                           #beta: #beta]];
      if (!(#p = NULL)) {
        apply lseg_to_list(#tail, #beta);
        fold sll(#p, #alpha)
    }
}

lemma lseg_append(p, q, alpha, a, end) {
    hypothesis: lseg(#p, #q, #alpha) * (#q -m> struct ln { #a; #end }) * i__is_int(len (#alpha @ [#a]))
    conclusions: lseg(#p, #end, #alpha @ [#a])
    proof:
      unfold lseg(#p, #q, #alpha)[[bind #head: #head,
                                           #tail: #tail,
                                           #beta: #beta]];
      if (!(#p = #q)) {
        apply lseg_append(#tail, #q, #beta, #a, #end);
        fold lseg(#p, #end, #alpha @ [#a])
      }
}
*/

/*@ spec listAppend(x, v) {
  requires: (x == #x) * sll(#x, #alpha) * (v == #v) * i__is_int(#v) * i__is_int((len #alpha) + 1)
  ensures:  sll(ret, #alpha @ [ #v ])
} */
SLL *listAppend(SLL *x, int v) {
    if (x == NULL) {
        SLL *el = malloc(sizeof(SLL));
        el->data = v;
        el->next = NULL;
        return el;
    } else {
        SLL *tailp = listAppend(x->next, v);
        __GILLIAN("assert [[bind #t]] sll(tailp, #t)");
        __GILLIAN("unfold sll(tailp, #t)");
        __GILLIAN("fold sll(tailp, #t)");
        x->next = tailp;
        return x;
    };
}

/*@ spec listPrepend(x, z) {
  requires: (x -m> struct ln { #head; NULL }) *
            (x == #v) *
            (z == #z) *
            sll(#z, #alpha) *
            i__is_int((len #alpha) + 1)
  ensures: sll(ret, #head::#alpha)
} */
SLL *listPrepend(SLL *x, SLL *z) {
    __GILLIAN("unfold sll(#z, #alpha)");
    x->next = z;
    return x;
}

/*@ spec listPrependV(x, v) {
  requires: (x == #x) * sll(#x, #alpha) * (v == #v) * i__is_int(#v) * i__is_int((len #alpha) + 1)
  ensures: sll(ret, #v::#alpha)
}
*/
SLL* listPrependV(SLL *x, int v) {
  SLL *el = malloc(sizeof(SLL));
  el->data = v;
  __GILLIAN("unfold sll(#x, #alpha)");
  el->next = x;
  return el;
}

/*@ spec listLength(x) {
  requires: sll(#x, #alpha) * (x == #x)
  ensures:  sll(#x, #alpha) * (ret == int(#r)) * (#r == len #alpha)
} */
int listLength(SLL *x) {
    if (x == NULL) {
        return 0;
    } else {
        return 1 + listLength(x->next);
    };
}

/*@ spec listDispose(x) {
  requires: sll(#x, #alpha) * (x == #x)
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
  requires: sll(#x, #alpha) * (x == #x)
  ensures:  sll(ret, #alpha) * sll(#x, #alpha)
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
  requires: sll(#x, #alpha) * (x == #x) * sll(#y, #beta) * (y == #y) * i__is_int((len #alpha) + (len #beta))
  ensures:  sll(ret, #alpha @ #beta)
} */
SLL *listConcat(SLL *x, SLL *y) {
    SLL *r;
    if (x == NULL) {
        r = y;
    } else {
        SLL *c = listConcat(x->next, y);
        __GILLIAN("assert [[bind #gamma]] sll(c, #gamma)");
        __GILLIAN("unfold sll(c, #gamma)");
        __GILLIAN("fold sll(c, #gamma)");
        x->next = c;
        r = x;
    };
    return r;
}
