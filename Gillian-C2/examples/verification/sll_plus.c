#include <stdlib.h>

typedef struct ln {
    int data;
    struct ln *next;
} SLL;

typedef struct list {
    size_t size;
    SLL *head;
    SLL *tail;
} List;

/*@ pred sll(+p, alpha) {
  (p -m> struct ln { #head; #tail } * (alpha == #head::#beta)) *
  sll(#tail,#beta) *
  i__is_int(len alpha);
  (p == NULL) * (alpha == nil)
}

pred sllSeg(+p, +q, alpha) {
    (p == q) * (alpha == nil);

    (p -m> struct ln { #head; #tail }) * (alpha == #head::#beta) *
    sllSeg(#tail, q, #beta) * i__is_int(len alpha)
}

pred list_ht_inner(+h, +t, alpha) {
  (h == NULL) * (t == NULL) * (alpha == nil);

  sllSeg(h, t, #beta) *
  sll(t, [ #last ]) *
  (alpha == #beta @ [ #last ])
}

pred list_ht(+x, h, t, alpha) {
    (x -m> struct list { #size; #h; #t }) *
    (h == #h) * (t == #t) *
    (#size == (len alpha)) *
    list_ht_inner(h, t, alpha)
}

pred list(+x, alpha) {
    list_ht(x, #h, #t, alpha)
}

lemma segToSLL(p, alpha) {
    hypothesis: sllSeg(#p, NULL, #alpha)
    conclusions: sll(#p, #alpha)
    proof:
      unfold sllSeg(#p, NULL, #alpha) [[bind #head: #head,
                                           #tail: #tail,
                                           #beta: #beta]];
      if (!(#p = NULL)) {
        apply segToSLL(#tail, #beta);
        fold sll(#p, #alpha)
    }
}

lemma sllSegAppend(p, q, alpha, a, end) {
    hypothesis: sllSeg(#p, #q, #alpha) * (#q -m> struct ln { #a; #end }) * i__is_int(len (#alpha @ [#a]))
    conclusions: sllSeg(#p, #end, #alpha @ [#a])
    proof:
      unfold sllSeg(#p, #q, #alpha)[[bind #head: #head,
                                           #tail: #tail,
                                           #beta: #beta]];
      if (!(#p = #q)) {
        apply sllSegAppend(#tail, #q, #beta, #a, #end);
        fold sllSeg(#p, #end, #alpha @ [#a])
      }
}

lemma sllSegConcat(p, q, r, alpha, gamma) {
    hypothesis: sllSeg(#p, #q, #alpha) * sllSeg(#q, #r, #gamma) * i__is_int(len (#alpha @ #gamma))
    conclusions: sllSeg(#p, #r, #alpha @ #gamma)
    proof:
      unfold sllSeg(#p, #q, #alpha)[[bind #head: #head,
                                           #tail: #tail,
                                           #beta: #beta]];
      if (!(#alpha = nil)) {
        apply sllSegConcat(#tail, #q, #r, #beta, #gamma);
        fold sllSeg(#p, #r, #alpha @ #gamma)
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
  requires: list(#x, #alpha) * (x == #x)
          * list(#y, #beta) * (y == #y)
          * i__is_int((len #alpha) + (len #beta))
  ensures:  list(ret, #alpha @ #beta)
} */
List *listConcat(List *x, List *y) {
    if (x->size == 0) {
        free(x);
        return y;
    } else {
        if (y->size == 0) {
            free(y);
            return x;
        } else {
            __GILLIAN(
              "assert [[bind #ah, #at, #bh, #xh, #xt, #yh, #yt]]"
                "(#alpha == #ah @ [ #at ])"
                "* (#beta == #bh @ [ #bt ])"
                "* (#x -m> struct list { #xsz; #xh; #xt })"
                "* (#y -m> struct list { #ysz; #yh; #yt })"
            );
            __GILLIAN("unfold sll(#xt, [ #at ])");
            x->tail->next = y->head;
            x->tail = y->tail;
            free(y);
            __GILLIAN("apply sllSegAppend(#xh, #xt, #ah, #at, #yh)");
            __GILLIAN("apply sllSegConcat(#xh, #yh, #yt, #alpha, #bh)");
            __GILLIAN("fold list(#x, #alpha @ #beta)");
            return x;
        }
    }
}
