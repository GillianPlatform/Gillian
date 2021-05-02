#include "sll.h"

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