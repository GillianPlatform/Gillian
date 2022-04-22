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