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
*/

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