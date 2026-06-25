#include <stdlib.h>

typedef struct ln {
    int data;
    struct ln *next;
} SLL;

/*@
pred list(+p, alpha) {
  p -m> struct ln { #head; #tail } *
    (alpha == #head::#beta) *
    list(#tail,#beta) *
    i__is_int(len alpha);
  (p == NULL) * (alpha == nil)
}
*/

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
