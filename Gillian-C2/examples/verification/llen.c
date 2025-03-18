#include <stdlib.h>

typedef struct ln {
    int data;
    struct ln *next;
} SLL;

/*@
pred list(+p, alpha) {
  (p -m> struct ln { #head; #tail } * (alpha == #head::#beta)) *
  list(#tail,#beta);
  (p == NULL) * (alpha == nil)
}
*/

/*@ spec listLength(x) {
  requires: list(#x, #alpha) * (x == #x)
  ensures:  list(#x, #alpha) * (ret == int(#r)) * (#r == len #alpha)
} */
int listLength(SLL *x) {
    int i = 0;
    if (x == NULL) {
        int i = 2;
        return 0;
    } else {
        return 1 + listLength(x->next);
    };
}
