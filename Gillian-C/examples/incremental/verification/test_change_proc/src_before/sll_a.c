#include "sll.h"

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