#include <stdlib.h>

typedef struct ln {
  int data;
  struct ln* next;
} SLL;

/*@ pred UList(+x, E) {
  (x == NULL) * (E == -{}-);

  (x -m> struct ln { int(#val); NULL }) * (E == -{ #val }-);

  (x -m> struct ln { int(#val); #next }) * (not (#next == NULL)) *
  UList(#next, #Ep) *
  (E == -u-(-{ #val }-, #Ep))
} */

/*@ pred OUList(+x, E) {
  (x == NULL) * (E == -{ }-);

  (x -m> struct ln { int(#val); NULL }) * (E == -{ #val }-);

  (x -m> struct ln { int(#val); #next }) * (not (#next == NULL)) *
  OUList(#next, #Ep) *
  (E == -u-(-{ #val }-, #Ep)) *
  (forall #z : Int. (#z --e-- #Ep => #val <# #z))
} */

/*@ spec makeNode(v) {
  requires: (v == int(#v))
  ensures:  OUList(ret, -{ #v }-)
} */
SLL* makeNode(int v) {
  SLL* r = malloc(sizeof(SLL));
  r->data = v;
  r->next = NULL;
  return r;
}

/*@ spec listPrepend(x, v) {
  requires: (v == int(#v)) * (x == #x) *
            OUList(#x, #E) * (forall #z : Int. (#z --e-- #E => #v <# #z))
  ensures: OUList(ret, -u-(-{#v}-, #E))
} */
SLL* listPrepend(SLL* x, int v) {
  SLL* new_node = makeNode(v);
  __builtin_annot("unfold OUList(new_node, -{ #v }-)");
  __builtin_annot("unfold OUList(#x, #E)");
  new_node->next = x;
  return new_node;
}

/*@ spec insert(l, v) {
  requires: (l == #l) * (v == int(#v)) * OUList(#l, #E)
  ensures: OUList(ret, -u-(-{#v}-, #E))
} */
SLL* insert(SLL* l, int v) {
  if (l == NULL) {
    return makeNode(v);
  } else if (l->data == v) {
    return l;
  } else if (l->data < v) {
    SLL* rec = insert(l->next, v);
    __builtin_annot("unfold OUList(rec, #someSet)");
    l->next = rec;
    return l;
  } else {
    return listPrepend(l, v);
  }
}

/*@ spec sort(l) {
  requires: (l == #l) * UList(#l, #E)
  ensures: OUList(ret, #E)
} */
SLL* sort(SLL* l) {
  if (l == NULL) {
    return NULL;
  } else {
    return insert(sort(l->next), l->data);
  }
}