#include <stdlib.h>

typedef struct dln {
    int data;
    struct dln *prev;
    struct dln *next;
} DLL;

/*@ pred DLL(+x, alpha) {
  (x == NULL) * (alpha == nil);
  (x -m> struct dln {#val; #prev; #next}) *
  DLL(#next, #beta) * (alpha == #val :: #beta) *
  (not (#prev == NULL)) * (not (#next == NULL));
  (x -m> struct dln {#val; NULL; #next}) *
  DLL(#next, #beta) * (alpha == #val :: #beta) *
  (not (#next == NULL));
  (x -m> struct dln {#val; NULL; NULL}) * (alpha == [ #val ]);
  (x -m> struct dln {#val; #prev; NULL}) * (alpha == [ #val ]) *
  (not (#prev == NULL))
} */

/*@ spec makeNode(x) {
  requires: (#x == int(#z)) * (#x == x)
  ensures:  DLL(ret, [#x])
} */
DLL *makeNode(int x) {
    DLL *r = malloc(sizeof(DLL));
    r->data = x;
    r->next = NULL;
    r->prev = NULL;
    return r;
}

/*@ spec listConcat(x, y) {
  requires: (x == #x) * (y == #y) * DLL(#x, #alpha) * DLL(#y, #beta)
  ensures:  DLL(ret, #alpha @ #beta)
} */
DLL *listConcat(DLL *x, DLL *y) {
    if (y == NULL) {
        return x;
    } else {
        if (x == NULL) {
            return y;
        } else {
            __builtin_annot(
                "assert [[bind #vx, #px, #nx, #gamma]] "
                "(x -m> struct dln {#vx; #px; #nx}) * DLL(#nx, #gamma) * "
                "(#alpha == #h :: #gamma)");
            __builtin_annot("unfold DLL(#nx, #gamma)");
            if (x->next == NULL) {
                x->next = y;
                y->prev = x;
                return x;
            } else {
                DLL *new_tail = listConcat(x->next, y);
                __builtin_annot("unfold DLL(new_tail, #gamma @ #beta)");
                x->next = new_tail;
                return x;
            }
        }
    }
}

/*@ spec listPrepend(x, v) {
  requires: (x == #x) * (v == #v) * DLL(#x, #alpha) * (#v == int(#z))
  ensures:  DLL(ret, #v :: #alpha)
} */
DLL *listPrepend(DLL *x, int v) {
    DLL *node_v = makeNode(v);
    if (x == NULL) {
        return node_v;
    } else {
        __builtin_annot("unfold DLL(node_v, [#v])");
        x->prev = node_v;
        node_v->next = x;
        return node_v;
    };
}

/*@ spec listAppend(x, v) {
  requires: (x == #x) * (v == #v) * DLL(#x, #alpha) * (#v == int(#z))
  ensures:  DLL(ret, #alpha @ [ #v ])
} */
DLL *listAppend(DLL *x, int v) { return listConcat(x, makeNode(v)); }

/*@ spec listCopy(x) {
  requires: (x == #x) * DLL(#x, #alpha)
  ensures:  DLL(#x, #alpha) * DLL(ret, #alpha)
} */
DLL *listCopy(DLL *x) {
    if (x == NULL) {
        return NULL;
    } else {
        DLL *t = listCopy(x->next);
        return listPrepend(t, x->data);
    };
}