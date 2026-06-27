#include <stdlib.h>

typedef struct snode_s {
    int data;
    struct snode_s *next;
} SNode;

typedef struct cc_slist_s {
    size_t size;
    SNode *head;
    SNode *tail;
} CC_SList;

/*@ pred sll(+p, alpha) {
  (p -m> struct snode_s { #head; #tail } * (alpha == #head::#beta)) *
  sll(#tail,#beta) *
  i__is_size_t(len alpha);
  (p == NULL) * (alpha == nil)
}

pred lseg(+p, +q, alpha) {
    (p == q) * (alpha == nil);

    (p -m> struct snode_s { #head; #tail }) * (alpha == #head::#beta) *
    lseg(#tail, q, #beta) * i__is_size_t(len alpha)
}

pred cc_sll_ht(+h, +t, +size, alpha) {
    (h == NULL) * (t == NULL) * (size == 0) * (alpha == []);

    lseg(h, t, #beta) *
    sll(t, [ #last ]) *
    (alpha == #beta @ [ #last ]) *
    (size == len alpha)
}

pred cc_sll(+x, alpha) {
    (x -m> struct cc_slist_s { #size; #h; #t }) *
    cc_sll_ht(#h, #t, #size, alpha)
}

lemma lseg_append(p, q, alpha, a, end) {
    hypothesis: lseg(#p, #q, #alpha) * (#q -m> struct snode_s { #a; #end }) * i__is_size_t((len #alpha) + 1)
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

lemma lseg_concat(p, q, r, alpha, gamma) {
    hypothesis: lseg(#p, #q, #alpha) * lseg(#q, #r, #gamma) * i__is_size_t(len (#alpha @ #gamma))
    conclusions: lseg(#p, #r, #alpha @ #gamma)
    proof:
      unfold lseg(#p, #q, #alpha)[[bind #head: #head,
                                        #tail: #tail,
                                        #beta: #beta]];
      if (!(#alpha = nil)) {
        apply lseg_concat(#tail, #q, #r, #beta, #gamma);
        fold lseg(#p, #r, #alpha @ #gamma)
      }
}
*/

/*@ spec cc_slist_add_last(x, v) {
  requires: (x == #x) * cc_sll(#x, #alpha)
          * (v == #v) * i__is_size_t((len #alpha) + 1)
  ensures:  cc_sll(#x, #alpha @ [ #v ])
} */
void cc_slist_add_last(CC_SList *x, int v) {
  SNode *node = calloc(1, sizeof(SNode));

  node->data = v;

  if (x->size == 0) {
    x->head = node;
    x->tail = node;
  } else {
    __GILLIAN(
      "assert [[bind #ah, #at, #xh, #xt]]"
        "(#alpha == #ah @ [ #at ])"
        "* (#x -m> struct cc_slist_s { #sz; #xh; #xt })"
    );
    __GILLIAN("unfold sll(#xt, [ #at ])");
    x->tail->next = node;
    x->tail = node;
    __GILLIAN("apply lseg_append(#xh, #xt, #ah, #at, node)");
  }
  x->size++;
}


/*@ spec cc_slist_splice(x, y) {
  requires: (x == #x) * cc_sll(#x, #alpha)
          * (y == #y) * cc_sll(#y, #beta)
          * i__is_size_t((len #alpha) + (len #beta))
  ensures:  cc_sll(#x, #alpha @ #beta) * cc_sll(#y, [])
} */
void cc_slist_splice(CC_SList *x, CC_SList *y) {
  if (y->size == 0)
    return;

  if (x->size == 0) {
    x->head = y->head;
    x->tail = y->tail;
  } else {
    __GILLIAN(
      "assert [[bind #ah, #at, #bh, #xh, #xt, #yh, #yt]]"
        "(#alpha == #ah @ [ #at ])"
        "* (#beta == #bh @ [ #bt ])"
        "* (#x -m> struct cc_slist_s { #xsz; #xh; #xt })"
        "* (#y -m> struct cc_slist_s { #ysz; #yh; #yt })"
    );
    __GILLIAN("unfold sll(#xt, [ #at ])");
    x->tail->next = y->head;
    x->tail = y->tail;
    __GILLIAN("apply lseg_append(#xh, #xt, #ah, #at, #yh)");
    __GILLIAN("apply lseg_concat(#xh, #yh, #yt, #alpha, #bh)");
  }
  x->size += y->size;

  y->head = NULL;
  y->tail = NULL;
  y->size = 0;
}
