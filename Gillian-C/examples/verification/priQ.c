#include <stdlib.h>

/* The implementation is slightly different than the Javascript's one.
   Idiomatic C code would rather give two functions : one to peek, that gives
   the highest priority value, and one to dequeue, that returns only the new
   queue with the highest priority node dequeued. */

typedef struct pqn {
    int pri;
    int val;
    struct pqn *next;
} PQN;

/*@ pred Node(+x, pri, val, next) {
  x -m> struct pqn { int(pri); int(val); next } * (0 <# pri)
} */

/*@ pred PriQ(+x, max_pri, max_val, length){
  (x == NULL) * (max_pri == 0) * (length == 0) * (max_val == NULL);

  Node(x, max_pri, max_val, #next) * (0 <# max_pri) * (not (#next == NULL)) *
  PriQ(#next, #next_pri, #next_val, #len_next) * (#next_pri <=# max_pri) *
  (length == #len_next + 1);

  Node(x, max_pri, max_val, NULL) * (0 <# max_pri) * (length == 1)
} */

/*@ spec makeNode(pri, val) {
  requires: (pri == int(#pri)) * (val == int(#val)) * (0 <# #pri)
  ensures:  Node(ret, #pri, #val, NULL)
} */
PQN *makeNode(int pri, int val) {
    PQN *r = malloc(sizeof(PQN));
    r->pri = pri;
    r->val = val;
    r->next = NULL;
    return r;
}

/*@ spec insert(queue, node) {
  requires: (#node == node) * (#queue == queue) *
            PriQ(#queue, #qpri, #qval, #qlen) *
            Node(#node, #npri, #nval, NULL) *
            (#qpri <# #npri)
  ensures:  PriQ(ret, #npri, #nval, #qlen + 1)

  OR

  requires: (#node == node) * (#queue == queue) *
            PriQ(#queue, #qpri, #qval, #qlen) *
            Node(#node, #npri, #nval, NULL) *
            (#npri <=# #qpri)
  ensures:  PriQ(ret, #qpri, #qval, #qlen + 1)

  OR

  requires: (#node == node) * (#queue == queue) *
            PriQ(#queue, #qpri, #qval, #qlen) *
            Node(#node, #npri, #nval, NULL)

  ensures:  PriQ(ret, #qpri, #qval, #qlen + 1) * (#npri <=# #qpri);
            PriQ(ret, #npri, #nval, #qlen + 1) * (#qpri <# #npri)
} */

PQN *insert(PQN *queue, PQN *node) {
    if (queue == NULL) {
        return node;
    } else if (node->pri > queue->pri) {
        node->next = queue;
        return node;
    } else {
        PQN *tmp = insert(queue->next, node);
        __builtin_annot("unfold PriQ(tmp, #p, #qval, #qlen)");
        queue->next = tmp;
        return queue;
    };
}

/*@ spec enqueue(queue, pri, val) {
  requires: (int(#npri) == pri) * (int(#nval) == val) * (#queue == queue) *
            PriQ(#queue, #qpri, #qval,  #qlen) *
            (#qpri <# #npri) * (0 <# #npri)
  ensures:  PriQ(ret, #npri, #nval, #qlen + 1)

  OR

  requires: (int(#npri) == pri) * (int(#nval) == val) * (#queue == queue) *
            PriQ(#queue, #qpri, #qval, #qlen) *
            (#npri <=# #qpri) * (0 <# #npri)
  ensures:  PriQ(ret, #qpri, #qval, #qlen + 1)
} */
PQN *enqueue(PQN *queue, int pri, int val) {
    return insert(queue, makeNode(pri, val));
}

/*@ spec peek(queue) {
  requires: (queue == #queue) * PriQ(#queue, #qpri, #qval, #qlen)
  ensures:  ret -m> int(#qval);
            (#qval == NULL) * (ret == NULL)
} */
int *peek(PQN *queue) {
    int *ret = malloc(sizeof(int));
    if (queue == NULL) {
        ret = NULL;
    } else {
        *ret = queue->val;
    };
    return ret;
}

/*@ spec dequeue(queue) {
  requires: (queue == #queue) *
            Node(#queue, #max_pri, #max_val, #next) *
            PriQ(#next, #next_pri, #next_val, #len_next)
  ensures:  PriQ(ret, #next_pri, #next_val, #len_next)
} */
PQN *dequeue(PQN *queue) {
    PQN *rest = queue->next;
    free(queue);
    return rest;
}