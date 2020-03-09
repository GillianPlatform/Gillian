#include <gillian-c/gillian-c.h>
#include <stdlib.h>

/* The implementation is slightly different than the Javascript's one.
   Idiomatic C code would rather give two functions : one to peek, that gives the highest
   priority value, and one to dequeue, that returns only the new queue with the highest priority
   node dequeued. */

typedef struct pqn {
  int pri;
  int val;
  struct pqn* next;
} PQN;

PQN* makeNode (int pri, int val) {
  PQN* r = malloc(sizeof(PQN));
  r->pri = pri;
  r->val = val;
  r->next = NULL;
  return r;
}

PQN* insert(PQN* queue, PQN* node) {
  if (queue == NULL) {
    return node;
  } else {
    if (node->pri > queue->pri) {
      node->next = queue;
      return node;
    } else {
      PQN* tmp = insert(queue->next, node);
      queue->next = tmp;
      return queue;
    };
  };
}

PQN* enqueue(PQN* queue, int pri, int val) {
  return insert(queue, makeNode(pri, val));
}

int isEmpty(PQN* queue) {
  return queue == NULL;
}

int peek(PQN* queue) {
  return queue->val;
}

PQN* dequeue(PQN* queue) {
  PQN* rest = queue->next;
  free(queue);
  return rest;
}

int main() {
  int v1 = __builtin_annot_intval("symb_int", v1);
  int p1 = __builtin_annot_intval("symb_int", p1);
  int v2 = __builtin_annot_intval("symb_int", v2);
  int p2 = __builtin_annot_intval("symb_int", p2);
  int v3 = __builtin_annot_intval("symb_int", v3);
  int p3 = __builtin_annot_intval("symb_int", p3);
  ASSUME(p3 > p2 && p2 > p1);
  PQN* q = makeNode(p2, v2);
  q = enqueue(q, p3, v3);
  q = enqueue(q, p1, v1);
  ASSERT(peek(q) == v3);
  q = dequeue(q);
  ASSERT(peek(q) == v2);
  q = dequeue(q);
  ASSERT(peek(q) == v1);
  q = dequeue(q);
  ASSERT(isEmpty(q));
  return 0;
}