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

PQN* a_makeNode (int pri, int val) {
  PQN* r = malloc(sizeof(PQN));
  r->pri = pri;
  r->val = val;
  r->next = NULL;
  return r;
}

PQN* b_insert(PQN* queue, PQN* node) {
  if (queue == NULL) {
    return node;
  } else if (node->pri > queue->pri) {
    node->next = queue;
    return node;
  } else {
    PQN* tmp = b_insert(queue->next, node);
    queue->next = tmp;
    return queue;
  };
}

PQN* c_enqueue(PQN* queue, int pri, int val) {
  return b_insert(queue, a_makeNode(pri, val));
}

int* d_peek(PQN* queue) {
  int* ret = malloc(sizeof(int));
  if (queue == NULL) {
    ret = NULL;
  } else {
    *ret = queue->val;
  };
  return ret;
}

PQN* e_dequeue(PQN* queue) {
  PQN* rest = queue->next;
  free(queue);
  return rest;
}