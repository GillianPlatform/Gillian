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
  __builtin_annot("unfold PriQ(#queue, #qpri, #qlen)");
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
  PQN* queue = makeNode(2, 7);
  queue = enqueue(queue, 3, 6);
  queue = enqueue(queue, 4, 5);
  queue = enqueue(queue, 1, 4);
  while (!isEmpty(queue)) {
    int SEEHERE = peek(queue);
    queue = dequeue(queue);
  }
  return 0;
}