#include <stdlib.h>
#include "klee/klee.h"

typedef struct kvnode {
    int value;
    int key;
    struct kvnode* next;
} KVMAP;

KVMAP* makeNode(int k, int v) {
  KVMAP* new_node = malloc(sizeof(KVMAP));
  new_node->key = k;
  new_node->value = v;
  new_node->next = NULL;
  return new_node;
}

void put(int k, int v, KVMAP** kvm) {
  KVMAP* new_node = makeNode(k, v);
  new_node->next = *kvm;
  *kvm = new_node;
  return;
}

void remove(int k, KVMAP** kvm) {
  KVMAP* p = *kvm;
  if (p == NULL) {
    /* pass */
  } else if (p->key == k) {
    *kvm = p->next;
  } else {
    remove(k, &(p->next));
  };
  return;
}

void replace(int k, int v, KVMAP** kvm) {
  remove(k, kvm);
  put(k, v, kvm);
  return;
}

int length(KVMAP* kvm) {
  if (kvm == NULL) {
    return 0;
  } else {
    return 1 + length(kvm->next);
  }
}

int* get (int k, KVMAP** kvm)
{
  if (*kvm == NULL)
  {
    return NULL;
  }
  else if ((*kvm)->key == k)
  {
    return &((*kvm)->value);
  }
  else
  {
    return get(k, &(*kvm)->next);
  }
}

int main() {
    int k1, k2;
    klee_make_symbolic(&k1, sizeof(k1), "k1");
    klee_make_symbolic(&k2, sizeof(k2), "k2");
    KVMAP* kvm = makeNode(k1, 1);
    replace(k2, 2, &kvm);
    klee_assert(
        ((k1 != k2) && (*get(k1, &kvm) == 1) && length(kvm) == 2)
        ||
        ((k1 == k2) && (*get(k1, &kvm) == 2) && (length(kvm) == 1))
      );
    return 0;
}