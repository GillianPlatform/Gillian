#include <stdlib.h>

typedef struct kvnode {
    int key;
    int value;
    struct kvnode* next;
} KVMAP;

KVMAP* a_makeNode(int k, int v) {
  KVMAP* new_node = malloc(sizeof(KVMAP));
  new_node->key = k;
  new_node->value = v;
  new_node->next = NULL;
  return new_node;
}

KVMAP* b_put(int k, int v, KVMAP* kvm) {
  if (kvm == NULL) {
    return a_makeNode(k, v);
  } else if (kvm->key == k) {
    kvm->value = v;
    return kvm;
  } else {
    KVMAP* rec = b_put(k, v, kvm->next);
    kvm->next = rec;
    return kvm;
  }
}

int* b_get (int k, KVMAP* kvm)
{
  if (kvm == NULL)
  {
    return NULL;
  }
  else if (kvm->key == k)
  {
    int* r = malloc(sizeof(int));
    *r = kvm->value;
    return r;
  }
  else
  {
    return b_get(k, kvm->next);
  }
}