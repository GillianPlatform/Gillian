#include <gillian-c/gillian-c.h>
#include <stdlib.h>

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

KVMAP* put(int k, int v, KVMAP* kvm) {
  if (kvm == NULL) {
    return makeNode(k, v);
  } else if (kvm->key == k) {
    kvm->value = v;
    return kvm;
  } else {
    KVMAP* rec = put(k, v, kvm->next);
    kvm->next = rec;
    return kvm;
  }
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
    int* r = malloc(sizeof(int));
    *r = (*kvm)->value;
    return r;
  }
  else
  {
    return get(k, &(*kvm)->next);
  }
}

int main() {
    int k1 = __builtin_annot_intval("symb_int", k1);
    int k2 = __builtin_annot_intval("symb_int", k2);
    KVMAP* kvm = makeNode(k1, 1);
    put(k2, 2, kvm);
    ASSERT(
        ((k1 != k2) && (*get(k1, &kvm) == 1) && length(kvm) == 2)
        ||
        ((k1 == k2) && (*get(k1, &kvm) == 2) && (length(kvm) == 1))
      );
    return 0;
}