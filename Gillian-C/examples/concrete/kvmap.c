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

int* get (int k, KVMAP* kvm)
{
  if (kvm == NULL)
  {
    return NULL;
  }
  else if (kvm->key == k)
  {
    return &(kvm->value);
  }
  else
  {
    return get(k, kvm->next);
  }
}

int main()
{
  KVMAP* kvm = makeNode(3, 5);
  kvm = put(5, 9, kvm);
  kvm = put(8, 10, kvm);
  int* shouldBeNull = get(6, kvm);
  int shouldbe9 = *(get(5, kvm));
  int shouldBeTrue = (shouldbe9 == 9) && (shouldBeNull == NULL);
  return !shouldBeTrue;
}
