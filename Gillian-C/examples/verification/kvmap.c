#include <stdlib.h>

typedef struct kvnode {
    int key;
    int value;
    struct kvnode* next;
} KVMAP;

/*@ pred KVMap(+o, kvs, keys) {
  (o == NULL) * (kvs == -{ }-) * (keys == -{ }-);

  (o -m> struct kvnode { int(#k); int(#v); #next }) * (not (#next == NULL)) *
  (kvs == -u- (-{ [ #k, #v ] }-, #rkvs)) * (keys == -u- (-{ #k }-, #rkeys)) *
  (not (#k --e-- #rkeys)) * (not ([#k, #v] --e-- #rkvs)) *
  (forall #kp : Num, #vp : Num. [#kp, #vp] --e-- #rkvs => (not (#kp == #k))) *
  KVMap(#next, #rkvs, #rkeys);

  (o -m> struct kvnode { int(#k); int(#v); NULL }) *
  (kvs == -{ [ #k, #v ] }-) * (keys == -{ #k }-)
} */

/*@ spec makeNode(k, v) {
  requires: (k == int(#k)) * (v == int(#v))
  ensures:  KVMap(ret, -{ [ #k, #v ] }-, -{ #k }-)
} */
KVMAP* makeNode(int k, int v) {
  KVMAP* new_node = malloc(sizeof(KVMAP));
  new_node->key = k;
  new_node->value = v;
  new_node->next = NULL;
  return new_node;
}

/*@ spec put(k, v, kvm) {
  requires: (kvm == #kvm) * (k == int(#k)) * (v == int(#v)) * KVMap(#kvm, #kvs, #keys) * (not (#k --e-- #keys))
  ensures:  KVMap(ret, -u- (-{ [#k, #v] }-, #kvs), -u-(-{ #k }-, #keys))
} */
/*
  // For some reason, the following doesn't really work ?
  OR

  [key_exists: #w : Num, #rkvs : Set]
  requires: (kvm == #kvm) * (k == int(#k)) * (v == int(#v)) *
            KVMap(#kvm, #kvs, #keys) * (#k --e-- #keys) * (#kvs == -u-(-{ [#k, #w] }-, #rkvs))
  ensures:  KVMap(ret, -u- (-{ [#k, #v] }-, #rkvs), #keys)
} */
KVMAP* put(int k, int v, KVMAP* kvm) {
  if (kvm == NULL) {
    return makeNode(k, v);
  } else if (kvm->key == k) {
    kvm->value = v;
    return kvm;
  } else {
    __builtin_annot("assert [[exists #next]] #kvm -m> struct kvnode { #somekey; #somevalue; #next }");
    __builtin_annot("if (! (#next = NULL)) { unfold KVMap(#next, #nextkvs, #nextkeys) }");
    KVMAP* rec = put(k, v, kvm->next);
    __builtin_annot("unfold KVMap(rec, #reckvs, #reckeys)");
    kvm->next = rec;
    return kvm;
  }
}

/*@ spec get(k, kvm) {
  requires: (kvm == #kvm) * (k == int(#k)) * KVMap(#kvm, #kvs, #keys) * (not (#k --e-- #keys))
  ensures:  KVMap(#kvm, #kvs, #keys) * (ret == NULL)

  OR

  [key_exists: #v : Num]
  requires: (kvm == #kvm) * (k == int(#k)) * KVMap(#kvm, #kvs, #keys) * (#k --e-- #keys) *
            ([#k, #v] --e-- #kvs)
  ensures:  KVMap(#kvm, #kvs, #keys) * (ret -m> int(#v))
}
*/
int* get (int k, KVMAP* kvm)
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
    return get(k, kvm->next);
  }
}