struct S {
  int x;
};

struct S get(struct S s, struct S t) {
  struct S ret = { s.x + t.x };
  return ret;
}


int main() {
  struct S t = { __nondet_int() };
  struct S s = { __nondet_int() };
  struct S z = get(s, t);
  __CPROVER_assert(z.x == 0, "trivial");
  return 0;
}