struct A {
  int x;
};

struct A f() {
  struct A a;
  a.x = __nondet_int();
  __CPROVER_assume(a.x > 0);
  return a;
}

int main() {
  struct A a;
  a = f();
  __CPROVER_assert(a.x >= 1, "cool");
  return 0;
}
