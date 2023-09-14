struct A {
  int x;
};

struct A f() {
  struct A a;
  a.x = __nondet_int();
  return a;
}

int main() {
  struct A a;
  a = f();
  __CPROVER_assert(a.x > 0, "that cannot be right");
  return 0;
}

