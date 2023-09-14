struct A {
  int a;
  int b;
};

int main() {
  struct A a = { 1, 2};
  struct A b = a;
  __CPROVER_assert(a.a == b.a && a.b == b.b, "should be equal!");
  return 0;
}