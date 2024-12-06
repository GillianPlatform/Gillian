int main() {
  int x = __nondet_int();
  __CPROVER_assume(x > -100);
  __CPROVER_assume(x < 100);
  if (x < 0) {
    x = x * x;
  }
  __CPROVER_assert(x >= 0, "x >= 0");
  return 0;
}
