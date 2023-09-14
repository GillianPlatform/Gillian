int main() {
  int x = __nondet();
  __CPROVER_assume(x >= 0);
  __CPROVER_assert(x <= 100, "x is less than 100");
  return 0;
}