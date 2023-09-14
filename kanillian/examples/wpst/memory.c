int main() {
  int x = __nondet();
  __CPROVER_assume(x >= 0);
  int* z = &x;
  __CPROVER_assert(*z <= 100, "x is less than 100");
  return 0;
}