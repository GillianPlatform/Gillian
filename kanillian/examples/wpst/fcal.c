int __nondet_with_assume() {
  int x = __nondet();
  __CPROVER_assume(x >= 0);
  return x;
}

int main() {
  int x = __nondet_with_assume();
  __CPROVER_assert(x <= 100, "x is less than 100");
  return 0;
}