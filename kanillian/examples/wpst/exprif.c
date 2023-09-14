struct S {
  int v;
};

int main() {
  int x = __nondet();
  __CPROVER_assume(x > 4);
  struct S yes = { 1 };
  struct S no = { 0 };
  struct S y;
  y = x < 2 ? yes : no;
  __CPROVER_assert(y.v == 0, "x cannot be smaller than 2");
  return 0;
}