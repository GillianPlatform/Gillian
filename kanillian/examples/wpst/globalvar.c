static int x;
static long y;

int __nondet_int();
int __nondet_long();


int assign_x() {
  x = __nondet_int();
  __CPROVER_assume(x >= 10 && x <= 30);
  return 0;
}

int assign_y() {
  y = x - 1;
  return 0;
}

int main(void) {
  __CPROVER_assert(x >= y, "cool");
  return 0;
}