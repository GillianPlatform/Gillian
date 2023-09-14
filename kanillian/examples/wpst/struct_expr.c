struct A {
    int x;
    int y;
};

int sum(struct A* a) {
    return a->x + a->y;
}

int sum_plus_2(int x, int y) {
    struct A a = {2, x + y};
    return sum(&a);
}

int main() {
  int a = __nondet_int();
  int b = __nondet_int();
  __CPROVER_assert(sum_plus_2(a, b) == 2, "aaa");
  return 0;
}