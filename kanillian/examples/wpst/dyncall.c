int zero(int a) {
  return a;
}

int one(int a) {
  return a + 1;
}

int two(int a) {
  return a + 2;
}

int __fresh_int();

int main() {
  int a = __fresh_int();
  __CPROVER_assume(a == 0 || a == 1);
  int (*fun_ptr)(int);
  switch (a) {
    case 0: 
      fun_ptr = &zero;
      break;
    
    case 1: 
      fun_ptr = &one;
      break;
    
    default: 
      fun_ptr = &two;
      break;
  }
  int b = (*fun_ptr)(a);
  __CPROVER_assert(b == a || b == a + 1, "called two??");
  return 0;
}