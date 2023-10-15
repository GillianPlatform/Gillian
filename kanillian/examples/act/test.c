int f(int k) {
  int* p = (int*) malloc(sizeof(int));
  if (k > 10) {
    *p = k;
  }
  // Missing else branch, value could be uninit
  return *p;
}