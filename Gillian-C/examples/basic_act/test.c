#define NULL 0

int deref(int* x) {
    if (x != NULL) {
        return *x;
    } else {
        return 0;
    }
}

int deref_bug(float* x) {
  return *x ;
}