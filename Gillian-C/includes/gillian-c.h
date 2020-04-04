void ASSUME(int boolean);

void ASSERT(int boolean);

/* Silence the "unsupported compiler" from apple's implementation */
#ifndef __GNUC__
#define __GNUC__ 4
#endif