#ifndef GILLIAN_C_H
#define GILLIAN_C_H

void ASSUME(int boolean);
void ASSERT(int boolean);

#define SIZE_MAX 4294967295LU

#define FRESH_UINT(X)                                                          \
    size_t X = (size_t)__builtin_annot_intval("symb_int", X);                  \
    ASSUME(X > 0 && X <= 4294967295);

#endif /* GILLIAN_C_H */
