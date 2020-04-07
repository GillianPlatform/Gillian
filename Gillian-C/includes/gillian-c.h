#ifndef GILLIAN_C_H
#define GILLIAN_C_H

void ASSUME(int boolean);

void ASSERT(int boolean);

/* Silence the "unsupported compiler" from Apple's implementation */
#ifndef __GNUC__
#define __GNUC__ 4
#endif /* __GNUC__ */

#endif /* GILLIAN_C_H */
