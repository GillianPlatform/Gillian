#ifndef GILLIAN_STDLIB_H
#define GILLIAN_STDLIB_H

#define NULL ((void *)0)

typedef unsigned char uint8_t;

typedef unsigned long size_t;

void *malloc(size_t size);

void *calloc(size_t num, size_t size);

void free(void *ptr);

void *memcpy(void *destination, const void *source, size_t num);

void *memmove(void *destination, const void *source, size_t num);

void *memset(void *ptr, int value, size_t num);

int rand(void);

/* FIXME: unimplemented */
void qsort(void *base, size_t num, size_t size,
           int (*comparator)(const void *, const void *));

#endif /* GILLIAN_STDLIB_H */
