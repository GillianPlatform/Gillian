// This file is inspired by one of the bugs found using Gillian-C in the Collections-C library.

#include <stdlib.h>
#include <string.h>
#define INITIAL_CAPACITY 4

typedef struct Array {
    int *buffer;
    size_t capacity;
    size_t size;
} Array;

void array_new(Array *arr) {
    arr->capacity = INITIAL_CAPACITY;
    arr->buffer = malloc(INITIAL_CAPACITY * sizeof(int));
    arr->size = 0;
}

void push(Array *ar, int value) {
    if (ar->size == ar->capacity) {
        ar->capacity *= 2;
        int *new_buffer = malloc(ar->capacity * sizeof(int));
        memcpy(new_buffer, ar->buffer, ar->size * sizeof(int));
        free(ar->buffer);
        ar->buffer = new_buffer;
    }
    ar->buffer[ar->size++] = value;
}

void remove(Array *ar, size_t index) {
    if (index != ar->size - 1) {
        size_t block_size = (ar->size - index) * sizeof(int);
        memmove(&(ar->buffer[index]), &(ar->buffer[index + 1]), block_size);
    }
    ar->size--;
}

int main() {
    unsigned int init_loops;
    Array arr;
    array_new(&arr);
    for (unsigned int i = init_loops; i > 0; i--)
        push(&arr, i);
    for (unsigned int i = init_loops; i > 0; i--)
        remove(&arr, 0);
    __CPROVER_assert(arr.size == 0, "failed final check");
    return 0;
}