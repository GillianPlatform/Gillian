#include <stdlib.h>
#include <string.h>

#define DEFAULT_CAPACITY 8
#define EXPENSION_FACTOR 2

struct vector_s {
    size_t size;
    size_t capacity;
    int *buffer;
};

typedef struct vector_s Vector;

/*@
    pred nounfold vector(+p, cap, alpha) {
        p -m> struct vector_s { long(0); long(0); NULL } *
        (cap == 0) * (alpha == nil);
    
        p -m> struct vector_s { long(#size); long(cap); #buffer } *
        (0 <# cap) * (#size <=# cap) *
        MARRAY(#buffer, int, cap, #raw_content) *
        (len #raw_content == cap) *
        (#raw_content == (alpha @ #rest)) *
        (len alpha == #size)
        
    }
*/

/*@ spec vector_new_capacity(capacity) {
    requires: (capacity == long(#capacity)) * (0 <=# #capacity)
    ensures: vector(ret, #capacity, nil)
}*/
Vector* vector_new_capacity(size_t capacity) {
    Vector* vec = malloc(sizeof(Vector));
    if (capacity) {
        vec->buffer = malloc(sizeof(int) * capacity);
        vec->size = 0;
        vec->capacity = capacity;
    } else {
        vec->buffer = NULL;
        vec->size = 0;
        vec->capacity = 0;
    }
    return vec;
}

/*@ spec vector_len(vec) {
    requires: (vec == #vec) * vector (#vec, #capacity, #content)
    ensures: vector(#vec, #capacity, #content) * (#sz == len(#content)) * (ret == long(#sz))
}*/
size_t vector_len(Vector *vec) {
    return vec->size;
}

/*@ spec vector_new() {
    requires: emp
    ensures: vector(ret, 8, nil)
}*/
Vector *vector_new() { return vector_new_capacity(DEFAULT_CAPACITY); }

/*@ spec vector_realloc_buffer(vec, new_capacity) {
    requires: (vec == #vec) * (new_capacity == long(#nc)) *
              vector(#vec, #cap, #alpha) * (#cap <=# #nc)
    ensures: vector(#vec, #nc, #alpha)
}*/
void vector_realloc_buffer(Vector *vec, size_t new_capacity) {
    if (new_capacity) {
        int *new_buffer = (int *)calloc(new_capacity, sizeof(int));
        if (vec->size) {
            memcpy((void *)new_buffer, (void *)vec->buffer, (vec->size) * sizeof(int));
        }
        if (vec->capacity) {
            free(vec->buffer);
        }
        vec->buffer = new_buffer;
        vec->capacity = new_capacity;
    }
}