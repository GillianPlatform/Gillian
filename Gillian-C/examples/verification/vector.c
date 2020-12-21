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
        p -m> struct vector_s { long(#size); long(cap); #buffer } *
        (#size == len(alpha)) *
        (1 <=# cap) *
        ARRAY(#buffer, int, #size, alpha) *
        ZEROS(#buffer p+ (4 * #size), (4 * cap) - (4 * #size)) *
        MALLOCED(#buffer, (4 * cap))
    }
*/

/*@ spec vector_new_capacity(capacity) {
    requires: (capacity == long(#capacity)) * (1 <=# #capacity)
    ensures: vector(ret, #capacity, nil)
}*/
Vector *vector_new_capacity(size_t capacity) {
    Vector *vec = (Vector *)malloc(sizeof(Vector));
    vec->size = 0;
    vec->capacity = capacity;
    vec->buffer = (int *)calloc(capacity, sizeof(int));
    return vec;
}

/*@ spec vector_len(vec) {
    requires: (vec == #vec) * vector (#vec, #capacity, #content)
    ensures: vector(#vec, #capacity, #content) * (ret == len(#content))
}*/
size_t vector_len(Vector *vec) { return vec->size; }

/*@ spec vector_new() {
    requires: emp
    ensures: vector(ret, 8, nil)
}*/
Vector *vector_new() { return vector_new_capacity(DEFAULT_CAPACITY); }

/* spec vector_realloc_buffer(vec, new_capacity) {
    requires: (vec == #vec) * (new_capacity == long(#nc)) *
              vector(#vec, #cap, #alpha)
    ensures: vector(#vec, #new_capacity, #alpha)
}*/
void vector_realloc_buffer(Vector *vec, size_t new_capacity) {
    int *new_buffer = (int *)calloc(new_capacity, sizeof(int));
    memcpy((void *)new_buffer, (void *)vec->buffer, (vec->size) * sizeof(int));
    free(vec->buffer);
    vec->buffer = new_buffer;
}

/*@ spec vector_append(vec, v) {
    requires: (vec == #vec) *
              (1 <=# #capacity) *
              vector(#vec, #capacity, #alpha) *
              (((len #alpha) + 1) <=# #capacity) *
              (v == int(#v))
    ensures: vector(#vec, #capacity, #alpha @ [int(#v)])
}*/
void vector_append(Vector *vec, int v) {
    size_t size = vec->size;
    if (size == vec->capacity) {
        vector_realloc_buffer(vec, 2 * vec->capacity);
    };
    vec->buffer[size] = v;
    vec->size = size + 1;
}