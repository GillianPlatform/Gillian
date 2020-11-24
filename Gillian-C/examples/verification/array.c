#include <stdlib.h>
#include <string.h>

#define DEFAULT_CAPACITY 8
#define EXPENSION_FACTOR 2

struct array_s {
    size_t size;
    size_t capacity;
    int *buffer;
};

typedef struct array_s Array;

/*@
    pred nounfold array(+p, cap, alpha) {
        p -m> struct array_s { long(#size); long(cap); #buffer } *
        (#size == len(alpha)) *
        (1 <=# cap) *
        ARRAY(#buffer, int, #size, alpha) *
        ZEROS(#buffer p+ (4 * #size), (4 * cap) - (4 * #size)) *
        MALLOCED(#buffer, (4 * cap))
    }
*/

/*@ spec array_new_capacity(capacity) {
    requires: (capacity == long(#capacity)) * (1 <=# #capacity)
    ensures: array(ret, #capacity, nil)
}*/
Array *array_new_capacity(size_t capacity) {
    Array *arr = (Array *)malloc(sizeof(Array));
    arr->size = 0;
    arr->capacity = capacity;
    arr->buffer = (int *)calloc(capacity, sizeof(int));
    return arr;
}

/*@ spec array_new() {
    requires: emp
    ensures: array(ret, 8, nil)
}*/
Array *array_new() { return array_new_capacity(DEFAULT_CAPACITY); }

/* spec array_realloc_buffer(arr, new_capacity) {
    requires: (arr == #arr) * (new_capacity == long(#nc)) *
              array(#arr, #cap, #alpha)
    ensures: array(#arr, #new_capacity, #alpha)
}*/
void array_realloc_buffer(Array *arr, size_t new_capacity) {
    int *new_buffer = (int *)calloc(new_capacity, sizeof(int));
    memcpy((void *)new_buffer, (void *)arr->buffer, (arr->size) * sizeof(int));
    free(arr->buffer);
    arr->buffer = new_buffer;
}

/*@ spec array_append(arr, v) {
    requires: (arr == #arr) *
              (1 <=# #capacity) *
              array(#arr, #capacity, #alpha) *
              (((len #alpha) + 1) <=# #capacity) *
              (v == int(#v))
    ensures: array(#arr, #capacity, #alpha @ [int(#v)])
}*/
void array_append(Array *arr, int v) {
    size_t size = arr->size;
    if (size == arr->capacity) {
        array_realloc_buffer(arr, 2 * arr->capacity);
    };
    arr->buffer[size] = v;
    arr->size = size + 1;
}