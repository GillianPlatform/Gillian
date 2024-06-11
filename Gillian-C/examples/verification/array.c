// This file is inspired by one of the bugs found using Gillian-C in the Collections-C library.
#include <gillian-c/gillian-c.h>
#include <stdlib.h>

void abort() { ASSUME(0); }

typedef struct Array {
    int *buffer;
    size_t capacity;
    size_t size;
} Array;

// The following should be in the Gillian-C stdlib
/*@
pred OARRAY(+p, +size, content) {
  (size == 0) * (content == nil);
  (0 <# size)  * ARRAY(p, int, size, content) * ((len content) == size)
}

pred OUNINIT(+p, +size) {
  (size == 0);
  (0 <# size) * UNDEFS(p, size)
}
*/

/*@
pred valid_array(+ar, content) {
  (ar -> struct Array { #buffer; long(#capacity); long(#size) }) *
  (0 <# #capacity) *
  (#size <=# #capacity) *
  OARRAY(#buffer, #size, content) *
  OUNINIT(#buffer p+ (#size * 4), (#capacity - #size) * 4) *
  MALLOCED(#buffer, #capacity * 4)
}
*/

/*@ spec push(ar, value) {
  requires: (ar == #ar) * (value == int(#value)) *
            valid_array(#ar, #content)
  ensures:  valid_array(#ar, #content @ [#value])
}*/
void push(Array *ar, int value) {
    if (ar->size == ar->capacity) {
        if (SIZE_MAX / 2 - 1 < ar->capacity)
            abort();
        ar->capacity *= 2;
        int *new_buffer = malloc(ar->capacity * sizeof(int));
        memcpy(new_buffer, ar->buffer, ar->size * sizeof(int));
        free(ar->buffer);
        ar->buffer = new_buffer;
    }
    ar->buffer[ar->size++] = value;
}

/* spec remove(ar, index) {
  requires: (ar == #ar) * (index == long(#index)) *
            valid_array(#ar, #content) * (0 <=# #index) * (#index <# (len #content))
  ensures:  valid_array(#ar, lsub(#content, 0, #index) @ lsub(#content, #index + 1, (len #content) - #index - 1))
}
*/
void remove(Array *ar, size_t index) {
    if (index != ar->size - 1) {
        size_t block_size = (ar->size - 1 - index) * sizeof(int);
        memmove(&(ar->buffer[index]), &(ar->buffer[index + 1]), block_size);
    }
    ar->size--;
}