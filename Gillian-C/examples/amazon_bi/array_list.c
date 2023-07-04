#include "array_list.h"
#include "edk.h"
#include "error.h"

/*  One can look at the following validity check to understand
    the following predicates :
    https://github.com/awslabs/aws-c-common/blob/bb797381f3468e6f076e53eddbb399a99f54f67b/include/aws/common/array_list.inl#L82

    The size of an edk is 96
*/
/*
    Unfortunately, we cannot go fully polymorphic here, because we don't have a
    core predicate for arbitrary structs. It has to do with the lack of
    structures in Csharpminor, but writing a compiler straight from C would let
    us do that better.
*/

// We're not using that spec either, because the function is very simple.
int aws_array_list_calc_necessary_size(struct aws_array_list *list,
                                       size_t index, size_t *necessary_size) {
    // AWS_PRECONDITION(aws_array_list_is_valid(list));
    size_t index_inc;
    if (aws_add_size_checked(index, 1, &index_inc)) {
        // AWS_POSTCONDITION(aws_array_list_is_valid(list));
        return AWS_OP_ERR;
    }

    if (aws_mul_size_checked(index_inc, list->item_size, necessary_size)) {
        // AWS_POSTCONDITION(aws_array_list_is_valid(list));
        return AWS_OP_ERR;
    }
    // AWS_POSTCONDITION(aws_array_list_is_valid(list));
    return AWS_OP_SUCCESS;
}

int aws_array_list_ensure_capacity(struct aws_array_list *list, size_t index) {
    // AWS_PRECONDITION(aws_array_list_is_valid(list));
    size_t necessary_size;
    if (aws_array_list_calc_necessary_size(list, index, &necessary_size)) {
        // AWS_POSTCONDITION(aws_array_list_is_valid(list));
        return AWS_OP_ERR;
    }

    if (list->current_size < necessary_size) {

        /* this will double capacity if the index isn't bigger than what the
         * next allocation would be, but allocates the exact requested size if
         * it is. This is largely because we don't have a good way to predict
         * the usage pattern to make a smart decision about it. However, if the
         * user
         * is doing this in an iterative fashion, necessary_size will never be
         * used. */
        size_t next_allocation_size = list->current_size << 1;
        size_t new_size = next_allocation_size > necessary_size
                              ? next_allocation_size
                              : necessary_size;

        if (new_size < list->current_size) {
            /* this means new_size overflowed. The only way this happens is on a
             * 32-bit system where size_t is 32 bits, in which case we're out of
             * addressable memory anyways, or we're on a 64 bit system and we're
             * most certainly out of addressable memory. But since we're simply
             * going to fail fast and say, sorry can't do it, we'll just tell
             * the user they can't grow the list anymore. */
            // AWS_POSTCONDITION(aws_array_list_is_valid(list));
            return aws_raise_error(AWS_ERROR_LIST_EXCEEDS_MAX_SIZE);
        }

        void *temp = malloc(new_size);

        if (!temp) {
            // AWS_POSTCONDITION(aws_array_list_is_valid(list));
            return AWS_OP_ERR;
        }

        if (list->data) {
            memcpy(temp, list->data, list->current_size);
            free(list->data);
        }
        list->data = temp;
        list->current_size = new_size;
    }

    return AWS_OP_SUCCESS;
}

size_t aws_array_list_length(const struct aws_array_list *list) {
    /*
     * This assert teaches clang-tidy and friends that list->data cannot be null in a non-empty
     * list.
     */
    // AWS_FATAL_PRECONDITION(!list->length || list->data);
    // AWS_PRECONDITION(aws_array_list_is_valid(list));
    size_t len = list->length;
    // AWS_POSTCONDITION(aws_array_list_is_valid(list));
    return len;
}

int aws_array_list_set_at(struct aws_array_list *list, const void *val,
                          size_t index) {
    // AWS_PRECONDITION(aws_array_list_is_valid(list));
    // AWS_PRECONDITION(
    //     val && AWS_MEM_IS_READABLE(val, list->item_size),
    //     "Input pointer [val] must point readable memory of
    // [list->item_size]
    //     bytes.");

    if (aws_array_list_ensure_capacity(list, index)) {
        // AWS_POSTCONDITION(aws_array_list_is_valid(list));
        return AWS_OP_ERR;
    };

    memcpy((void *)((uint8_t *)list->data + (list->item_size * index)), val,
           list->item_size);

    if (index >= aws_array_list_length(list)) {
        if (aws_add_size_checked(index, 1, &list->length)) {
            // AWS_POSTCONDITION(aws_array_list_is_valid(list));
            return AWS_OP_ERR;
        }
    }

    // AWS_POSTCONDITION(aws_array_list_is_valid(list));
    return AWS_OP_SUCCESS;
}

int aws_array_list_push_back(struct aws_array_list *list, const void *val) {
    // AWS_PRECONDITION(aws_array_list_is_valid(list));
    // AWS_PRECONDITION(val && AWS_MEM_IS_READABLE(val, list->item_size),
    //                  "Input pointer [val] must point writable memory of "
    //                  "[list->item_size] bytes.");

    int err_code =
        aws_array_list_set_at(list, val, aws_array_list_length(list));

    if (err_code && aws_last_error() == AWS_ERROR_INVALID_INDEX) {
        // AWS_POSTCONDITION(aws_array_list_is_valid(list));
        return aws_raise_error(AWS_ERROR_LIST_EXCEEDS_MAX_SIZE);
    }

    // AWS_POSTCONDITION(aws_array_list_is_valid(list));
    return err_code;
}

int aws_array_list_get_at_ptr(const struct aws_array_list *list, void **val,
                              size_t index) {
    // AWS_PRECONDITION(aws_array_list_is_valid(list));
    // AWS_PRECONDITION(val != NULL);
    if (aws_array_list_length(list) > index) {
        *val = (void *)((uint8_t *)list->data + (list->item_size * index));
        // AWS_POSTCONDITION(aws_array_list_is_valid(list));
        return AWS_OP_SUCCESS;
    }
    return aws_raise_error(AWS_ERROR_INVALID_INDEX);
}

void aws_array_list_clear(struct aws_array_list *list) {
    // AWS_PRECONDITION(aws_array_list_is_valid(list));
    if (list->data) {
        // #ifdef DEBUG_BUILD
        //         memset(list->data, AWS_ARRAY_LIST_DEBUG_FILL, list->current_size);
        // #endif
        list->length = 0;
    }
    // AWS_POSTCONDITION(aws_array_list_is_valid(list));
}
