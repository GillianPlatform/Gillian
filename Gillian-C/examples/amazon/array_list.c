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
// Various predicates for dealing with array lists of edks
/*@

    pred edk_array_list_content_pref(+data, +size, +alloc, content) {
        (size == 0) * (content == []);
        (0 <=# (size - 96)) *
        valid_aws_cryptosdk_edk_ptr(data, alloc, #edk) *
        (content == #edk :: #rest) *
        (#next == data p+ 96) *
        edk_array_list_content_pref(#next, size - 96, alloc, #rest)
    }

    lemma array_list_content_pref_is_array(data, size, alloc) {
        hypothesis:
            edk_array_list_content_pref(#data, #size, #alloc, #content) * (0 <# #size)

        conclusions:
            ARRAY(#data, char, #size, #something)

        proof:
            unfold edk_array_list_content_pref(#data, #size, #alloc, #content);
            if (0 < (#size - 96)) {
                apply array_list_content_pref_is_array(#data p+ 96, #size - 96, #alloc);
                unfold_all i__ptr_add
            } else {
                unfold_all edk_array_list_content_pref
            }
    }

    pred nounfold optPadding(+p, +sz) {
        (sz == 0);
        (1 <=# sz) * ARRAY(p, char, sz, #fill)
    }

    pred edk_array_list_data_content(+data, +prefix_size, +total_size, +alloc, content) {
        (0 <# prefix_size) * (0 <# total_size) * MALLOCED(data, total_size) *
        edk_array_list_content_pref(data, prefix_size, alloc, content) *
        (prefix_size == (96 * (len content))) *
        optPadding(data p+ prefix_size, #rest_size) *
        (#rest_size == (total_size - prefix_size));

        (0 == prefix_size) * (0 <# total_size) * MALLOCED(data, total_size) *
        optPadding(data, total_size) * (content == [])
    }

    lemma edk_array_list_data_is_freeable(data, prefix_size, total_size, alloc) {
        hypothesis:
            edk_array_list_data_content(#data, #prefix_size, #total_size, #alloc, #content)

        conclusions:
            MARRAY(#data, char, #total_size, #whatever)

        proof:
            unfold edk_array_list_data_content(#data, #prefix_size, #total_size, #alloc, #content);
            if (0 < #prefix_size) {
                apply array_list_content_pref_is_array(#data, #prefix_size, #alloc)
            };
            unfold optPadding(#data p+ #prefix_size, #total_size - #prefix_size)
    }

    pred nounfold valid_edk_array_list(+current_size, +length, +item_size, +data, +alloc, content) {
        (current_size == 0) * (length == 0) * (item_size == 96) *
        (data == NULL) * (content == []);

        (item_size == 96) * (0 <=# length) * (0 <# current_size) *
        aws_mul_u64_checked(item_size, length, #required_size) *
        (#required_size <=# current_size) *
        (not (data == NULL)) *
        edk_array_list_data_content(data, #required_size, current_size, alloc, content) *
        (length == len content)
    }

    pred valid_edk_array_list_fields(+fields, alloc, content) {
        (fields == [ alloc, long(#current_size), long(#length), long(#item_size), #data ]) *
        valid_edk_array_list(#current_size, #length, #item_size, #data, alloc, content)
    }

    pred empty_edk_array_list_fields(+fields, alloc) {
        valid_edk_array_list_fields(fields, alloc, [])
    }

    pred nounfold valid_edk_array_list_ptr(+list, alloc, content) {
        (list -> struct aws_array_list { alloc; long(#current_size); long(#length); long(#item_size); #data}) *
        valid_edk_array_list(#current_size, #length, #item_size, #data, alloc, content)
    }

    pred empty_edk_array_list_ptr(+list, alloc) {
        valid_edk_array_list_ptr(list, alloc, [])
    }
*/

/*

lemma empty_edk_array_list_is_valid(fields) {
  hypothesis: (fields == #fields) * empty_edk_array_list_fields(fields)
  conclusions: valid_edk_array_list_fields(#fields, [])
  proof: unfold empty_edk_array_list_fields(fields)
}
*/
void __for_aws_array_list_to_appear_in_csm(struct aws_array_list *list) {
    (void)*list;
}

/*
spec aws_array_list_calc_necessary_size(list, index, necessary_size) {
    requires: (list == #list) * (index == long(#index)) *
              (#index <=# 65534) * (#item_size <=# 65535) *
              (#list -> struct aws_array_list {
                    long(#cur_size); long(#length);
                    long(#item_size); #data }) *
              (necessary_size == #ns) *
              UNDEFS(#ns, 8)
    ensures:  (ret == int(0)) *
              (#res == (#index + 1) * #item_size) *
              (#ns -> long(#res)) *
              (#list -> struct aws_array_list {
                    long(#cur_size); long(#length);
                    long(#item_size); #data })

}*/
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

/*@
axiomatic spec aws_array_list_ensure_capacity(list, index) {
  requires: (list == #list) * (index == long(#index)) *
            (0 <=# #index) *
            (#list -> struct aws_array_list {
              #alloc; long(#current_size); long(#length);
              long(#item_size); #data
            }) *
            valid_edk_array_list(#current_size, #length, #item_size,
                                 #data, #alloc, #content) *
            (#item_size <=# 65535) * (#index <=# 65534) *
            default_allocator(#alloc)
  ensures:  (((#index + 1) * #item_size) <=# #new_size) *
            (#list -> struct aws_array_list {
              #alloc; long(#new_size); long(#length);
              long(#item_size); #new_data
            }) *
            valid_edk_array_list(#new_size, #length, #item_size,
                                 #new_data, #alloc, #content) *
            default_allocator(#alloc) *
            (ret == int(0))
}
*/
int aws_array_list_ensure_capacity(struct aws_array_list *list, size_t index) {
    // AWS_PRECONDITION(aws_array_list_is_valid(list));
    size_t necessary_size;
    if (aws_array_list_calc_necessary_size(list, index, &necessary_size)) {
        // AWS_POSTCONDITION(aws_array_list_is_valid(list));
        return AWS_OP_ERR;
    }

    if (list->current_size < necessary_size) {
        if (!list->alloc) {
            // AWS_POSTCONDITION(aws_array_list_is_valid(list));
            return aws_raise_error(AWS_ERROR_INVALID_INDEX);
        }

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

        void *temp = aws_mem_acquire(list->alloc, new_size);

        if (!temp) {
            // AWS_POSTCONDITION(aws_array_list_is_valid(list));
            return AWS_OP_ERR;
        }

        GILLIAN(
            "if (! (#length = 0)) { "
            "   assert [[bind #x]] valid_aws_cryptosdk_edk_ptr(#data, #alloc, #x); "
            "   branch (#length == 1);"
            "   unfold valid_aws_cryptosdk_edk_ptr(#data, #alloc, #x) "
            "}");
        if (list->data) {
            memcpy(temp, list->data, list->current_size);
            aws_mem_release(list->alloc, list->data);
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

/*@ axiomatic spec aws_array_list_set_at(list, val, index) {
    requires: (list == #list) * (index == long(#index)) *
              (val == #val) * (0 <=# #index) *
              (#index <=# (len #content)) *
              valid_edk_array_list_ptr(#list, #alloc, #content) *
              valid_aws_cryptosdk_edk_ptr(#val, #alloc, #edk) *
              (#index <=# 65534) * default_allocator(#alloc)
    ensures:
              (#length == len #content) * (#index <# #length) *
              (#new_content == #fp @ [ #edk ] @ #sp) *
              (#fp == lsub(#content, 0, #index - 1)) *
              (#sp == lsub(#content, #index + 1, (len #content) - #index - 1 )) *
              valid_edk_array_list_ptr(#list, #alloc, #new_content) *
              valid_aws_cryptosdk_edk_ptr(#val, #alloc, #edk) *
              default_allocator(#alloc) * (ret == int(0));

              (#index == (len #content)) *
              valid_edk_array_list_ptr(#list, #alloc, #content @ [ #edk ]) *
              valid_aws_cryptosdk_edk_ptr(#val, #alloc, #edk) *
              default_allocator(#alloc) * (ret == int(0))
} */
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

/*@
axiomatic spec aws_array_list_push_back(list, val) {
    requires: (list == #list) * (val == #val) *
              valid_edk_array_list_ptr(#list, #alloc, #content) *
              default_allocator(#alloc) *
              valid_aws_cryptosdk_edk_ptr(#val, #alloc, #edk) *
              ((len #content) <=# 65534)

    ensures: (list == #list) * (val == #val) *
             valid_edk_array_list_ptr(#list, #alloc, #content @ [#edk]) *
             default_allocator(#alloc) *
             ARRAY(#val, long, 12, #trash) *
             (ret == int(0))
}
*/
int aws_array_list_push_back(struct aws_array_list *list, const void *val) {
    // AWS_PRECONDITION(aws_array_list_is_valid(list));
    // AWS_PRECONDITION(val && AWS_MEM_IS_READABLE(val, list->item_size),
    //                  "Input pointer [val] must point writable memory of "
    //                  "[list->item_size] bytes.");

    int err_code =
        aws_array_list_set_at(list, val, aws_array_list_length(list));

    if (err_code && aws_last_error() == AWS_ERROR_INVALID_INDEX &&
        !list->alloc) {
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
