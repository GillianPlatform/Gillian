/**
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0.
 */
#include "string.h"
/*@
import `../logic/StringStruct`;
*/

/*@
  pred valid_aws_string_ptr(+str, alloc, content) {
    m_struct_aws_string_exposing_pointer(str, alloc, long(#len), #bytes) *
    (0 <=# #len) *
    ARRAY(#bytes, char, #len + 1, #bytes_content) *
    (#rawContent == lsub(#bytes_content, 0, #len)) *
    (#bytes_content == #rawContent @ [ 0 ]) *
    toUtf8(#rawContent, content)
  } 
*/

/*@ pred nounfold optBytes(+bytes, +length, content) {
  (length == 0) * (content == nil);
  (0 <# length) * ARRAY(bytes, char, length, content)
}
*/

// additional argument for how compcert handles struct passing
/*@ spec aws_string_new_from_array(allocator, bytes, length) {
  requires: (allocator == #alloc) *
            (bytes == #bytes) * (length == long(#len)) *
            (#len <=# 65535) *
            optBytes(#bytes, #len, #rawContent) *
            toUtf8(#rawContent, #strContent) *
            default_allocator(#alloc)
  ensures:  valid_aws_string_ptr(ret, #alloc, #strContent) *
            optBytes(#bytes, #len, #rawContent) *
            default_allocator(#alloc)
}
*/
struct aws_string *aws_string_new_from_array(struct aws_allocator *allocator,
                                             const uint8_t *bytes,
                                             size_t length) {
    // AWS_PRECONDITION(allocator);
    // AWS_PRECONDITION(AWS_MEM_IS_READABLE(bytes, length));
    size_t malloc_size;
    if (aws_add_size_checked(sizeof(struct aws_string) + 1, length,
                             &malloc_size)) {
        return NULL;
    }
    struct aws_string *str = aws_mem_acquire(allocator, malloc_size);
    if (!str) {
        return NULL;
    }

    /* Fields are declared const, so we need to copy them in like this */
    *(struct aws_allocator **)(&str->allocator) = allocator;
    *(size_t *)(&str->len) = length;
    if (length > 0) {
        memcpy((void *)str->bytes, bytes, length);
    }
    *(uint8_t *)&str->bytes[length] = '\0';
    // AWS_RETURN_WITH_POSTCONDITION(str, aws_string_is_valid(str));
    return str;
}

/*@ spec aws_string_destroy(str) {
    requires: valid_aws_string_ptr(str, #alloc, #strContent) *
              default_allocator(#alloc)
    ensures: default_allocator(#alloc)
    
    OR
    
    requires: (str == NULL)
    ensures: emp
}*/
void aws_string_destroy(struct aws_string *str) {
    if (str && str->allocator) {
        aws_mem_release(str->allocator, str);
    }
}