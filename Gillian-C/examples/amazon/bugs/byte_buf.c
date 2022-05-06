#include "../byte_buf.h"

/*@

pure pred invalid_read(read_len, cursor_len) {
  cursor_len <# read_len;
  2147483647 <# read_len;
  2147483647 <# cursor_len
}

pure pred valid_read(read_len, cursor_len) {
  (read_len <=# cursor_len) *
  (read_len <=# 2147483647) *
  (cursor_len <=# 2147483647)
}

pred nounfold valid_aws_byte_cursor_ptr(+cur, length, buffer: List, alpha) {
  (cur -> struct aws_byte_cursor { long(0); buffer }) *
  (length == 0) * (alpha == nil);

  (cur -> struct aws_byte_cursor { long(length); buffer }) * (0 <# length) *
  ARRAY(buffer, char, length, alpha) * (length == len alpha) *
  (length <=# 2147483647)
}

spec aws_byte_cursor_advance(_res, cursor, length) {
  requires: (_res == #res) * (cursor == #cursor) * (length == long(#length)) *
            (0 <=# #length) * ARRAY(#res, long, 2, #trash) *
            valid_aws_byte_cursor_ptr(#cursor, #cur_len, #buffer, #content)

  ensures: invalid_read(#length, #cur_len) *
           valid_aws_byte_cursor_ptr(#res, 0, NULL, nil) *
           valid_aws_byte_cursor_ptr(#cursor, #cur_len, #buffer, #content);

           valid_read(#length, #cur_len) *
           valid_aws_byte_cursor_ptr(#res, #length, #buffer, #data) *
           valid_aws_byte_cursor_ptr(#cursor, #rest_len,
                                     #buffer p+ #length, #rest) *
           (#length == len #data) *
           (#content == #data @ #rest) *
           (#rest_len == (#cur_len - #length))
}
*/
struct aws_byte_cursor
aws_byte_cursor_advance(struct aws_byte_cursor *const cursor,
                        const size_t length) {
    struct aws_byte_cursor rv;
    if (cursor->len > 2147483647 || length > 2147483647 ||
        length > cursor->len) {
        rv.ptr = NULL;
        rv.len = 0;
    } else {
        rv.ptr = cursor->ptr;
        rv.len = length;

        cursor->ptr += length;
        cursor->len -= length;
        GILLIAN("branch (#length == #cur_len)");
        GILLIAN("branch (#length == 0)");
    }
    return rv;
}