#include "edk.h"

void __for_edk_struct_to_appear(struct aws_cryptosdk_edk edk) { (void)edk; }

/*@
pred nounfold valid_aws_cryptosdk_edk_ptr(+edk, alloc, edk_content) {
  (edk -> struct aws_cryptosdk_edk {
    #provider_id;
    #provider_info;
    #ciphertext
  }) *
  valid_aws_byte_buf_fields(
    #provider_id, len #prov_id_content, len #prov_id_content,
    #prov_id_buffer, alloc, #prov_id_content) *
  valid_aws_byte_buf_fields(
    #provider_info, len #prov_info_content, len #prov_info_content,
    #prov_info_buffer, alloc, #prov_info_content) *
  valid_aws_byte_buf_fields(
    #ciphertext, len #ct_content, len #ct_content,
    #ct_buffer, alloc, #ct_content
  ) *
  (edk_content == [#prov_id_content, #prov_info_content, #ct_content])
}

pred nounfold empty_aws_cryptosdk_edk_ptr(+edk) {
  (edk -> struct aws_cryptosdk_edk {
    #provider_id;
    #provider_info;
    #cipher_text
  }) *
  empty_aws_byte_buf_fields(#provider_id) *
  empty_aws_byte_buf_fields(#provider_info) *
  empty_aws_byte_buf_fields(#ciphertext)
}
*/

// Not using this spec, but it is valid
/* spec aws_cryptosdk_edk_clean_up(edk) {
  requires: (edk == #edk) *
            valid_aws_cryptosdk_edk_ptr(#edk, #alloc, #edk_content) *
            default_allocator(#alloc)
  ensures: empty_aws_cryptosdk_edk_ptr(#edk) *
           default_allocator(#alloc)
}*/
void aws_cryptosdk_edk_clean_up(struct aws_cryptosdk_edk *edk) {
    if (edk->provider_id.allocator)
        aws_byte_buf_clean_up(&edk->provider_id);
    if (edk->provider_info.allocator)
        aws_byte_buf_clean_up(&edk->provider_info);
    if (edk->ciphertext.allocator)
        aws_byte_buf_clean_up(&edk->ciphertext);
}

/*@ axiomatic spec aws_cryptosdk_edk_list_clear(edk_list) {
    requires: (edk_list == #edk_list) *
              valid_edk_array_list_ptr(#edk_list, #alloc, #content) *
              default_allocator(#alloc)
    ensures:  empty_edk_array_list_ptr(#edk_list, #alloc) *
              default_allocator(#alloc)
}

pred prefix_start_in_invariant(+data, +key_idx, prefix_start) {
  (data == NULL) * (prefix_start == NULL);
  (data == ptr(#dloc, 0)) * (prefix_start == (ptr(#dloc, 0) p+ (96 * key_idx)))
}
*/
void aws_cryptosdk_edk_list_clear(struct aws_array_list *edk_list) {
    size_t num_keys = edk_list->length;
    // bind the data pointer to a logical variable
    __builtin_annot(
        "assert [[bind #current_size, #num_keys, #data]] "
        "(#edk_list -> struct aws_array_list { #alloc; long(#current_size); long(#num_keys); long(96); #data }) * "
        "(num_keys == long(#num_keys))");
    __builtin_annot(
        "invariant: [[bind key_idx, #key_idx, #rem_sz, #r, #edk, #trash, #prefix_start]] "
        "(key_idx == long(#key_idx)) * (0 <=# #key_idx) * "
        "(num_keys == long(#num_keys)) * "
        "(#edk_list -> struct aws_array_list { #alloc; long(#current_size); long(#num_keys); long(96); #data }) * "
        "default_allocator(#alloc) * (#num_keys == len #content) * "
        "optPadding(#data, #key_idx * 96) * "
        "prefix_start_in_invariant(#data, #key_idx, #prefix_start) * "
        "(#rem_sz == ((#num_keys - #key_idx) * 96)) * "
        "edk_array_list_content_pref(#prefix_start, #rem_sz, #alloc, #r) * "
        // csharpminor trickery, edk is hoisted.
        // We don't care about what it contains when reaching the invariant.
        "(edk == [#l_edk, 8]) * (#edk == ptr(#l_edk, 0)) * ARRAY(#edk, long, 1, [ #trash ])");
    for (size_t key_idx = 0; key_idx < num_keys; ++key_idx) {
        struct aws_cryptosdk_edk *edk;
        if (!aws_array_list_get_at_ptr(edk_list, (void **)&edk, key_idx)) {
            __builtin_annot(
                "unfold edk_array_list_content_pref(#prefix_start, #rem_sz, #alloc, #r)");
            __builtin_annot("unfold default_allocator(#alloc)");
            aws_cryptosdk_edk_clean_up(edk);
        }
        __builtin_annot("unfold empty_aws_cryptosdk_edk_ptr(#prefix_start)");
        __builtin_annot("unfold optPadding(#data, #key_idx * 96)");
    }
    aws_array_list_clear(edk_list);
    __builtin_annot("unfold default_allocator(#alloc)");
}