#include "edk.h"

void __for_edk_struct_to_appear(struct aws_cryptosdk_edk edk) { (void)edk; }

// Predicates describing pointers to valid and empty EDKs
/*@
    pred nounfold valid_aws_cryptosdk_edk_ptr(+edk, alloc, edk_content) {
        (edk -> struct aws_cryptosdk_edk {
            #provider_id;
            #provider_info;
            #ciphertext
        }) *
        valid_aws_byte_buf_fields(#provider_id, len #prov_id_content, len #prov_id_content, #prov_id_buffer, alloc, #prov_id_content) *
        valid_aws_byte_buf_fields(#provider_info, len #prov_info_content, len #prov_info_content, #prov_info_buffer, alloc, #prov_info_content) *
        valid_aws_byte_buf_fields(#ciphertext, len #ct_content, len #ct_content, #ct_buffer, alloc, #ct_content) *
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

// aws_cryptosdk_edk_clean_up(edk) cleans the deserialised edk edk
void aws_cryptosdk_edk_clean_up(struct aws_cryptosdk_edk *edk) {
    if (edk->provider_id.allocator)
        aws_byte_buf_clean_up(&edk->provider_id);
    if (edk->provider_info.allocator)
        aws_byte_buf_clean_up(&edk->provider_info);
    if (edk->ciphertext.allocator)
        aws_byte_buf_clean_up(&edk->ciphertext);
}


// aws_cryptosdk_edk_list_clear(edk_list) resets the list of edks edk_list
/*@
    axiomatic spec aws_cryptosdk_edk_list_clear(edk_list) {
        requires:
            (edk_list == #edk_list) *
            valid_edk_array_list_ptr(#edk_list, #alloc, #content) *
            default_allocator(#alloc)

        ensures:
            empty_edk_array_list_ptr(#edk_list, #alloc) *
            default_allocator(#alloc)
    }
*/
void aws_cryptosdk_edk_list_clear(struct aws_array_list *edk_list) {
    size_t num_keys = edk_list->length;

    for (size_t key_idx = 0; key_idx < num_keys; ++key_idx) {
        struct aws_cryptosdk_edk *edk;
        if (!aws_array_list_get_at_ptr(edk_list, (void **)&edk, key_idx)) {
            aws_cryptosdk_edk_clean_up(edk);
        }
    }
    aws_array_list_clear(edk_list);
}