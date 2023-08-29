#include "edk.h"

// aws_cryptosdk_edk_clean_up(edk) cleans the deserialised edk edk
void aws_cryptosdk_edk_clean_up(struct aws_cryptosdk_edk *edk) {
    aws_byte_buf_clean_up(&edk->provider_id);
    aws_byte_buf_clean_up(&edk->provider_info);
    aws_byte_buf_clean_up(&edk->ciphertext);
}

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