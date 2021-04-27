/*
 * Copyright 2018 Amazon.com, Inc. or its affiliates. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"). You may not
 * use this file except in compliance with the License. A copy of the License is
 * located at
 *
 *     http://aws.amazon.com/apache2.0/
 *
 * or in the "license" file accompanying this file. This file is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
 * express or implied. See the License for the specific language governing
 * permissions and limitations under the License.
 */

#ifndef AWS_CRYPTOSDK_EDK_H
#define AWS_CRYPTOSDK_EDK_H

#include "array_list.h"
#include "byte_buf.h"

/*
 * This public interface to the encrypted data key (EDK) objects is provided for
 * developers of CMMs and keyrings only. If you are a user of the AWS Encryption
 * SDK for C and you are not developing your own CMMs and/or keyrings, you do
 * not need to use it and you should not do so.
 */

/**
 * @addtogroup cmm_kr_lowlevel
 * @{
 */

/**
 * A structure representing a single encrypted data key.
 */
struct aws_cryptosdk_edk {
    struct aws_byte_buf provider_id;
    struct aws_byte_buf provider_info;
    struct aws_byte_buf ciphertext;
};

void aws_cryptosdk_edk_list_clear(struct aws_array_list *edk_list);

void aws_cryptosdk_edk_clean_up(struct aws_cryptosdk_edk *edk);
/** @} */ // doxygen group cmm_kr_lowlevel

#endif // AWS_CRYPTOSDK_EDK_H