#ifndef AWS_COMMON_ERROR_H
#define AWS_COMMON_ERROR_H

/**
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0.
 */

#define AWS_OP_SUCCESS (0)
#define AWS_OP_ERR (-1)

/* We override & hardcode the values, because the spec comments don't get overriden by the preprocessor.
  We also reduce the amount of variables.
*/
enum aws_common_error {
    AWS_ERROR_SUCCESS = 0,
    AWS_ERROR_OOM = 1,
    AWS_ERROR_UNKNOWN = 2,
    AWS_ERROR_SHORT_BUFFER = 3,
    AWS_ERROR_OVERFLOW_DETECTED = 4,
    AWS_ERROR_UNSUPPORTED_OPERATION = 5,
    AWS_ERROR_INVALID_BUFFER_SIZE = 6,
    AWS_ERROR_INVALID_INDEX = 7,
    AWS_ERROR_LIST_EMPTY = 8,
    AWS_ERROR_DEST_COPY_TOO_SMALL = 9,
    AWS_ERROR_LIST_EXCEEDS_MAX_SIZE = 10,
    AWS_ERROR_INVALID_ARGUMENT = 11,
    AWS_ERROR_C_STRING_BUFFER_NOT_NULL_TERMINATED = 12,
    AWS_ERROR_DIVIDE_BY_ZERO = 13
};

void aws_raise_error_private(int err);

int aws_raise_error(int err);

int aws_last_error(void);

// We also use this file for the encryption-sdk specific errors

/**
 * CryptoSDK specific error codes. Note that we also make use of error codes defined in the aws-c-common library
 */

enum aws_cryptosdk_err {
    /** The ciphertext was malformed or corrupt */
    AWS_CRYPTOSDK_ERR_BAD_CIPHERTEXT = 0x2000,
    /** A function was called on an object in the wrong state */
    AWS_CRYPTOSDK_ERR_BAD_STATE,
    /** No keyrings were able to decrypt the message in question */
    AWS_CRYPTOSDK_ERR_CANNOT_DECRYPT,
    /** An unknown internal error has occurred */
    AWS_CRYPTOSDK_ERR_CRYPTO_UNKNOWN,
    /** KMS returned an error */
    AWS_CRYPTOSDK_ERR_KMS_FAILURE,
    /** Caller attempted to exceed a hard limit */
    AWS_CRYPTOSDK_ERR_LIMIT_EXCEEDED,
    /** Caller attempted to use a name reserved by AWS */
    AWS_CRYPTOSDK_ERR_RESERVED_NAME,
    /** An unsupported format version was encountered on decrypt */
    AWS_CRYPTOSDK_ERR_UNSUPPORTED_FORMAT,
    AWS_CRYPTOSDK_ERR_END_RANGE = 0x2400
};

#endif /* AWS_COMMON_ERROR_H */