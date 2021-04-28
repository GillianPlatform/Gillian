/**
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0.
 */
#include "error.h"

/* We use the same stubs as the cbmc stubs that are in the original aws-c-common repo. We don't need error handlers */

static int tl_last_error = 0;

// Various predicates for describing errors
/*@

    pred any_aws_last_error() {
        (error__tl_last_error -g> int(#trash))
    }

    pred aws_last_error_is(err) {
        (error__tl_last_error -g> int(err))
    }

    pred aws_last_error_is_SHORT_BUF() {
        aws_last_error_is(3)
    }

    pred aws_last_error_is_BAD_CIPHERTEXT() {
        aws_last_error_is(8192)
    }
*/

// We override the original aws_raise_error_private implementation to avoid
// error handler functions (unnecessary for the verification process).
void aws_raise_error_private(int err) { tl_last_error = err; }

int aws_raise_error(int err) {
    /*
     * Certain static analyzers can't see through the out-of-line call to aws_raise_error,
     * and assume that this might return AWS_OP_SUCCESS. We'll put the return inline just
     * to help with their assumptions.
     */
    aws_raise_error_private(err);

    return AWS_OP_ERR;
}

// We override the original aws_last_error implementation similarly
int aws_last_error(void) { return tl_last_error; }
