/*
 * Copyright 2018 Amazon.com, Inc. or its affiliates. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 20 (the "License"). You may not
 * use this file except in compliance with the License. A copy of the License is
 * located at
 *
 *     http://aws.amazon.com/apache20/
 *
 * or in the "license" file accompanying this file. This file is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
 * express or implied. See the License for the specific language governing
 * permissions and limitations under the License.
 */

/*
 This file corresponds to the following commit of the encryption sdk:
 https://github.com/aws/aws-encryption-sdk-c/tree/22151f59f1a3128ecef877ddc491c20f6754af4e
*/

#include "header.h"
#include "base.h"

// Optional value or null
/*@
pred or_NULL(+x, +x_opt) {
    x_opt == x;
    x_opt == NULL
}
*/

// Any valid header
/*@
pred nounfold any_valid_aws_cryptosdk_hdr(+hdr, allocator) {
  (hdr -> struct aws_cryptosdk_hdr {
    allocator;
    #alg_id;
    #frame_len;
    #iv;
    #auth_tag;
    #message_id;
    #enc_ctx;
    #edk_list;
    #auth_len
  }) *
  (not (allocator == NULL)) *
  valid_aws_byte_buf_fields(#iv, #len_iv, #cap_iv, #buf_iv, #iv_alloc, #content_iv) * or_NULL(allocator, #iv_alloc) *
  valid_aws_byte_buf_fields(#auth_tag, #len_auth_tag, #cap_auth_tag, #buf_auth_tag, #auth_tag_alloc, #content_auth_tag) * or_NULL(allocator, #auth_tag_alloc) *
  valid_hash_table_fields(#enc_ctx, allocator, #ctx_content, #ctx_content_utf8) *
  valid_edk_array_list_fields(#edk_list, allocator, #edk_content)
}
*/

// Empty header
/*@
pred nounfold empty_aws_cryptosdk_hdr(+hdr, allocator) {
  (hdr -> struct aws_cryptosdk_hdr {
    allocator;
    int(0);
    int(0);
    #iv;
    #auth_tag;
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
    #enc_ctx;
    #edk_list;
    long(0)
  }) *
  (not (allocator == NULL)) *
  empty_aws_byte_buf_fields(#iv) *
  empty_aws_byte_buf_fields(#auth_tag) *
  empty_hash_table_fields(#enc_ctx, allocator) *
  empty_edk_array_list_fields(#edk_list, allocator)
}
*/

// Deserialised header
/*@
pred nounfold header_struct(+hdr, allocator, alg_id, message_id, enc_ctx_content, edk_content, frame_len, auth_len, iv, auth_tag) {
  (hdr -> struct aws_cryptosdk_hdr {
    allocator;
    int(alg_id);
    int(frame_len);
    #iv;
    #auth_tag;
    message_id;
    #enc_ctx;
    #edk_list;
    long(auth_len)
  }) *
  (not (allocator == NULL)) *
  valid_aws_byte_buf_fields(#iv, len iv, len iv, #iv_buf, allocator, iv) *
  (0 <# len iv) * (not (#iv_buf == NULL)) *
  valid_aws_byte_buf_fields(#auth_tag, len auth_tag, len auth_tag, #auth_buf, allocator, auth_tag) *
  (0 <# len auth_tag) * (not (#auth_buf == NULL)) *
  valid_hash_table_fields(#enc_ctx, allocator, enc_ctx_content, #enc_ctx_utf8) *
  valid_edk_array_list_fields(#edk_list, allocator, edk_content)
}
*/

//
// is_known_type(content_type) returns 1 if the content_type
// is known (framed or non-framed), and 0 otherwise
//
/*@
    spec is_known_type(content_type) {
        requires:
            (content_type == int(#ct))

        ensures:
            CContentType(#ct) * (ret == TRUE);
            BContentType(#ct) * (ret == FALSE)
    }
*/
int is_known_type(uint8_t content_type) {
    switch (content_type) {
    case AWS_CRYPTOSDK_HEADER_CTYPE_FRAMED:
    case AWS_CRYPTOSDK_HEADER_CTYPE_NONFRAMED:
        return 1;
    default:
        return 0;
    }
}

//
// aws_cryptosdk_algorithm_is_known(alg_id) returns 1 if
// the algorithm identified by alg_id is known, and 0 otherwise.
//
/*@
    spec aws_cryptosdk_algorithm_is_known(alg_id) {
        requires:
            (alg_id == int(#alg_id)) *
            CAlgorithmSuite(#alg_id, #stringId, #ivLength, #tagLength)

        ensures:
            CAlgorithmSuite(#alg_id, #stringId, #ivLength, #tagLength) *
            (ret == TRUE)

    OR

        requires:
            (alg_id == int(#alg_id)) *
            BAlgorithmSuite(#alg_id, #errorMessage)

        ensures:
            BAlgorithmSuite(#alg_id, #errorMessage) *
            (ret == FALSE)
    }
*/
int aws_cryptosdk_algorithm_is_known(uint16_t alg_id) {
    switch (alg_id) {
    case ALG_AES256_GCM_HKDF_SHA512_COMMIT_KEY_ECDSA_P384:
    case ALG_AES256_GCM_HKDF_SHA512_COMMIT_KEY:
    case ALG_AES256_GCM_IV12_TAG16_HKDF_SHA384_ECDSA_P384:
    case ALG_AES192_GCM_IV12_TAG16_HKDF_SHA384_ECDSA_P384:
    case ALG_AES128_GCM_IV12_TAG16_HKDF_SHA256_ECDSA_P256:
    case ALG_AES256_GCM_IV12_TAG16_HKDF_SHA256:
    case ALG_AES192_GCM_IV12_TAG16_HKDF_SHA256:
    case ALG_AES128_GCM_IV12_TAG16_HKDF_SHA256:
    case ALG_AES256_GCM_IV12_TAG16_NO_KDF:
    case ALG_AES192_GCM_IV12_TAG16_NO_KDF:
    case ALG_AES128_GCM_IV12_TAG16_NO_KDF:
        return 1;
    default:
        return 0;
    }
}

//
// aws_cryptosdk_algorithm_ivlen(alg_id) returns the iv length
// associated with the algorithm alg_id, or -1 if no such iv length is defined
//
/*@
    spec aws_cryptosdk_algorithm_ivlen(alg_id) {
        requires:
            (alg_id == int(#alg_id)) *
            CAlgorithmSuite(#alg_id, #stringId, #ivLength, #tagLength)

        ensures:
            CAlgorithmSuite(#alg_id, #stringId, #ivLength, #tagLength) *
            (ret == int(#ivLength))

    OR

        requires:
            (alg_id == int(#alg_id)) *
            BAlgorithmSuite(#alg_id, #errorMessage)

        ensures:
            BAlgorithmSuite(#alg_id, #errorMessage) *
            (ret == int(-1))
    }
*/
int aws_cryptosdk_algorithm_ivlen(uint16_t alg_id) {
    if (aws_cryptosdk_algorithm_is_known(alg_id)) {
        // all known algorithms have an IV length of 12 bytes
        return 12;
    }

    return -1;
}

//
// aws_cryptosdk_algorithm_taglen(alg_id) returns the authentication
// tag length associated with the algorithm alg_id, or -1 if no
// such tag length is defined
//
/*@
    spec aws_cryptosdk_algorithm_taglen(alg_id) {
        requires:
            (alg_id == int(#alg_id)) *
            CAlgorithmSuite(#alg_id, #stringId, #ivLength, #tagLength)

        ensures:
            CAlgorithmSuite(#alg_id, #stringId, #ivLength, #tagLength) *
            (ret == int(#tagLength))

    OR

        requires:
            (alg_id == int(#alg_id)) *
            BAlgorithmSuite(#alg_id, #errorMessage)

        ensures:
            BAlgorithmSuite(#alg_id, #errorMessage) *
            (ret == int(-1))
    }
*/
int aws_cryptosdk_algorithm_taglen(uint16_t alg_id) {
    if (aws_cryptosdk_algorithm_is_known(alg_id)) {
        // all known algorithms have a tag length of 16 bytes
        return 16;
    }

    return -1;
}

//
// aws_cryptosdk_hdr_clear(hdr) clears the header hdr
//
/*@
    spec aws_cryptosdk_hdr_clear(hdr) {
        requires:
            (hdr == #hdr) *
            any_valid_aws_cryptosdk_hdr(#hdr, #alloc) *
            default_allocator(#alloc)

        ensures:
            empty_aws_cryptosdk_hdr(#hdr, #alloc) *
            default_allocator(#alloc)
    }
*/
void aws_cryptosdk_hdr_clear(struct aws_cryptosdk_hdr *hdr) {
    /* hdr->alloc is preserved */
    hdr->alg_id = 0;
    hdr->frame_len = 0;

    aws_byte_buf_clean_up(&hdr->iv);
    aws_byte_buf_clean_up(&hdr->auth_tag);

    memset(&hdr->message_id, 0, sizeof(hdr->message_id));

    aws_cryptosdk_enc_ctx_clear(&hdr->enc_ctx);
    aws_cryptosdk_edk_list_clear(&hdr->edk_list);

    hdr->auth_len = 0;

    GILLIAN("unfold_all valid_aws_byte_buf_ptr");
    GILLIAN("unfold_all empty_aws_byte_buf_ptr");
    GILLIAN("unfold_all valid_hash_table_ptr");
}

//
// parse_edk(allocator, edk, cur) deserialises an encrypted data key at cur,
// storing the resulting information in edk
//
/*@
    spec parse_edk(allocator, edk, cur) {
        requires:
            (allocator == #alloc) * (edk == #edk) * (cur == #cur) *
            default_allocator(#alloc) *
            Element(#definition, #content, 0, 3, #edk_content, #element_length) *
            valid_aws_byte_cursor_ptr(#cur, #total_length, #buffer, #content) *
            (not (#buffer == NULL)) *
            any_aws_last_error() *
            ARRAY(#edk, long, 12, #trash) *

            (#content == #edk_raw_content @ #rest) *
            (len #edk_raw_content == #element_length) *
            (len #rest == #total_length - #element_length)

        ensures:
            default_allocator(#alloc) *
            (#definition == `Complete`) *
            valid_aws_byte_cursor_ptr(#cur, #total_length - #element_length, #buffer p+ #element_length, #rest) *
            valid_aws_cryptosdk_edk_ptr(#edk, #alloc, #edk_content) *
            ARRAY(#buffer, char, #element_length, #edk_raw_content) *
            any_aws_last_error() *
            (ret == int(0));

            default_allocator(#alloc) *
            (#definition == `Incomplete`) *
            valid_aws_byte_cursor_ptr(#cur, #left_length, #new_buffer, #restAfterConsuming) *
            (0 <=# #consumed_length) *
            (#consumed_length == #total_length - #left_length) *
            (#new_buffer == #buffer p+ #consumed_length) *
            empty_aws_cryptosdk_edk_ptr(#edk) *
            optBytes(#buffer, #consumed_length, #consumed_data) *
            (#content == #consumed_data @ #restAfterConsuming) *
            aws_last_error_is_SHORT_BUF() *
            (ret == int(-1))
}
*/
int parse_edk(struct aws_allocator *allocator, struct aws_cryptosdk_edk *edk,
              struct aws_byte_cursor *cur) {

    uint16_t field_len;

    memset(edk, 0, sizeof(*edk));

    GILLIAN("apply valid_aws_byte_cursor_ptr_facts(#cur, #total_length, #buffer, #content)");
    GILLIAN("unfold Element(#definition, #content, 0, 3, #edk_content, #element_length)");
    GILLIAN(
        "if (#definition = `Complete`) { \
            unfold CElement(#content, 0, 3, #edk_content, #element_length) \
            [[bind #pid: #field, #rf1: #restFields]] \
        } else { \
            unfold IElement(#content, 0, 3, #edk_content, #element_length) \
            [[bind #l1: #fLength, #rf1: #restFList, #rel1: #restELength]] \
        }");

    if (!aws_byte_cursor_read_be16(cur, &field_len))
        goto SHORT_BUF;
    if (aws_byte_buf_init(&edk->provider_id, allocator, field_len))
        goto MEM_ERR;
    if (!aws_byte_cursor_read_and_fill_buffer(cur, &edk->provider_id))
        goto SHORT_BUF;

    GILLIAN(
        "if (#definition = `Complete`) { \
            unfold CElement(#content, 2 + len #pid, 2, #rf1, #element_length - (2 + len #pid)) \
            [[bind #pinfo: #field, #rf2: #restFields]] \
        } else { \
            unfold IElement(#content, #l1, 2, #rf1, #rel1) \
            [[bind #l2: #fLength, #rf2: #restFList, #rel2: #restELength]] \
        }");

    if (!aws_byte_cursor_read_be16(cur, &field_len))
        goto SHORT_BUF;
    if (aws_byte_buf_init(&edk->provider_info, allocator, field_len))
        goto MEM_ERR;
    if (!aws_byte_cursor_read_and_fill_buffer(cur, &edk->provider_info))
        goto SHORT_BUF;

    GILLIAN(
        "if (#definition = `Complete`) { \
            unfold CElement(#content, 2 + len #pid + 2 + len #pinfo, 1, #rf2, #element_length - (2 + len #pid + 2 + len #pinfo)) [[bind #ctxt: #field]] \
        } else { \
            unfold IElement(#content, #l1 + #l2, 1, #rf2, #rel2) \
        }");
    GILLIAN("unfold_all CElement");
    GILLIAN("unfold_all IElement");

    if (!aws_byte_cursor_read_be16(cur, &field_len))
        goto SHORT_BUF;
    if (aws_byte_buf_init(&edk->ciphertext, allocator, field_len))
        goto MEM_ERR;
    if (!aws_byte_cursor_read_and_fill_buffer(cur, &edk->ciphertext))
        goto SHORT_BUF;

    GILLIAN("unfold_all valid_aws_byte_buf_ptr");
    GILLIAN(
        "if (#definition = `Incomplete`) { \
            unfold_all valid_aws_byte_cursor_ptr \
        }");
    GILLIAN(
        "fold valid_aws_cryptosdk_edk_ptr(#edk, #alloc, #edk_content)");

    return AWS_OP_SUCCESS;

SHORT_BUF:
    GILLIAN("unfold_all valid_aws_byte_buf_ptr");
    aws_cryptosdk_edk_clean_up(edk);
    return aws_raise_error(AWS_ERROR_SHORT_BUFFER);

MEM_ERR:
    // We do not model OOM error, this path must not be reached
    GILLIAN("assert False");
    aws_cryptosdk_edk_clean_up(edk);
    // The _init function should have already raised an AWS_ERROR_OOM
    return AWS_OP_ERR;
}

//
// MAIN FUNCTION: aws_cryptosdk_hdr_parse(hdr, pcursor)
// deserialises a header at pcursor, storing the result in hdr
//
/*@
    spec aws_cryptosdk_hdr_parse(hdr, pcursor) {
        requires:
            (hdr == #hdr) * (pcursor == #pcursor) *
            valid_aws_byte_cursor_ptr(#pcursor, #length, #buffer, #data) *
            Header(#definition, #data, #part_one, #version, #type, #suiteId, #messageId,
                   #ECLength, #part_two, #ECKs, #part_three, #EDKs, #contentType, #headerIvLength,
                   #frameLength, #headerLength, #headerIv, #headerAuthTag, #edkDef, #errorMessage) *
            (#length == len #data) *
            (#length <# 2147483647) *
            any_valid_aws_cryptosdk_hdr(#hdr, #alloc) *
            default_allocator(#alloc) *
            any_aws_last_error()

        ensures:
            (#definition == `Complete`) *
            ARRAY(#buffer, char, #length, #data) *
            valid_aws_byte_cursor_ptr(#pcursor, 0, #end_buffer, []) *
            header_struct(#hdr, #alloc, #suiteId, #messageId, #ECKs, #EDKs, #frameLength, #headerLength, #headerIv, #headerAuthTag) *
            default_allocator(#alloc) *
            any_aws_last_error() *
            (ret == int(0));

            (#definition == `Incomplete`) *
            valid_aws_byte_cursor_ptr(#pcursor, #length, #buffer, #data) *
            empty_aws_cryptosdk_hdr(#hdr, #alloc) *
            default_allocator(#alloc) *
            aws_last_error_is_SHORT_BUF() *
            (ret == int(-1));

            (#definition == `Broken`) *
            valid_aws_byte_cursor_ptr(#pcursor, #length, #buffer, #data) *
            empty_aws_cryptosdk_hdr(#hdr, #alloc) *
            default_allocator(#alloc) *
            aws_last_error_is_BAD_CIPHERTEXT() *
            (ret == int(-1))
  }
*/
int aws_cryptosdk_hdr_parse(struct aws_cryptosdk_hdr *hdr,
                            struct aws_byte_cursor *pcursor) {

    GILLIAN(
        "unfold Header(#definition, #data, #part_one, #version, #type, \
            #suiteId, #messageId, #ECLength, #part_two, #ECKs, \
            #part_three, #EDKs, #contentType, #headerIvLength, \
            #frameLength, #headerLength, #headerIv, #headerAuthTag, #edkDef, #errorMessage) \
        [[ bind #ECDef: #ECDef ]]");

    GILLIAN(
        "if (#definition = `Complete`) { \
            unfold CHeader(#data, #part_one, #version, #type, \
                #suiteId, #messageId, #ECLength, #part_two, #ECKs, \
                #part_three, #EDKs, #contentType, #headerIvLength, \
                #frameLength, #headerLength, #headerIv, #headerAuthTag, #edkDef) \
            [[bind #tagLength: #tagLength, #edks: #edks, #EDKsLength: #EDKsLength]] \
        } else { \
            if (#definition = `Incomplete`) { \
                unfold IHeader(#data, #part_one, #version, #type, \
                    #suiteId, #messageId, #ECLength, #part_two, #ECKs, \
                    #part_three, #EDKs, #contentType, #headerIvLength, \
                    #frameLength, #headerLength, #headerIv, #headerAuthTag, #edkDef) \
                [[bind #tagLength: #tagLength, #edks: #edks, #EDKsLength: #EDKsLength]] \
            } else { \
                unfold BHeader(#data, #part_one, #version, #type, \
                    #suiteId, #messageId, #ECLength, #part_two, #ECKs, \
                    #part_three, #EDKs, #contentType, #headerIvLength, \
                    #frameLength, #headerLength, #headerIv, #headerAuthTag, #ECDef, #edkDef, #errorMessage) \
                [[bind #tagLength: #tagLength, #edks: #edks, #EDKsLength: #EDKsLength, #BEC: #EC]] \
            } \
        }");

    struct aws_byte_cursor cur = *pcursor;

    aws_cryptosdk_hdr_clear(hdr);
    GILLIAN("unfold_all empty_aws_cryptosdk_hdr");
    GILLIAN("unfold_all valid_edk_array_list");

    uint8_t bytefield;

    if (!aws_byte_cursor_read_u8(&cur, &bytefield)) {
        goto SHORT_BUF;
    }

    if (aws_cryptosdk_unlikely(bytefield != AWS_CRYPTOSDK_HEADER_VERSION_1_0))
        goto PARSE_ERR;

    if (!aws_byte_cursor_read_u8(&cur, &bytefield)) {
        goto SHORT_BUF;
    }

    if (aws_cryptosdk_unlikely(bytefield !=
                               AWS_CRYPTOSDK_HEADER_TYPE_CUSTOMER_AED))
        goto PARSE_ERR;

    uint16_t alg_id;
    if (!aws_byte_cursor_read_be16(&cur, &alg_id)) {
        goto SHORT_BUF;
    }

    if (aws_cryptosdk_unlikely(!aws_cryptosdk_algorithm_is_known(alg_id))) {
        goto PARSE_ERR;
    }

    hdr->alg_id = alg_id;

    if (!aws_byte_cursor_read(&cur, hdr->message_id, MESSAGE_ID_LEN)) {
        goto SHORT_BUF;
    }

    uint16_t aad_len;
    if (!aws_byte_cursor_read_be16(&cur, &aad_len)) {
        goto SHORT_BUF;
    }

    GILLIAN("unfold_all Elements");
    if (aad_len) {
        GILLIAN("if (#definition = `Broken`) { if (#ECDef = `Broken`) { unfold BRawEncryptionContext(#errorMessage, #BEC, #ECKs) } }");
        struct aws_byte_cursor aad = aws_byte_cursor_advance(&cur, aad_len);
        if (!aad.ptr)
            goto SHORT_BUF;
        // Note that, even if this fails with SHORT_BUF, we report a parse
        // error, since we know we have enough data (according to the aad
        // length field).
        if (aws_cryptosdk_enc_ctx_deserialize(hdr->alloc, &hdr->enc_ctx, &aad))
            goto PARSE_ERR;
        if (aad.len) {
            // trailing garbage after the aad block
            goto PARSE_ERR;
        }
    };
    GILLIAN("if (#ECLength = 0) { unfold CRawEncryptionContext([], #ECKs) }");
    GILLIAN("unfold_all valid_hash_table_ptr");
    GILLIAN("unfold_all RawEncryptedDataKeys");
    GILLIAN(
        "if (#edkDef = `Complete`) { \
            unfold CRawEncryptedDataKeys(#part_three, 0, #EDKs, #EDKsLength) \
        } else { \
            if (#edkDef = `Incomplete`) { \
                unfold IRawEncryptedDataKeys(#part_three, 0) \
            } \
        }");
    uint16_t edk_count;
    if (!aws_byte_cursor_read_be16(&cur, &edk_count))
        goto SHORT_BUF;
    if (!edk_count)
        goto PARSE_ERR;

    // C#m trickery for local addressable variables
    GILLIAN(
        "assert [[bind #edk_count, #edkcptr]] \
            (edk_count == [ #l, #n ]) * (#edkcptr == ptr(#l, 0)) * ARRAY(#edkcptr, int16, 1, [#edk_count])");
    GILLIAN(
        "assert [[bind #cur]] \
            (cur == [#l, 16]) * (#cur == ptr(#l, 0))");

    GILLIAN(
        "assert [[bind #edktemp]] \
            (edk == [#l, 96]) * (#edktemp == ptr(#l, 0))");

    GILLIAN("apply ElementsShift(#part_three, 2, #edk_count, 3, 2)");
    GILLIAN("unfold_all Elements");
    GILLIAN(
        "if (#edkDef = `Complete`) { \
            assert [[bind #edk_al, #edkLenb0, #edkLenb1, #justEDKs, #EDKsAndRest, #restOfHeader, #totalEDKLength, #atEDKs, #bptr, #eList]] \
                (#edk_al == #hdr p+ 104) * \
                (#edks == ([ #edkLenb0, #edkLenb1 ] @ #justEDKs)) * \
                (#eList == #EDKs) * \
                (#EDKsAndRest == lsub(#part_three, 2, (len #part_three) - 2)) * \
                (#EDKsAndRest == #justEDKs @ #restOfHeader) * \
                CElements(#EDKsAndRest, 0, #edk_count, 3, #eList, #totalEDKLength) * \
                valid_aws_byte_cursor_ptr(#cur, #atEDKs, #bptr, #EDKsAndRest) \
        } else { \
            if (#edkDef = `Incomplete`) { \
                assert [[bind #edk_al, #edkLenb0, #edkLenb1, #justEDKs, #EDKsAndRest, #restOfHeader, #totalEDKLength, #atEDKs, #bptr, #eList]] \
                    (#edk_al == #hdr p+ 104) * \
                    (#edks == ([ #edkLenb0, #edkLenb1 ] @ #justEDKs)) * \
                    (#EDKsAndRest == lsub(#part_three, 2, (len #part_three) - 2)) * \
                    (#EDKsAndRest == #justEDKs @ #restOfHeader) * \
                    IElements(#EDKsAndRest, 0, #edk_count, 3, #eList, #totalEDKLength) * \
                    valid_aws_byte_cursor_ptr(#cur, #atEDKs, #bptr, #EDKsAndRest) \
            } else { \
                assert False \
            } \
        }");

    GILLIAN("assert [[bind #all, #alo]] #alloc == ptr(#all, #alo)");

    GILLIAN(
        "invariant: \
            [[bind i, #i, #cbptr, #readLength, #readEDKs, #leftEDKs, #restEDKsAndRest, \
                   #acc, #rest_count, #restEDKs, #restEDKLength, #restLength, #trash]] \
            any_aws_last_error() * \
            (i == int(#i)) * default_allocator(#alloc) * \
            (#hdr -> ptr(#all, #alo)) * (#alloc == ptr(#all, #alo)) * \
            optBytes(#bptr, #readLength, #readEDKs) * \
            (#readLength == len #readEDKs) * \
            (len #data == 22 + #ECLength + 2 + #readLength + #restLength) * \
            valid_aws_byte_cursor_ptr(#cur, #restLength, #cbptr, #restEDKsAndRest) * \
            valid_edk_array_list_ptr(#edk_al, #alloc, #acc) * \
            (len #acc == #i) * (#eList == #acc @ #restEDKs) * \
            (#i <=# #edk_count) * \
            Elements(#edkDef, #restEDKsAndRest, 0, #rest_count, 3, #restEDKs, #restEDKLength) * \
            (#restLength == len #restEDKsAndRest) * \
            (#rest_count == (#edk_count - #i)) * \
            (#restLength == (#atEDKs - #readLength)) * \
            (#restEDKLength == (#totalEDKLength - #readLength)) * \
            (#justEDKs == (#readEDKs @ #leftEDKs)) * \
            (#restEDKsAndRest == (#leftEDKs @ #restOfHeader)) * \
            (#EDKsAndRest == (#readEDKs @ #restEDKsAndRest)) * \
            (#cbptr == (#bptr p+ #readLength)) * \
            ARRAY(#edkcptr, int16, 1, [#edk_count]) * \
            ARRAY(#edktemp, long, 12, #trash)");

    for (uint16_t i = 0; i < edk_count; ++i) {
        struct aws_cryptosdk_edk edk;

        GILLIAN("unfold_all i__ptr");
        GILLIAN("unfold_all i__ptr_add");
        GILLIAN("unfold Elements(#edkDef, #restEDKsAndRest, 0, #rest_count, 3, #restEDKs, #restEDKLength)");
        GILLIAN(
            "if (#edkDef = `Complete`) { \
                unfold CElements(#restEDKsAndRest, 0, #rest_count, 3, #restEDKs, #restEDKLength) \
                [[bind #e: #element, #el: #eLength ]]; \
                apply CElementsShift(#restEDKsAndRest, #el, #rest_count - 1, 3, #el) \
            } else { \
                if (#edkDef = `Incomplete`) { \
                    unfold IElements(#restEDKsAndRest, 0, #rest_count, 3, #restEDKs, #restEDKLength) \
                    [[bind #e: #fList, #el: #eLength]]; \
                    if (! (#restEDKs = [])) { \
                        apply IElementsShift(#restEDKsAndRest, #el, #rest_count - 1, 3, #el) \
                    } \
                } else { \
                    assert False \
                } \
            }");

        if (parse_edk(hdr->alloc, &edk, &cur)) {

            GILLIAN("unfold valid_edk_array_list_ptr(#edk_al, #alloc, #EDKs)");
            GILLIAN("unfold valid_aws_byte_cursor_ptr(#cur, #ra, #rb, #rc)");
            GILLIAN("apply optBytesConcat(#bptr, #readLength, #cbptr, len #leftEDKs)");
            GILLIAN("unfold optBytes(#bptr, #readLength + (len #leftEDKs), #restOrSomething)");
            goto RETHROW;
        }

        aws_array_list_push_back(&hdr->edk_list, &edk);

        GILLIAN("apply optBytesConcat(#bptr, #readLength, #bptr p+ (len #readEDKs), #el)");
    }

    GILLIAN("assert (len #acc == #edk_count)");
    GILLIAN(
        "if (#edkDef = `Incomplete`) { \
            unfold Elements(#edkDef, #restEDKsAndRest, 0, #rest_count, 3, #restEDKs, #restEDKLength); \
            unfold IElements(#restEDKsAndRest, 0, #rest_count, 3, #restEDKs, #restEDKLength) \
        }");
    GILLIAN("assert (len #restEDKs == 0) ");
    GILLIAN("assert (#rest_count == 0) * (#EDKs == #acc) ");
    GILLIAN("unfold Elements(#edkDef, #restEDKsAndRest, 0, #rest_count, 3, #restEDKs, #restEDKLength)");
    GILLIAN("unfold CElements(#restEDKsAndRest, 0, #rest_count, 3, #restEDKs, #restEDKLength)");
    GILLIAN("assert (len #leftEDKs == 0)");

    GILLIAN("unfold optBytes(#bptr, #readLength, #readEDKs)");

    uint8_t content_type;
    if (!aws_byte_cursor_read_u8(&cur, &content_type))
        goto SHORT_BUF;

    if (aws_cryptosdk_unlikely(!is_known_type(content_type)))
        goto PARSE_ERR;

    uint32_t reserved; // must be zero
    if (!aws_byte_cursor_read_be32(&cur, &reserved))
        goto SHORT_BUF;
    if (reserved)
        goto PARSE_ERR;

    uint8_t iv_len;
    if (!aws_byte_cursor_read_u8(&cur, &iv_len))
        goto SHORT_BUF;

    if (iv_len != aws_cryptosdk_algorithm_ivlen(alg_id))
        goto PARSE_ERR;

    uint32_t frame_len;
    if (!aws_byte_cursor_read_be32(&cur, &frame_len))
        goto SHORT_BUF;

    if ((content_type == AWS_CRYPTOSDK_HEADER_CTYPE_NONFRAMED &&
         frame_len != 0) ||
        (content_type == AWS_CRYPTOSDK_HEADER_CTYPE_FRAMED && frame_len == 0))
        goto PARSE_ERR;

    hdr->frame_len = frame_len;

    // cur.ptr now points to end of portion of header that is authenticated
    hdr->auth_len = cur.ptr - pcursor->ptr;

    if (aws_byte_buf_init(&hdr->iv, hdr->alloc, iv_len))
        goto MEM_ERR;
    if (!aws_byte_cursor_read_and_fill_buffer(&cur, &hdr->iv))
        goto SHORT_BUF;

    size_t tag_len = aws_cryptosdk_algorithm_taglen(alg_id);
    if (aws_byte_buf_init(&hdr->auth_tag, hdr->alloc, tag_len))
        goto MEM_ERR;
    if (!aws_byte_cursor_read_and_fill_buffer(&cur, &hdr->auth_tag))
        goto SHORT_BUF;

    GILLIAN("assert [[bind #iv_ptr, #auth_tag_ptr]] (#iv_ptr == (#hdr p+ 16)) * (#auth_tag_ptr == (#hdr p+ 48))");
    GILLIAN("unfold valid_aws_byte_buf_ptr(#iv_ptr, #headerIvLength, #headerIvLength, #iv_buf, #alloc, #headerIv)");
    GILLIAN("unfold valid_aws_byte_buf_ptr(#auth_tag_ptr, 16, 16, #auth_tag_buf, #alloc, #headerAuthTag)");

    *pcursor = cur;

    GILLIAN("unfold valid_edk_array_list_ptr(#edk_al, #alloc, #EDKs)");

    return AWS_OP_SUCCESS;

SHORT_BUF:
    GILLIAN("unfold_all BRawEncryptionContext");
    GILLIAN("unfold_all valid_aws_byte_buf_ptr");
    GILLIAN("unfold_all valid_edk_array_list_ptr");
    GILLIAN("unfold_all optBytes");
    GILLIAN("unfold_all valid_aws_byte_cursor_ptr");

    aws_cryptosdk_hdr_clear(hdr);
    return aws_raise_error(AWS_ERROR_SHORT_BUFFER);

PARSE_ERR:
    GILLIAN("unfold_all ECErrorCodeOfJSErrorMessage");
    GILLIAN("unfold_all valid_aws_byte_buf_ptr");
    GILLIAN("unfold_all valid_edk_array_list_ptr");
    GILLIAN("unfold_all optBytes");
    GILLIAN("unfold_all valid_aws_byte_cursor_ptr");

    aws_cryptosdk_hdr_clear(hdr);
    return aws_raise_error(AWS_CRYPTOSDK_ERR_BAD_CIPHERTEXT);

MEM_ERR:
RETHROW:
    GILLIAN("unfold_all valid_aws_byte_buf_ptr");
    GILLIAN("unfold_all valid_edk_array_list_ptr");
    GILLIAN("unfold_all optBytes");
    GILLIAN("unfold_all valid_aws_byte_cursor_ptr");

    aws_cryptosdk_hdr_clear(hdr);
    return AWS_OP_ERR;
}