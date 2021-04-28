#include "ec.h"
#include "error.h"
#include "string.h"

void aws_cryptosdk_enc_ctx_clear(struct aws_hash_table *enc_ctx) {
    aws_hash_table_clear(enc_ctx);
}

/*@ pred nounfold ECErrorCodeOfJSErrorMessage(errorMessage) {
    (errorMessage == `decodeEncryptionContext: Underflow, not enough data.`) *
        aws_last_error_is_SHORT_BUF();
    (errorMessage == `decodeEncryptionContext: Not enough data to read key count.`) *
        aws_last_error_is_SHORT_BUF();
    (errorMessage == `decodeEncryptionContext: Key Count is 0.`) *
        aws_last_error_is_BAD_CIPHERTEXT();
    (errorMessage == `decodeEncryptionContext: Duplicate encryption context key value.`) *
        aws_last_error_is_BAD_CIPHERTEXT()
}
*/

/*@
pred enc_ctx_complete_case_equality(+ECKs, +elements, +restElements, +consumedElements, +keys, +consumedKeys, +restKeys) {
    (ECKS == elements) * (keys == consumedKeys @ restKeys)
}

pred nounfold enc_ctx_deser_invariant_cases(+definition, +elementsDef, +aad_len, +esLength, +restEsLength, +restLength, +ECKs,
                                            +elements, +consumedElements, +restElements, +keys, +consumedKeys, +restKeys, errorMessage) {
    (definition == `Complete`) * (elementsDef == `Complete`) *
    (aad_len == 2 + esLength) *
    enc_ctx_complete_case_equality(ECKs, elements, restElements, consumedElements, keys, consumedKeys, restKeys) *
    (restEsLength == restLength) *
    Unique(keys) * (errorMessage == ``);

    (definition == `Broken`) * (elementsDef == `Complete`) *
    enc_ctx_complete_case_equality(ECKs, elements, restElements, consumedElements, keys, consumedKeys, restKeys) *
    Duplicated(consumedKeys, restKeys) *
    (errorMessage == `decodeEncryptionContext: Duplicate encryption context key value.`);

    (definition == `Broken`) * (elementsDef == `Complete`) *
    (not (aad_len == 2 + esLength)) *
    enc_ctx_complete_case_equality(ECKs, elements, restElements, consumedElements, keys, consumedKeys, restKeys) *
    Unique(keys) * (errorMessage == `decodeEncryptionContext: Overflow, too much data.`);

    (definition == `Broken`) * (elementsDef == `Incomplete`) *
    Unique(keys) * (errorMessage == `decodeEncryptionContext: Underflow, not enough data.`)

}
*/

/*@
spec aws_cryptosdk_enc_ctx_deserialize(alloc, enc_ctx, cursor) {

  requires: (alloc == #alloc) * (enc_ctx == #enc_ctx) * (cursor == #cursor) *
            default_allocator(#alloc) *
            empty_hash_table_ptr(#enc_ctx, #alloc) *
            valid_aws_byte_cursor_ptr(#cursor, #aad_len, #buffer, #buffer_content) *
            (0 <# #aad_len) *
            (not (#buffer == NULL)) *
            (#definition == `Complete`) * (#errorMessage == ``) *
            CRawEncryptionContext(#buffer_content, #ECKs) *
            any_aws_last_error()

  ensures:  default_allocator(#alloc) *
            valid_hash_table_ptr(#enc_ctx, #alloc, #ECKs, #utf8ECKs) *
            valid_aws_byte_cursor_ptr(#cursor, 0, #buffer p+ #aad_len, []) *
            ARRAY(#buffer, char, #aad_len, #buffer_content) *
            any_aws_last_error() *
            (ret == int(0))


  OR


  requires: (alloc == #alloc) * (enc_ctx == #enc_ctx) * (cursor == #cursor) *
            default_allocator(#alloc) *
            empty_hash_table_ptr(#enc_ctx, #alloc) *
            valid_aws_byte_cursor_ptr(#cursor, #aad_len, #buffer, #buffer_content) *
            (0 <# #aad_len) *
            (not (#buffer == NULL)) *
            (#definition == `Broken`) * (#errorMessage == `decodeEncryptionContext: Overflow, too much data.`) *
            BRawEncryptionContext(#errorMessage, #buffer_content, #ECKs) *
            any_aws_last_error()

  ensures:  default_allocator(#alloc) *
            valid_hash_table_ptr(#enc_ctx, #alloc, #ECKs, #utf8ECKs) *
            valid_aws_byte_cursor_ptr(#cursor, #rest_len, #new_buffer, #rest_buffer) *
            (#new_buffer == #buffer p+ #consumed_len) *
            (#buffer_content ==  #consumed_content @ #rest_buffer) *
            (0 <# #consumed_len) *
            (0 <# #rest_len) *
            ARRAY(#buffer, char, #consumed_len, #consumed_content) *
            any_aws_last_error() *
            (ret == int(0))


  OR


  requires: (alloc == #alloc) * (enc_ctx == #enc_ctx) * (cursor == #cursor) *
            default_allocator(#alloc) *
            empty_hash_table_ptr(#enc_ctx, #alloc) *
            valid_aws_byte_cursor_ptr(#cursor, #aad_len, #buffer, #buffer_content) *
            (0 <# #aad_len) *
            (not (#buffer == NULL)) *
            (#definition == `Broken`) *
            (not (#errorMessage == `decodeEncryptionContext: Overflow, too much data.`)) *
            BRawEncryptionContext(#errorMessage, #buffer_content, #ECKs) *
            any_aws_last_error()

  ensures:  default_allocator(#alloc) *
            empty_hash_table_ptr(#enc_ctx, #alloc) *
            valid_aws_byte_cursor_ptr(#cursor, #rest_len, #new_buffer, #rest_buffer) *
            (#new_buffer == #buffer p+ #consumed_len) *
            (#buffer_content ==  #consumed_content @ #rest_buffer) *
            (#consumed_len == (#aad_len - #rest_len)) *
            optBytes(#buffer, #consumed_len, #consumed_content) *
            ECErrorCodeOfJSErrorMessage(#errorMessage) *
            (ret == int(-1))
}
*/
int aws_cryptosdk_enc_ctx_deserialize(struct aws_allocator *alloc,
                                      struct aws_hash_table *enc_ctx,
                                      struct aws_byte_cursor *cursor) {
    aws_cryptosdk_enc_ctx_clear(enc_ctx);

    if (cursor->len == 0) {
        return AWS_OP_SUCCESS;
    }
    GILLIAN("unfold_all CRawEncryptionContext");
    GILLIAN("unfold_all BRawEncryptionContext");

    uint16_t elem_count;
    if (!aws_byte_cursor_read_be16(cursor, &elem_count))
        goto SHORT_BUF;
    if (!elem_count)
        return aws_raise_error(AWS_CRYPTOSDK_ERR_BAD_CIPHERTEXT);

    GILLIAN(
        "assert [[bind #l_ec, #elem_count]] "
        "(elem_count == [#l_ec, 2]) * ARRAY(ptr(#l_ec, 0), int16, 1, [ #elem_count ]) ");

    GILLIAN(
        "assert [[bind #elementsDef, #elements, #esLength]] Elements(#elementsDef, #buffer_content, 2, #elem_count, 2, #elements, #esLength)");

    GILLIAN(
        "unfold Elements(#elementsDef, #buffer_content, 2, #elem_count, 2, #ECKS, #esLength)");
    GILLIAN(
        "assert [[bind #lengthPtr]] (length == [#l, 2]) * (#lengthPtr == ptr(#l, 0))");
    GILLIAN(
        "assert [[bind #l_res_1, #l_res, #l_k_cursor, #l_v_cursor]] "
        "(_res__1 == [#l_res_1, 16]) * (_res == [#l_res, 16]) * "
        "(k_cursor == [#l_k_cursor, 16]) * (v_cursor == [#l_v_cursor, 16]) * "
        "(was_created == [#l_wc, 4])");
    GILLIAN("assert [[bind #keys]] FirstProj(#elements, #keys)");
    GILLIAN(
        "invariant: [[bind i, #i, #togo, #trash, #restBuffer, #restLength,"
        "                  #cbptr, #consumedLength, #consumedBuffer, #restEsLength,"
        "                  #consumedKeys, #consumedUtf8Keys, #restKeys, #consumedUtf8Elements,  "
        "                  #consumedElements, #restElements,"
        "                  #res_1_trash, #res_trash, #k_trash, #v_trash, #wc_trash, #b1, #b2]]"
        // The allocator and error global variable
        "default_allocator(#alloc) * "
        "any_aws_last_error() * "
        // i is the number of ECKs parsed
        "(i == int(#i)) * "
        "(#i <=# #elem_count) * "
        "(#togo == (#elem_count - #i)) * "
        "(#consumedElements == lsub(#elements, 0, #i)) * "
        "(#elements == #consumedElements @ #restElements) * "
        // Some spacial information about the byte_cursor
        "(#cbptr == (#buffer p+ (#consumedLength + 2))) * "
        "(#aad_len == (2 + #consumedLength + #restLength)) * "
        "(#consumedBuffer == lsub(#buffer_content, 2, #consumedLength)) * "
        "(#buffer_content == [ #b1, #b2 ] @ #consumedBuffer @ #restBuffer) * "
        "(#esLength == #consumedLength + #restEsLength) *"
        "optBytes(#buffer p+ 2, #consumedLength, #consumedBuffer) * "
        "valid_aws_byte_cursor_ptr(#cursor, #restLength, #cbptr, #restBuffer) * "
        "((len #buffer_content) == #aad_len) * (len #restBuffer == #restLength) * "
        "(0 <# #elem_count) * "
        // Spacial information about the table
        // The buffer contains the rest of the ECKS
        // They are unique, so they can be put safely in the hashtable
        // The UTF8 condition is necessary for the hashtable strings to work
        // That information is required for the hash_table_put to work
        "Elements(#elementsDef, #buffer_content, 2 + #consumedLength, #togo, 2, #restElements, #restEsLength) * "
        "FirstProj(#elements, #keys) * "
        "FirstProj(#consumedElements, #consumedKeys) * "
        "FirstProj(#restElements, #restKeys) * "
        "toUtf8PairMap(#consumedElements, #consumedUtf8Elements) * "
        "FirstProj(#consumedUtf8Elements, #consumedUtf8Keys) * "
        "toUtf8PairMap(#restElements, #restUtf8Elements) * "
        "valid_hash_table_ptr(#enc_ctx, #alloc, #consumedElements, #consumedUtf8Elements) * "
        // The different cases represented in the invariant
        "enc_ctx_deser_invariant_cases("
        "        #definition, #elementsDef, #aad_len, #esLength, #restEsLength, #restLength, #ECKs,"
        "        #elements, #consumedElements, #restElements, #keys, #consumedKeys, #restKeys, #errorMessage"
        ") * "
        // Csharpminor trickery, variable that are defined inside the loop
        // still need to be in the invariant because they are hoisted and treated as
        // resource
        "(elem_count == [#l_ec, 2]) * ARRAY(ptr(#l_ec, 0), int16, 1, [ #elem_count ]) * "
        "ARRAY(#lengthPtr, int16, 1, [ #trash ]) * "
        "(_res == [#l_res, 16]) * ARRAY(ptr(#l_res, 0), long, 2, #res_trash) * "
        "(_res__1 == [#l_res_1, 16]) * ARRAY(ptr(#l_res_1, 0), long, 2, #res_1_trash) * "
        "(k_cursor == [#l_k_cursor, 16]) * ARRAY(ptr(#l_k_cursor, 0), long, 2, #k_trash) * "
        "(v_cursor == [#l_v_cursor, 16]) * ARRAY(ptr(#l_v_cursor, 0), long, 2, #v_trash) * "
        "(was_created == [#l_wc, 4]) * ARRAY(ptr(#l_wc, 0), int, 1, [#wc_trash])");
    for (uint16_t i = 0; i < elem_count; i++) {
        uint16_t length;
        GILLIAN(
            "unfold Elements(#elementsDef, #buffer_content, 2 + #consumedLength, #togo, 2, #restElements, #restEsLength)");
        GILLIAN(
            "if (#elementsDef = `Complete`) {"
            "  unfold CElements(#buffer_content, 2 + #consumedLength, #togo, 2, #restElements, #restLength)"
            "    [[bind #element: #kvPair, #eLength: #this_length, #restElements: #newRestElements, #restLength: #newRestEsLength]];"
            "  assert [[bind #this_key, #this_value]] (#kvPair == [#this_key, #this_value]);"
            "  unfold CElement(#buffer_content, 2 + #consumedLength, 2, [#this_key, #this_value], #this_length)"
            "    [[bind #fLength: #key_length, #restLength: #value_length]]"
            "} else {"
            "  unfold IElements(#buffer_content, 2 + #consumedLength, #togo, 2, #restElements, #restLength) "
            "    [[bind #fList: #maybePartialKvPair]];"
            "  if (#restElements = []) {"
            "    unfold IElement(#buffer_content, 2 + #consumedLength, 2, #maybePartialKvPair, #this_length)"
            "      [[bind #fLength: #key_length]];"
            "    branch #consumedLength == 0"
            "  } else {"
            "    assert [[bind #this_key, #this_value]] (#maybePartialKvPair == [#this_key, #this_value]);"
            "    unfold CElement(#buffer_content, 2 + #consumedLength, 2, [#this_key, #this_value], #this_length) "
            "      [[bind #fLength: #key_length, #restLength: #value_length]]"
            "  }"
            "}");

        if (!aws_byte_cursor_read_be16(cursor, &length))
            goto SHORT_BUF;
        struct aws_byte_cursor k_cursor =
            aws_byte_cursor_advance(cursor, length);
        if (!k_cursor.ptr)
            goto SHORT_BUF;

        GILLIAN(
            "if (#elementsDef = `Complete`) {"
            "  unfold CElement(#buffer_content, 2 + #consumedLength + #key_length, 1, [ #this_value ], #value_length);"
            "  unfold_all CElement"
            "} else {"
            "  if (#restElements = []) {" // Incomplete Case
            "    unfold IElement(#buffer_content, 2 + #consumedLength + #key_length, 1, [], #some_length_trash)"
            "  } else {"
            "    unfold CElement(#buffer_content, 2 + #consumedLength + #key_length, 1, [ #this_value ], #value_length);"
            "    unfold_all CElement"
            "  }"
            "}");
        if (!aws_byte_cursor_read_be16(cursor, &length))
            goto SHORT_BUF;
        struct aws_byte_cursor v_cursor =
            aws_byte_cursor_advance(cursor, length);
        if (!v_cursor.ptr)
            goto SHORT_BUF;

        GILLIAN(
            "if (#restElements = []) { unfold_all valid_aws_byte_cursor_ptr }");
        GILLIAN(
            "unfold toUtf8PairMap(#restElements, #restUtf8Elements)");

        struct aws_string *k =
            aws_string_new_from_array(alloc, k_cursor.ptr, k_cursor.len);

        struct aws_string *v =
            aws_string_new_from_array(alloc, v_cursor.ptr, v_cursor.len);

        GILLIAN("apply ProduceListToSet(#consumedKeys)");
        GILLIAN(
            "assert [[bind #consumedKeySet]] ListToSet(#consumedKeys, #consumedKeySet)");
        GILLIAN(
            "apply FirstProjConcatSplit(#elements, #consumedElements, #restElements)");
        GILLIAN("unfold FirstProj(#restElements, #restKeys)");
        GILLIAN(
            "apply FirstProjToUtf8MapPairCompat(#consumedElements)");
        GILLIAN("apply ProduceListToSet(#consumedUtf8Keys)");
        GILLIAN(
            "if (#errorMessage = `decodeEncryptionContext: Duplicate encryption context key value.`) {"
            "  unfold Duplicated(#consumedKeys, #restKeys) [[bind #preSet: #consumedKeySetAlias]];"
            "  apply ListToSetFunction(#consumedKeys, #consumedKeySet, #consumedKeys, #consumedKeySetAlias)"
            "} else {"
            "  apply ProduceListToSet(#restKeys);"
            "  apply UniqueConcatSplitNotInSuffix(#keys, #consumedKeys, #restKeys, #this_key)"
            "}");
        GILLIAN("if (#this_key -e- #consumedKeySet) {"
                        "  apply InListToUtf8(#this_key, #consumedKeys)"
                        "} else {"
                        "  apply NotInListToUtf8(#this_key, #consumedKeys)"
                        "}");

        GILLIAN(
            "apply optBytesConcat(#buffer p+ 2, #consumedLength, #buffer p+ (2 + #consumedLength), 2)");
        GILLIAN(
            "apply optBytesConcat(#buffer p+ 2, #consumedLength + 2, #buffer p+ (4 + #consumedLength), #key_length - 2)");
        GILLIAN(
            "apply optBytesConcat(#buffer p+ 2, #consumedLength + #key_length, #buffer p+ (2 + #consumedLength + #key_length), 2)");
        GILLIAN(
            "apply optBytesConcat(#buffer p+ 2, #consumedLength + #key_length, #buffer p+ (2 + #consumedLength + #key_length), 2)");
        GILLIAN(
            "apply optBytesConcat(#buffer p+ 2, #consumedLength + #key_length + 2, #buffer p+ (4 + #consumedLength + #key_length), #value_length - 2)");
        int was_created;
        if (!k || !v ||
            aws_hash_table_put(enc_ctx, k, (void *)v, &was_created)) {
            // Errors here are only on memory allocation. aws-c-common will
            // raise the error code
            aws_string_destroy(k);
            aws_string_destroy(v);
            goto RETHROW;
        }
        if (!was_created) {
            // !was_created means there was a duplicate key in serialized
            // encryption context, so fail
            aws_raise_error(AWS_CRYPTOSDK_ERR_BAD_CIPHERTEXT);
            goto RETHROW;
        }
        GILLIAN(
            "apply FirstProjAppendPair(#consumedElements, #consumedKeys, #this_key, #this_value)");

        GILLIAN(
            "apply toUtf8PairMapAppendPair(#consumedElements, #consumedUtf8Elements, #this_key, #this_value)");
        GILLIAN("assert [[bind #utf8Key, #utf8Value]] "
                        "toUtf8(#this_key, #utf8Key) * "
                        "toUtf8(#this_value, #utf8Value)");
        GILLIAN(
            "apply FirstProjAppendPair(#consumedUtf8Elements, #consumedUtf8Keys, #utf8Key, #utf8Value)");
        GILLIAN(
            "apply FirstProjToUtf8MapPairCompat(#consumedElements @ [ [#this_key, #this_value] ])");
    }

    GILLIAN("assert #togo == 0");

    GILLIAN(
        "unfold Elements(#elementsDef, #buffer_content, (2. + #consumedLength), #togo, 2., #restElements, #restEsLength)");
    GILLIAN(
        "unfold CElements(#buffer_content, (2. + #consumedLength), #togo, 2., #restElements, #restEsLength)");

    GILLIAN(
        "if (#errorMessage = `decodeEncryptionContext: Duplicate encryption context key value.`) {"
        "  unfold FirstProj(#restElements, #restKeys);"
        "  assert #restKeys == [];"
        "  unfold Duplicated(#consumedKeys, #restKeys)"
        "}");

    GILLIAN("unfold_all optBytes");

    return AWS_OP_SUCCESS;

SHORT_BUF:
    GILLIAN(
        "if (#errorMessage = `decodeEncryptionContext: Underflow, not enough data.`) {"
        "  apply optBytesConcat(#buffer, 2, #buffer p+ 2, #consumedLength)"
        "}");
    aws_raise_error(AWS_ERROR_SHORT_BUFFER);
RETHROW:
    aws_cryptosdk_enc_ctx_clear(enc_ctx);
    return AWS_OP_ERR;
USELESS_DEATH:
    return 1000;
}