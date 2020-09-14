'use strict';

/*************************
 *************************
 *******           *******
 *******   needs   *******
 *******           *******
 *************************
 *************************/

/**
 * @id needs
 */
function needs (condition, errorMessage) {
    if (!condition) {
        throw new Error(errorMessage)
    }
}

/********************************
 ********************************
 *******                  *******
 *******   readElements   *******
 *******                  *******
 ********************************
 ********************************/

/*
    @id readElements

    @onlyspec readElements (elementCount, fieldsPerElement, buffer, readPos)
    [[
      (elementCount == #elementCount) * (fieldsPerElement == #fieldsPerElement) * (buffer == #buffer) * (readPos == #readPos) *
      Uint8Array (#buffer, #ab, #viewOffset, #viewSize) *
      ArrayBuffer(#ab, #data) *
      (#view == l-sub(#data, #viewOffset, #viewSize)) *
      Elements(#view, #readPos, #elementCount, #fieldsPerElement, #elementList, #elementsLength) *
      JSInternals ()
    ]]
    [[
      Uint8Array (#buffer, #ab, #viewOffset, #viewSize) *
      ArrayBuffer(#ab, #data) *
      Elements(#view, #readPos, #elementCount, #fieldsPerElement, #elementList, #elementsLength) *
      JSInternals () *

      JSObject(ret) *
        DataProp(ret, "elements", #elements) *
            ArrayOfArraysOfUInt8Arrays(#elements, #elementList, #elementCount) *
        DataProp(ret, "readPos", #ret_readPos) *
            (#ret_readPos == #readPos + #elementsLength)
    ]]
    normal;

    [[
        (elementCount == #elementCount) * (fieldsPerElement == #fieldsPerElement) * (buffer == #buffer) * (readPos == #readPos) *
        Uint8Array (#buffer, #ab, #viewOffset, #viewSize) *
        ArrayBuffer(#ab, #data) *
        (#view == l-sub(#data, #viewOffset, #viewSize)) *
        IncompleteElements(#viewSize, #view, #readPos, #elementCount, #fieldsPerElement) *
        JSInternals ()
    ]]
    [[
        Uint8Array (#buffer, #ab, #viewOffset, #viewSize) *
        ArrayBuffer(#ab, #data) *
        IncompleteElements(#viewSize, #view, #readPos, #elementCount, #fieldsPerElement) *
        JSInternals () *

        (ret == false)
    ]]
    normal
*/
function readElements(elementCount, fieldsPerElement, buffer, readPos) {

    var dataView = new DataView(
        buffer.buffer,
        buffer.byteOffset,
        buffer.byteLength
    );

    /* @tactic apply lemma ElementsPureFacts(#view, #readPos, #elementCount, #fieldsPerElement, #elementList, #elementsLength) */

    /* Well-formedness: readPos must be within the byte length of the buffer given. */
    needs(readPos >= 0 && dataView.byteLength >= readPos, 'readPos out of bounds.')

    /* Well-formedness: elementCount must not be negative. */
    needs(elementCount >= 0, 'elementCount must be positive.')

    var elements = [];

    /*
      @invariant
        scope(elements : #doneEls) * scope(readPos : #outerLoopReadPos) * scope(elementCount : #elementsLeft) *
        Elements(#view, #outerLoopReadPos, #elementsLeft, #fieldsPerElement, #remainingElsList, #remainingElsLength) *
        Elements(#view, #readPos, #elementCount - #elementsLeft, #fieldsPerElement, #doneElsList, #doneElsLength) *
        (#elementList == l+ (#doneElsList, #remainingElsList)) *
        (#elementsLength == #doneElsLength + #remainingElsLength) *
        (#readPos + #doneElsLength == #outerLoopReadPos) *
        ArrayOfArraysOfUInt8Arrays(#doneEls, #doneElsList, #elementCount - #elementsLeft)
        [bind : #doneEls, #outerLoopReadPos, #elementsLeft, #remainingElsList, #remainingElsLength, #doneElsList, #doneElsLength]
    */
    while (elementCount--) {
      /* @tactic assert Element(#view, #outerLoopReadPos, #fieldsPerElement, #fieldsList, #elementLength) [bind: #fieldsList, #elementLength] */

      var element = []
      var fieldCount = fieldsPerElement

      /* @invariant
          scope(element : #doneEl) * scope(readPos : #innerLoopReadPos) * scope(fieldCount : #fieldsLeft) *
          Element(#view, #innerLoopReadPos, #fieldsLeft, #remainingElList, #remainingElLength) *
          Element(#view, #outerLoopReadPos, #fieldsPerElement - #fieldsLeft, #doneElList, #doneElLength) *
          (#fieldsList == l+ (#doneElList, #remainingElList)) *
          (#elementLength == #doneElLength + #remainingElLength) *
          (#outerLoopReadPos + #doneElLength == #innerLoopReadPos) *
          ArrayOfUInt8Arrays(#doneEl, #doneElList, #fieldsPerElement - #fieldsLeft)
          [bind: #doneEl, #innerLoopReadPos, #fieldsLeft, #remainingElList, #remainingElLength, #doneElList, #doneElLength] */
      while (fieldCount--) {
        /* Check for early return (Postcondition): Enough data must exist to read the Uint16 length value. */
        if (readPos + 2 > dataView.byteLength) return false
        var length = dataView.getUint16(readPos, false) // big endian
        readPos += 2
        /* Check for early return (Postcondition): Enough data must exist length of the value. */
        if (readPos + length > dataView.byteLength) return false
        var fieldBinary = buffer.slice(readPos, readPos + length)
        readPos += length
        /* @tactic assert #remainingElList == l+ ({{ #fld }}, #rfld) [bind: #fld, #rfld];
           apply lemma ElementAppend(#view, #outerLoopReadPos, #fieldsPerElement - #fieldsLeft, #doneElList, #doneElLength, #fieldsLeft, #fld, #rfld, #remainingElLength) */
        element.push(fieldBinary)
      }
      /* @tactic
          apply lemma ElementsAppend(#view, #readPos, (#elementCount - #elementsLeft), #fieldsPerElement, #doneElsList, #doneElsLength, #doneElList, #doneElLength) */
      elements.push(element);
    }

    return { elements, readPos }
}

/************************************
 ************************************
 *******                      *******
 *******   EncryptedDataKey   *******
 *******                      *******
 ************************************
 ************************************/

/*
    @pred EDKPrototype () :
      JSObjGeneral($l_edk_proto, null, "Object", false) *
      empty_fields($l_edk_proto : -{ }-);

    @pred nounfold EncryptedDataKey(+EDK, pId:Str, pInfo:Str, encryptedDataKey:List, rawInfo:List) :
        JSObjGeneral(EDK, $l_edk_proto, "Object", false) *
        readOnlyProperty(EDK, "providerId", pId) *
        readOnlyProperty(EDK, "providerInfo", pInfo) *
        readOnlyProperty(EDK, "encryptedDataKey", #aEDK) *
            Uint8Array(#aEDK, #abEDK, 0, #viewSizeEDK) *
            ArrayBuffer(#abEDK, encryptedDataKey) *
            (#viewSizeEDK == l-len encryptedDataKey) *
        readOnlyProperty(EDK, "rawInfo", #aRInfo) *
            Uint8Array(#aRInfo, #abRInfo, 0, #viewSizeRInfo) *
            ArrayBuffer(#abRInfo, rawInfo) *
            (#viewSizeRInfo == l-len rawInfo);

    @id EncryptedDataKey

    @onlyspec EncryptedDataKey (edk)
    [[
        (edk == #edk) *
        JSObject(#edk) *
        DataProp(#edk, "providerId", #pId) * types(#pId : Str) *
        DataProp(#edk, "providerInfo", #pInfo) * types(#pInfo : Str) *
        DataProp(#edk, "encryptedDataKey", #aEDK) *
            Uint8Array (#aEDK, #abEDK, 0, #viewSizeEDK) *
            ArrayBuffer(#abEDK, #encryptedDataKey) *
            (#viewSizeEDK == l-len #encryptedDataKey) *
        DataProp(#edk, "rawInfo", #aRInfo) *
            Uint8Array (#aRInfo, #abRInfo, 0, #viewSizeRInfo) *
            ArrayBuffer(#abRInfo, #rawInfo) *
            (#viewSizeRInfo == l-len #rawInfo) *
        JSObjWithProto (this, $l_edk_proto)
    ]]
    [[
        JSObject(#edk) *
        DataProp(#edk, "providerId", #pId) *
        DataProp(#edk, "providerInfo", #pInfo) *
        DataProp(#edk, "encryptedDataKey", #aEDK) *
            Uint8Array (#aEDK, #abEDK, 0, #viewSizeEDK) *
            ArrayBuffer(#abEDK, #encryptedDataKey) *
        DataProp(#edk, "rawInfo", #aRInfo) *
            Uint8Array (#aRInfo, #abRInfo, 0, #viewSizeRInfo) *
            ArrayBuffer(#abRInfo, #rawInfo) *

        EncryptedDataKey(this, #pId, #pInfo, #encryptedDataKey, #rawInfo) *
        (ret == this)
    ]]
    normal
*/
var EncryptedDataKey = function (edk) { };

/**************************
 **************************
 *******            *******
 *******   toUtf8   *******
 *******            *******
 **************************
 **************************/

 /*
    @id toUtf8

    @onlyspec toUtf8 (buffer)
        [[
            (buffer == #buffer) *
            Uint8Array (#buffer, #ab, 0, #length) *
            ArrayBuffer(#ab, #element)
        ]]
        [[
            Uint8Array (#buffer, #ab, 0, #length) *
            ArrayBuffer(#ab, #element) *
            toUtf8(#element, ret)
        ]]
        normal
*/
var toUtf8 = function (buffer) { };

/************************************************
 ************************************************
 *******                                  *******
 *******   deserializeEncryptedDataKeys   *******
 *******                                  *******
 ************************************************
 ************************************************/

/*
    @id deserializeEncryptedDataKeys

    @pre (this == undefined) * (buffer == #buffer) * (startPos == #startPos) *
         Uint8Array(#buffer, #aBuffer, #byteOffset, #byteLength) * ArrayBuffer(#aBuffer, #data) *
         (#view == l-sub(#data, #byteOffset, #byteLength)) *
         (#definition == "Complete EDKs") *
         RawEncryptedDataKeys(#definition, #byteLength, #view, #startPos, #EDKCount, #EDKs, #EDKsLength) *

         (0 <=# startPos) * (#startPos + #EDKsLength <=# #byteLength) *

         scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
         scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
         JSInternals()

    @post Uint8Array(#buffer, #aBuffer, #byteOffset, #byteLength) * ArrayBuffer(#aBuffer, #data) *
          (#view == l-sub(#data, #byteOffset, #byteLength)) *
          RawEncryptedDataKeys(#definition, #byteLength, #view, #startPos, #EDKCount, #EDKs, #EDKsLength) *

          JSObject(ret) *
            DataProp(ret, "encryptedDataKeys", #dEDKs) *
                DecodedEncryptedDataKeys(#dEDKs, #EDKs, #EDKCount) *
            DataProp(ret, "readPos", #startPos + #EDKsLength) *

          scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
          scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
          JSInternals ()

    @pre (this == undefined) * (buffer == #buffer) * (startPos == #startPos) *
         Uint8Array(#buffer, #aBuffer, #byteOffset, #byteLength) * ArrayBuffer(#aBuffer, #data) *
         (#view == l-sub(#data, #byteOffset, #byteLength)) *
         (#definition == "Incomplete EDKs") *
         RawEncryptedDataKeys(#definition, #byteLength, #view, #startPos, #EDKCount, #EDKs, #EDKsLength) *

         (0 <=# startPos) * (#startPos <=# #byteLength) *

         scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
         scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
         JSInternals()

    @post Uint8Array(#buffer, #aBuffer, #byteOffset, #byteLength) * ArrayBuffer(#aBuffer, #data) *
          (#view == l-sub(#data, #byteOffset, #byteLength)) *
          RawEncryptedDataKeys(#definition, #byteLength, #view, #startPos, #EDKCount, #EDKs, #EDKsLength) *

          (ret == false) *

          scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
          scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
          JSInternals ()
*/
function deserializeEncryptedDataKeys(buffer, startPos) {
  /* @tactic
      if (#definition = "Complete EDKs") then {
        unfold CompleteRawEncryptedDataKeys(#view, #startPos, #EDKCount, #EDKs, #EDKsLength)
      } else {
        unfold IncompleteRawEncryptedDataKeys(#byteLength, #view, #startPos)
      } */

  /* Precondition: startPos must be within the byte length of the buffer given. */
  needs(
    buffer.byteLength >= startPos && startPos >= 0,
    'startPos out of bounds.'
  )

  /* Check for early return (Postcondition): Need to have at least Uint16 (2) bytes of data. */
  if (startPos + 2 > buffer.byteLength) return false

  /* Uint8Array is a view on top of the underlying ArrayBuffer.
   * This means that raw underlying memory stored in the ArrayBuffer
   * may be larger than the Uint8Array.  This is especially true of
   * the Node.js Buffer object.  The offset and length *must* be
   * passed to the DataView otherwise I will get unexpected results.
   */
  var dataView = new DataView(
    buffer.buffer,
    buffer.byteOffset,
    buffer.byteLength
  )
  var encryptedDataKeysCount = dataView.getUint16(startPos, false) // big endian

  /* Precondition: There must be at least 1 EncryptedDataKey element. */
  needs(encryptedDataKeysCount, 'No EncryptedDataKey found.')

  var elementInfo = readElements(
    encryptedDataKeysCount,
    3,
    buffer,
    startPos + 2
  )
  /* Check for early return (Postcondition): readElement will return false if there is not enough data.
   * I can only continue if I have at least the entire EDK section.
   */
  if (!elementInfo) return false
  var { elements, readPos } = elementInfo

  var encryptedDataKeys = elements.map(
    /**
        @id aux_deserializeEncryptedDataKey

        @pre (element == #element) *
             ArrayOfUInt8Arrays (#element, {{ #rawId, #rawInfo, #encryptedDataKey }}, 3) *
             scope(toUtf8: #toUtf8) * JSFunctionObject(#toUtf8, "toUtf8", #t_sc, #t_len, #t_proto) *
             scope(EncryptedDataKey: #EncryptedDataKey) * JSFunctionObject(#EncryptedDataKey, "EncryptedDataKey", #e_sc, #e_len, $l_edk_proto) *
             GlobalObject()

        @post ArrayOfUInt8Arrays (#element, {{ #rawId, #rawInfo, #encryptedDataKey }}, 3) *
              scope(toUtf8: #toUtf8) * JSFunctionObject(#toUtf8, "toUtf8", #t_sc, #t_len, #t_proto) *
              scope(EncryptedDataKey: #EncryptedDataKey) * JSFunctionObject(#EncryptedDataKey, "EncryptedDataKey", #e_sc, #e_len, $l_edk_proto) *
              GlobalObject() *

              toUtf8(#rawId, #pId) * toUtf8(#rawInfo, #pInfo) *
              EncryptedDataKey(ret, #pId, #pInfo, #encryptedDataKey, #rawInfo)
    */
    element => {
      /* FIXME: Implement the array deconstructor */
      var [rawId, rawInfo, encryptedDataKey] = element
      var providerId = toUtf8(rawId)
      var providerInfo = toUtf8(rawInfo)
      return new EncryptedDataKey({
        providerInfo,
        providerId,
        encryptedDataKey,
        rawInfo,
      })
    }
  )

  Object.freeze(encryptedDataKeys)
  return { encryptedDataKeys, readPos }
}

/********************************************
 ********************************************
 *******                              *******
 *******   AlgorithmSuiteIdentifier   *******
 *******                              *******
 ********************************************
 ********************************************/

/*
    @pred AlgorithmSuiteIdentifierObject(o) :
        JSObject(o) *
        DataProp(o, "20",  "ALG_AES128_GCM_IV12_TAG16") * DataProp(o, "ALG_AES128_GCM_IV12_TAG16", 20) *
        DataProp(o, "70",  "ALG_AES192_GCM_IV12_TAG16") * DataProp(o, "ALG_AES192_GCM_IV12_TAG16", 70) *
        empty_fields(o : -{ "20", "70", "ALG_AES128_GCM_IV12_TAG16", "ALG_AES192_GCM_IV12_TAG16" }-);
*/
var AlgorithmSuiteIdentifier;

/****************************
 ****************************
 *******              *******
 *******   SdkSuite   *******
 *******              *******
 ****************************
 ****************************/

/*
    @pred pure AlgorithmSuite(+numId, stringId, ivLength, tagLength) :
        (numId == 20)  * (stringId == "ALG_AES128_GCM_IV12_TAG16") * (ivLength == 12) * (tagLength == 128),
        (numId == 70)  * (stringId == "ALG_AES192_GCM_IV12_TAG16") * (ivLength == 12) * (tagLength == 128);

    @pred pure BrokenAlgorithmSuite(+numId, errorMessage) :
        (! (numId == 20)) * (! (numId == 70)) * (errorMessage == "Unsupported algorithm suite.");


    @pred AlgorithmSuiteObject(+aso: Obj, ivLength: Num, tagLength: Num) :
        JSObject(aso) *
        readOnlyProperty(aso, "ivLength",  ivLength) *
        readOnlyProperty(aso, "tagLength", tagLength);

    @onlyspec SdkSuite (suiteId)
        [[
            JSObject(this) * ((this, "ivLength") -> none) * ((this, "tagLength") -> none) *
            AlgorithmSuite(suiteId, #stringId, #ivLength, #tagLength)
        ]]
        [[
            AlgorithmSuiteObject(this, #ivLength, #tagLength) *
            (ret == this)
        ]]
        normal
 */
var SdkSuite = function (suiteId) { };

/********************************************
 ********************************************
 *******                              *******
 *******   deserializeMessageHeader   *******
 *******                              *******
 ********************************************
 ********************************************/

/**
    @pred CorrectVersionAndType(+version, +type) :
      (version == 1) * (type == 128);

    @pred BrokenVersionAndType(+version:Num, +type:Num, errorMessage:Str) :
        (version == 65) * (type == 89) * (errorMessage == "Malformed Header: This blob may be base64 encoded."),
        (! (version == 1) \/ ! (type == 128)) * (! (version == 65) \/ ! (type == 89)) * (errorMessage == "Malformed Header.");

    @pred nounfold BrokenHeader(errorMessage, +byteLength, +rawHeaderData, part_one, version, type, suiteId, messageId, rECLength,
                                                                           part_two, EC,
                                                                           part_three, EDKs, contentType, headerIvLength, frameLength, headerLength, headerIv, headerAuthTag) :
        (22 <=# byteLength) *
        (rawHeaderData == l+ ({{ version, type }}, #rest)) *
        BrokenVersionAndType(version, type, errorMessage) *

        (part_one == {{ }}) *
        (suiteId == 0) * (messageId == {{ }}) * (rECLength == 0) *
        (part_two == {{ }}) * (EC == {{ }}) *
        (part_three == {{ }}) * (EDKs == {{ }}) * (contentType == 0) * (headerIvLength == 0) *
        (frameLength == 0) * (headerLength == 0) * (headerIv == {{ }}) * (headerAuthTag == {{ }}),

        (22 <=# byteLength) *
        (rawHeaderData == l+ ({{ version, type }}, #rawSuiteId, #rest)) *
        (l-len #rawSuiteId == 2) *
        CorrectVersionAndType(version, type) *
        rawToUInt16(#rawSuiteId, false, suiteId) *
        BrokenAlgorithmSuite(suiteId, errorMessage) *

        (part_one == {{ }}) * (messageId == {{ }}) * (rECLength == 0) *
        (part_two == {{ }}) * (EC == {{ }}) *
        (part_three == {{ }}) * (EDKs == {{ }}) * (contentType == 0) * (headerIvLength == 0) *
        (frameLength == 0) * (headerLength == 0) * (headerIv == {{ }}) * (headerAuthTag == {{ }}),

		    (22 <=# byteLength) *
        (rawHeaderData == l+ (part_one, part_two)) *
        (l-len part_one == 22) *
        (part_one == l+ (
          {{ version, type }},
          #rawSuiteId,
          messageId,
          #rawContextLength)) *
        (l-len #rawSuiteId == 2) *
        (l-len messageId == 16) *
        (l-len #rawContextLength == 2) *
        CorrectVersionAndType(version, type) *
        rawToUInt16(#rawSuiteId, false, suiteId) *
        AlgorithmSuite(suiteId, #stringId, headerIvLength, #tagLength) *
        rawToUInt16(#rawContextLength, false, rECLength) *
        (22 + rECLength <=# byteLength) *

        (part_two == l+ (EC, part_three)) *
        (part_three == l+ (#edks, {{ contentType }}, #rawReservedBytes, #rest)) *
        (l-len EC == rECLength) *
        CompleteRawEncryptedDataKeys(rawHeaderData, 22 + rECLength, #edkCount, EDKs, #EDKsLength) *
        (#EDKsLength == l-len #edks) *
        (l-len #rawReservedBytes == 4) *
        rawToUInt32(#rawReservedBytes, false, #reservedBytes) *
        (! (#reservedBytes == 0)) *
        (headerLength == 22 + rECLength + #EDKsLength + 1 + 4 + 1 + 4) *
        (headerLength + headerIvLength + (#tagLength / 8) <=# byteLength) *

        (errorMessage == "Malformed Header") *
        (frameLength == 0) * (headerIv == {{ }}) * (headerAuthTag == {{ }}),

        (22 <=# byteLength) *
        (rawHeaderData == l+ (part_one, part_two)) *
        (l-len part_one == 22) *
        (part_one == l+ (
          {{ version, type }},
          #rawSuiteId,
          messageId,
          #rawContextLength)) *
        (l-len #rawSuiteId == 2) *
        (l-len messageId == 16) *
        (l-len #rawContextLength == 2) *
        CorrectVersionAndType(version, type) *
        rawToUInt16(#rawSuiteId, false, suiteId) *
        AlgorithmSuite(suiteId, #stringId, #ivLength, #tagLength) *
        rawToUInt16(#rawContextLength, false, rECLength) *
        (22 + rECLength <=# byteLength) *

        (part_two == l+ (EC, part_three)) *
        (part_three == l+ (#edks, {{ contentType }}, {{ 0, 0, 0, 0 }}, {{ headerIvLength }}, #rest)) *
        (l-len EC == rECLength) *
        CompleteRawEncryptedDataKeys(rawHeaderData, 22 + rECLength, #edkCount, EDKs, #EDKsLength) *
        (#EDKsLength == l-len #edks) *
        (headerLength == 22 + rECLength + #EDKsLength + 1 + 4 + 1 + 4) *
        (headerLength + #ivLength + (#tagLength / 8) <=# byteLength) *
        (! (headerIvLength == #ivLength)) *

        (errorMessage == "Malformed Header") *
        (frameLength == 0) * (headerIv == {{ }}) * (headerAuthTag == {{ }});

    @pred nounfold IncompleteHeader(+byteLength, +rawHeaderData, part_one, version, type, suiteId, messageId, rECLength,
                                                                 part_two, EC,
                                                                 part_three, EDKs, contentType, headerIvLength, frameLength, headerLength, headerIv, headerAuthTag) :
        (byteLength <# 22) *

        (part_one == {{ }}) * (version == 0) * (type == 0) * (suiteId == 0) * (messageId == {{ }}) * (rECLength == 0) *
        (part_two == {{ }}) * (EC == {{ }}) *
        (part_three == {{ }}) * (EDKs == {{ }}) * (contentType == 0) * (headerIvLength == 0) *
        (frameLength == 0) * (headerLength == 0) * (headerIv == {{ }}) * (headerAuthTag == {{ }}),

        (22 <=# byteLength) *
        (rawHeaderData == l+ (part_one, part_two)) *
        (l-len part_one == 22) *
        (part_one == l+ ({{ version, type }}, #rawSuiteId, messageId, #rawContextLength)) *
        (l-len #rawSuiteId == 2) *
        (l-len messageId == 16) *
        (l-len #rawContextLength == 2) *
        CorrectVersionAndType(version, type) *
        rawToUInt16(#rawSuiteId, false, suiteId) *
        AlgorithmSuite(suiteId, #stringId, headerIvLength, #tagLength) *
        rawToUInt16(#rawContextLength, false, rECLength) *
        (byteLength <# 22 + rECLength) *

        (EC == {{ }}) *
        (part_three == {{ }}) * (EDKs == {{ }}) * (contentType == 0) *
        (frameLength == 0) * (headerLength == 0) * (headerIv == {{ }}) * (headerAuthTag == {{ }}),

        (22 <=# byteLength) *
        (rawHeaderData == l+ (part_one, part_two)) *
        (l-len part_one == 22) *
        (part_one == l+ ({{ version, type }}, #rawSuiteId, messageId, #rawContextLength)) *
        (l-len #rawSuiteId == 2) *
        (l-len messageId == 16) *
        (l-len #rawContextLength == 2) *
        CorrectVersionAndType(version, type) *
        rawToUInt16(#rawSuiteId, false, suiteId) *
        AlgorithmSuite(suiteId, #stringId, headerIvLength, #tagLength) *
        rawToUInt16(#rawContextLength, false, rECLength) *
        (22 + rECLength <=# byteLength) *

        (part_two == l+ (EC, part_three)) *
        (l-len EC == rECLength) *

        IncompleteRawEncryptedDataKeys(byteLength, rawHeaderData, 22 + rECLength) *
        (EDKs == {{ }}) * (contentType == 0) *
        (frameLength == 0) * (headerLength == 0) * (headerIv == {{ }}) * (headerAuthTag == {{ }}),

        (22 <=# byteLength) *
        (rawHeaderData == l+ (part_one, part_two)) *
        (l-len part_one == 22) *
        (part_one == l+ ({{ version, type }}, #rawSuiteId, messageId, #rawContextLength)) *
        (l-len #rawSuiteId == 2) *
        (l-len messageId == 16) *
        (l-len #rawContextLength == 2) *
        CorrectVersionAndType(version, type) *
        rawToUInt16(#rawSuiteId, false, suiteId) *
        AlgorithmSuite(suiteId, #stringId, headerIvLength, #tagLength) *
        rawToUInt16(#rawContextLength, false, rECLength) *
        (22 + rECLength <=# byteLength) *

        (part_two == l+ (EC, part_three)) *
        (l-len EC == rECLength) *

        (part_three == l+ (#edks, {{ contentType }}, {{ 0, 0, 0, 0 }}, {{ headerIvLength }}, #rawFrameLength, #rest)) *
        CompleteRawEncryptedDataKeys(rawHeaderData, 22 + rECLength, #edkCount, EDKs, #EDKsLength) *
        (#EDKsLength == l-len #edks) *
        (l-len #rawFrameLength == 4) *
        rawToUInt32(#rawFrameLength, false, frameLength) *
        (headerLength == 22 + rECLength + #EDKsLength + 1 + 4 + 1 + 4) *
        (l-len headerIv == headerIvLength) *
        (l-len headerAuthTag == #tagLength / 8) *
        (byteLength <# headerLength + headerIvLength + (#tagLength / 8)) *

        (headerIv == {{ }}) * (headerAuthTag == {{ }});

    @pred nounfold CompleteHeader(+rawHeaderData, part_one, version, type, suiteId, messageId, rECLength,
                                                  part_two, EC,
                                                  part_three, EDKs, contentType, headerIvLength, frameLength, headerLength, headerIv, headerAuthTag) :
        (rawHeaderData == l+ (part_one, part_two)) *
        (l-len part_one == 22) *
        (part_one == l+ (
          {{ version, type }},
          #rawSuiteId,
          messageId,
          #rawContextLength)) *
        (l-len #rawSuiteId == 2) *
        (l-len messageId == 16) *
        (l-len #rawContextLength == 2) *
        CorrectVersionAndType(version, type) *
        rawToUInt16(#rawSuiteId, false, suiteId) *
        AlgorithmSuite(suiteId, #stringId, headerIvLength, #tagLength) *
        rawToUInt16(#rawContextLength, false, rECLength) *

        (part_two == l+ (EC, part_three)) *
        (part_three == l+ (#edks, {{ contentType }}, {{ 0, 0, 0, 0 }}, {{ headerIvLength }}, #rawFrameLength, headerIv, headerAuthTag)) *
        (l-len EC == rECLength) *
        CompleteRawEncryptedDataKeys(rawHeaderData, 22 + rECLength, #edkCount, EDKs, #EDKsLength) *
        (#EDKsLength == l-len #edks) *
        (l-len #rawFrameLength == 4) *
        rawToUInt32(#rawFrameLength, false, frameLength) *
        (headerLength == 22 + rECLength + #EDKsLength + 1 + 4 + 1 + 4) *
        (l-len headerIv == headerIvLength) *
        (l-len headerAuthTag == #tagLength / 8) *
        (headerLength + headerIvLength + (#tagLength / 8) <=# l-len rawHeaderData);

    @pred MessageHeader(+messageHeader, version, type, suiteId, messageId, EDKs, contentType, headerIvLength, frameLength) :
        JSObject(messageHeader) *
        DataProp(messageHeader, "version", version) *
        DataProp(messageHeader, "type", type) *
        DataProp(messageHeader, "suiteId", suiteId) *
        DataProp(messageHeader, "messageId", #ui8aMessageId) *
            Uint8Array(#ui8aMessageId, #abMessageId, 0, 16) *
            ArrayBuffer(#abMessageId, messageId) *
        DataProp(messageHeader, "encryptedDataKeys", #dEDKs) *
            DecodedEncryptedDataKeys(#dEDKs, EDKs, #keyCount) *
        DataProp(messageHeader, "contentType", contentType) *
        DataProp(messageHeader, "headerIvLength", headerIvLength) *
        DataProp(messageHeader, "frameLength", frameLength);

    @pred nounfold HeaderInfo(+headerInfo, version, type, suiteId, messageId, EDKs, contentType, headerIvLength,
                              frameLength, headerLength, rawHeaderData, headerIv, headerAuthTag) :
        JSObject(headerInfo) *
        DataProp(headerInfo, "messageHeader", #messageHeader) *
            MessageHeader(#messageHeader, version, type, suiteId, messageId, EDKs, contentType, headerIvLength, frameLength) *
        DataProp(headerInfo, "headerLength", headerLength) *
        DataProp(headerInfo, "algorithmSuite", #algoSuiteObject) *
            AlgorithmSuiteObject(#algoSuiteObject, headerIvLength, #tagLength) *
        DataProp(headerInfo, "rawHeader", #rawHeader) *
            Uint8Array(#rawHeader, #rawBuffer, 0, headerLength) *
            ArrayBuffer(#rawBuffer, rawHeaderData) *
        DataProp(headerInfo, "headerIv", #ui8aHeaderIv) *
            Uint8Array(#ui8aHeaderIv, #abHeaderIv, 0, headerIvLength) *
            ArrayBuffer(#abHeaderIv, headerIv) *
        DataProp(headerInfo, "headerAuthTag", #ui8aHeaderAuthTag) *
            Uint8Array(#ui8aHeaderAuthTag, #abHeaderAuthTag, 0, #tagLength / 8) *
            ArrayBuffer(#abHeaderAuthTag, headerAuthTag);
*/

/**
  @pred nounfold Header(definition, +byteLength,
                        +rawHeaderData, part_one, version, type, suiteId, messageId, rECLength,
                                        part_two, EC,
                                        part_three, EDKs, contentType, headerIvLength, frameLength, headerLength, headerIv, headerAuthTag,
                        errorMessage) :

        (definition == "Complete Header") *
        CompleteHeader(rawHeaderData, part_one, version, type, suiteId, messageId, rECLength,
                                      part_two, EC,
                                      part_three, EDKs, contentType, headerIvLength, frameLength, headerLength, headerIv, headerAuthTag) *
        (errorMessage == ""),

        (definition == "Incomplete Header") *
        IncompleteHeader(byteLength, rawHeaderData, part_one, version, type, suiteId, messageId, rECLength,
                                                    part_two, EC,
                                                    part_three, EDKs, contentType, headerIvLength, frameLength, headerLength, headerIv, headerAuthTag) *
        (errorMessage == ""),

        (definition == "Broken Header") *
        BrokenHeader(errorMessage, byteLength, rawHeaderData, part_one, version, type, suiteId, messageId, rECLength,
                                                              part_two, EC,
                                                              part_three, EDKs, contentType, headerIvLength, frameLength, headerLength, headerIv, headerAuthTag);

*/

/**
    @id deserializeMessageHeader

    @pre (messageBuffer == #messageBuffer) *
         Uint8Array(#messageBuffer, #buffer, #byteOffset, #byteLength) * ArrayBuffer(#buffer, #data) *
         (#view == l-sub(#data, #byteOffset, #byteLength)) *
         (#byteOffset + #byteLength <=# l-len #data) *
         (#definition == "Complete Header") *
         Header(#definition, #byteLength,
                #view, #part_one, #version, #type, #suiteId, #messageId, #rECLength,
                       #part_two, #EC,
                       #part_three, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #headerIv, #headerAuthTag,
                #errorMessage) *

         scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
         scope(AlgorithmSuiteIdentifier : #ASIObject) * AlgorithmSuiteIdentifierObject(#ASIObject) *
         scope(deserializeEncryptedDataKeys : #DEDKObject) * JSFunctionObject(#DEDKObject, "deserializeEncryptedDataKeys", #ds_sc, #ds_len, #ds_proto) *
         scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
         scope(SdkSuite : #SdkObject) * JSFunctionObject(#SdkObject, "SdkSuite", #s_sc, #s_len, $lobj_proto) *
         JSInternals ()

    @post Uint8Array(#messageBuffer, #buffer, #byteOffset, #byteLength) * ArrayBuffer(#buffer, #data) *
          Header(#definition, #byteLength,
                 #view, #part_one, #version, #type, #suiteId, #messageId, #rECLength,
                        #part_two, #EC,
                        #part_three, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #headerIv, #headerAuthTag,
                 #errorMessage) *

          HeaderInfo(ret, #version, #type, #suiteId, #messageId, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #rawHeaderData, #headerIv, #headerAuthTag) *
          (#rawHeaderData == l-sub(#view, 0, #headerLength)) *

          scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
          scope(AlgorithmSuiteIdentifier : #ASIObject) * AlgorithmSuiteIdentifierObject(#ASIObject) *
          scope(deserializeEncryptedDataKeys : #DEDKObject) * JSFunctionObject(#DEDKObject, "deserializeEncryptedDataKeys", #ds_sc, #ds_len, #ds_proto) *
          scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
          scope(SdkSuite : #SdkObject) * JSFunctionObject(#SdkObject, "SdkSuite", #s_sc, #s_len, $lobj_proto) *
          JSInternals()

    @pre (messageBuffer == #messageBuffer) *
         Uint8Array(#messageBuffer, #buffer, #byteOffset, #byteLength) * ArrayBuffer(#buffer, #data) *
         (#view == l-sub(#data, #byteOffset, #byteLength)) *
         (#byteOffset + #byteLength <=# l-len #data) *
         (#definition == "Incomplete Header") *
         Header(#definition, #byteLength,
                #view, #part_one, #version, #type, #suiteId, #messageId, #rECLength,
                       #part_two, #EC,
                       #part_three, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #headerIv, #headerAuthTag,
                #errorMessage) *

         scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
         scope(AlgorithmSuiteIdentifier : #ASIObject) * AlgorithmSuiteIdentifierObject(#ASIObject) *
         scope(deserializeEncryptedDataKeys : #DEDKObject) * JSFunctionObject(#DEDKObject, "deserializeEncryptedDataKeys", #ds_sc, #ds_len, #ds_proto) *
         scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
         scope(SdkSuite : #SdkObject) * JSFunctionObject(#SdkObject, "SdkSuite", #s_sc, #s_len, $lobj_proto) *
         JSInternals ()

    @post Uint8Array(#messageBuffer, #buffer, #byteOffset, #byteLength) * ArrayBuffer(#buffer, #data) *
          Header(#definition, #byteLength,
                 #view, #part_one, #version, #type, #suiteId, #messageId, #rECLength,
                        #part_two, #EC,
                        #part_three, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #headerIv, #headerAuthTag,
                 #errorMessage) *

          (ret == false) *

          scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
          scope(AlgorithmSuiteIdentifier : #ASIObject) * AlgorithmSuiteIdentifierObject(#ASIObject) *
          scope(deserializeEncryptedDataKeys : #DEDKObject) * JSFunctionObject(#DEDKObject, "deserializeEncryptedDataKeys", #ds_sc, #ds_len, #ds_proto) *
          scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
          scope(SdkSuite : #SdkObject) * JSFunctionObject(#SdkObject, "SdkSuite", #s_sc, #s_len, $lobj_proto) *
          JSInternals()

    @pre (messageBuffer == #messageBuffer) *
         Uint8Array(#messageBuffer, #buffer, #byteOffset, #byteLength) * ArrayBuffer(#buffer, #data) *
         (#view == l-sub(#data, #byteOffset, #byteLength)) *
         (#byteOffset + #byteLength <=# l-len #data) *
         (#definition == "Broken Header") *
         Header(#definition, #byteLength,
                #view, #part_one, #version, #type, #suiteId, #messageId, #rECLength,
                       #part_two, #EC,
                       #part_three, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #headerIv, #headerAuthTag,
                #errorMessage) *

         scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
         scope(AlgorithmSuiteIdentifier : #ASIObject) * AlgorithmSuiteIdentifierObject(#ASIObject) *
         scope(deserializeEncryptedDataKeys : #DEDKObject) * JSFunctionObject(#DEDKObject, "deserializeEncryptedDataKeys", #ds_sc, #ds_len, #ds_proto) *
         scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
         scope(SdkSuite : #SdkObject) * JSFunctionObject(#SdkObject, "SdkSuite", #s_sc, #s_len, $lobj_proto) *
         JSInternals ()

    @posterr
          Uint8Array(#messageBuffer, #buffer, #byteOffset, #byteLength) * ArrayBuffer(#buffer, #data) *
          Header(#definition, #byteLength,
                 #view, #part_one, #version, #type, #suiteId, #messageId, #rECLength,
                        #part_two, #EC,
                        #part_three, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #headerIv, #headerAuthTag,
                 #errorMessage) *

          ErrorObjectWithMessage(ret, #errorMessage) *

          scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
          scope(AlgorithmSuiteIdentifier : #ASIObject) * AlgorithmSuiteIdentifierObject(#ASIObject) *
          scope(deserializeEncryptedDataKeys : #DEDKObject) * JSFunctionObject(#DEDKObject, "deserializeEncryptedDataKeys", #ds_sc, #ds_len, #ds_proto) *
          scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
          scope(SdkSuite : #SdkObject) * JSFunctionObject(#SdkObject, "SdkSuite", #s_sc, #s_len, $lobj_proto) *
          JSInternals()
*/

function deserializeMessageHeader(messageBuffer) {
    /* Uint8Array is a view on top of the underlying ArrayBuffer.
    * This means that raw underlying memory stored in the ArrayBuffer
    * may be larger than the Uint8Array.  This is especially true of
    * the Node.js Buffer object.  The offset and length *must* be
    * passed to the DataView otherwise I will get unexpected results.
    */
    /* @tactic unfold Header(#definition, #byteLength, #view, #part_one, #version, #type, #suiteId, #messageId, #rECLength, #part_two, #EC, #part_three, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #headerIv, #headerAuthTag, #errorMessage) */
    /* @tactic
        if (#definition = "Complete Header") then {
          unfold CompleteHeader(#view, #part_one, #version, #type, #suiteId, #messageId, #rECLength, #part_two, #EC, #part_three, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #headerIv, #headerAuthTag)
        } else { if (#definition = "Incomplete Header") then {
            unfold IncompleteHeader(#byteLength, #view, #part_one, #version, #type, #suiteId, #messageId, #rECLength, #part_two, #EC, #part_three, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #headerIv, #headerAuthTag)
          } else {
            unfold BrokenHeader(#errorMessage, #byteLength, #view, #part_one, #version, #type, #suiteId, #messageId, #rECLength, #part_two, #EC, #part_three, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #headerIv, #headerAuthTag)
          }
        }*/
    var dataView = new DataView(
      messageBuffer.buffer,
      messageBuffer.byteOffset,
      messageBuffer.byteLength
    )

    /* Check for early return (Postcondition): Not Enough Data. Need to have at least 22 bytes of data to begin parsing.
    * The first 22 bytes of the header are fixed length.  After that
    * there are 2 variable length sections.
    */
    if (dataView.byteLength < 22) return false // not enough data

    var version = dataView.getUint8(0)
    var type = dataView.getUint8(1)
    /* Precondition: version and type must be the required values. */
    needs(
      version === 1 && type === 128,
      version === 65 && type === 89
        ? 'Malformed Header: This blob may be base64 encoded.'
        : 'Malformed Header.'
    )

    var suiteId = dataView.getUint16(2, false)  // big endian
    /* Precondition: suiteId must match supported algorithm suite */
    needs(AlgorithmSuiteIdentifier[suiteId], 'Unsupported algorithm suite.')
    var messageId = messageBuffer.slice(4, 20)
    var contextLength = dataView.getUint16(20, false) // big endian

    /* Check for early return (Postcondition): Not Enough Data. Need to have all of the context in bytes before we can parse the next section.
    * This is the first variable length section.
    */
    if (22 + contextLength > dataView.byteLength) return false // not enough data

    var encryptionContext = // decodeEncryptionContext(
      messageBuffer.slice(22, 22 + contextLength)
    // )

    var dataKeyInfo = deserializeEncryptedDataKeys(
      messageBuffer,
      22 + contextLength
    )

    /* Check for early return (Postcondition): Not Enough Data. deserializeEncryptedDataKeys will return false if it does not have enough data.
    * This is the second variable length section.
    */
    if (!dataKeyInfo) return false // not enough data

    var { encryptedDataKeys, readPos } = dataKeyInfo

    /* I'm doing this here, after decodeEncryptionContext and deserializeEncryptedDataKeys
     * because they are the bulk of the header section.
     */
    var algorithmSuite = new SdkSuite(suiteId)
    var { ivLength, tagLength } = algorithmSuite
    var tagLengthBytes = tagLength / 8
    var headerLength = readPos + 1 + 4 + 1 + 4

    /* Check for early return (Postcondition): Not Enough Data. Need to have the remaining fixed length data to parse. */
    if (headerLength + ivLength + tagLengthBytes > dataView.byteLength)
      return false // not enough data

    var contentType = dataView.getUint8(readPos)
    var reservedBytes = dataView.getUint32(readPos + 1, false) // big endian
    /* Postcondition: reservedBytes are defined as 0,0,0,0
     * See: https://docs.aws.amazon.com/encryption-sdk/latest/developer-guide/message-format.html#header-reserved
     */
    needs(reservedBytes === 0, 'Malformed Header')
    var headerIvLength = dataView.getUint8(readPos + 1 + 4)
    /* Postcondition: The headerIvLength must match the algorithm suite specification. */
    needs(headerIvLength === ivLength, 'Malformed Header')
    var frameLength = dataView.getUint32(readPos + 1 + 4 + 1, false) // big endian
    var rawHeader = messageBuffer.slice(0, headerLength)

    var messageHeader = {
      version,
      type,
      suiteId,
      messageId,
      encryptionContext,
      encryptedDataKeys,
      contentType,
      headerIvLength,
      frameLength,
    }

    var headerIv = messageBuffer.slice(headerLength, headerLength + ivLength)
    var headerAuthTag = messageBuffer.slice(
      headerLength + ivLength,
      headerLength + ivLength + tagLengthBytes
    )

    return {
      messageHeader,
      headerLength,
      rawHeader,
      algorithmSuite,
      headerIv,
      headerAuthTag,
    }
}