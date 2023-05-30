'use strict';

/**
   @import AmazonLogic.jsil
   @import EncryptionHeaderLogic.gil
   @import ListLogic.gil
   @import Utf8Logic.gil
 */

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
function needs(condition, errorMessage) {
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

/**
    @id readElements

    @pred nounfold innerLoopInvariantFacts(+definition, +remElsList, +view, +innerLoopReadPos, +fLeft, +remElList, +eLength, +remElsLength, +doneElLength, remElLength) :
      (definition == "Complete") * CElement(view, innerLoopReadPos, fLeft, remElList, remElLength) * (eLength == doneElLength + remElLength),
      (definition == "Incomplete") * (remElsList == {{ }}) * IElement(view, innerLoopReadPos, fLeft, remElList, remElLength) * (remElsLength == doneElLength + remElLength),
      (definition == "Incomplete") * (! (remElsList == {{ }})) * CElement(view, innerLoopReadPos, fLeft, remElList, remElLength) * (eLength == doneElLength + remElLength);

    @pre
      (elementCount == #eCount) * (fieldsPerElement == #fCount) * (buffer == #buffer) * (readPos == #readPos) *
      Uint8Array (#buffer, #ab, #viewOffset, #viewSize) *
      ArrayBuffer(#ab, #data) *
      (#view == l-sub(#data, #viewOffset, #viewSize)) *
      Elements(#definition, #view, #readPos, #eCount, #fCount, #eList, #esLength) *

      scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
      JSInternals ()

    @post
      (#definition == "Complete") *
      Uint8Array (#buffer, #ab, #viewOffset, #viewSize) *
      ArrayBuffer(#ab, #data) *
      Elements(#definition, #view, #readPos, #eCount, #fCount, #eList, #esLength) *
      scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
      JSInternals () *

      JSObject(ret) *
        DataProp(ret, "elements", #elements) *
            ArrayOfArraysOfUInt8Arrays(#elements, #eList) *
        DataProp(ret, "readPos", #ret_readPos) *
            (#ret_readPos == #readPos + #esLength);

      (#definition == "Incomplete") *
      Uint8Array (#buffer, #ab, #viewOffset, #viewSize) *
      ArrayBuffer(#ab, #data) *
      Elements(#definition, #view, #readPos, #eCount, #fCount, #eList, #esLength) *

      scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
      JSInternals () *

      (ret == false)
*/
function readElements(elementCount, fieldsPerElement, buffer, readPos) {
  var dataView = new DataView(
    buffer.buffer,
    buffer.byteOffset,
    buffer.byteLength
  );

  needs(readPos >= 0 && dataView.byteLength >= readPos, 'readPos out of bounds.')
  /* Precondition: elementCount and fieldsPerElement must be non-negative. */
  needs(elementCount >= 0 && fieldsPerElement >= 0, 'elementCount and fieldsPerElement must be non-negative.')

  var elements = [];

  /* @invariant
      scope(buffer: #buffer) * Uint8Array (#buffer, #ab, #viewOffset, #viewSize) * ArrayBuffer(#ab, #data) *
      scope(dataView: #dataView) * DataView(#dataView, #ab, #viewOffset, #viewSize) *
      scope(elements : #doneEls) * scope(readPos : #outerLoopReadPos) *
      scope(elementCount : #eLeft) * scope(fieldsPerElement: #fCount) *
      scope(element: _) * scope(fieldCount: _) * scope(fieldBinary: _) * scope(length: _) *
      JSInternals() *

      CElements(#view, #readPos, #eCount - #eLeft, #fCount, #doneElsList, #doneElsLength) *
      Elements(#definition, #view, #outerLoopReadPos, #eLeft, #fCount, #remElsList, #remElsLength) *
      (#eList == l+ (#doneElsList, #remElsList)) *
      (#esLength == #doneElsLength + #remElsLength) *
      (#readPos + #doneElsLength == #outerLoopReadPos) *
      ArrayOfArraysOfUInt8Arrays(#doneEls, #doneElsList)
      [bind : #doneEls, #outerLoopReadPos, #eLeft, #remElsList, #remElsLength, #doneElsList, #doneElsLength] */
  while (elementCount--) {
    /* @tactic
        unfold Elements(#definition, #view, #outerLoopReadPos, #eLeft, #fCount, #remElsList, #remElsLength);
        if (#definition = "Complete") then {
            unfold CElements(#view, #outerLoopReadPos, #eLeft, #fCount, #remElsList, #remElsLength) [bind: (#fList := #element) and (#eLength := #eLength)]
        } else {
            unfold IElements(#view, #outerLoopReadPos, #eLeft, #fCount, #remElsList, #remElsLength) [bind: (#fList := #fList) and (#eLength := #eLength)]
        } */
    var element = []
    var fieldCount = fieldsPerElement

    /* @invariant
        scope(buffer: #buffer) * Uint8Array (#buffer, #ab, #viewOffset, #viewSize) * ArrayBuffer(#ab, #data) *
        scope(dataView: #dataView) * DataView(#dataView, #ab, #viewOffset, #viewSize) *
        scope(element : #doneEl) * scope(readPos : #innerLoopReadPos) * scope(fieldCount : #fLeft) *
        scope(fieldBinary: _) * scope(length: _) *
        JSInternals() *

        CElement(#view, #outerLoopReadPos, #fCount - #fLeft, #doneElList, #doneElLength) *
        (#fList == l+ (#doneElList, #remElList)) *
        innerLoopInvariantFacts(#definition, #remElsList, #view, #innerLoopReadPos, #fLeft, #remElList, #eLength, #remElsLength, #doneElLength, #remElLength) *
        (#outerLoopReadPos + #doneElLength == #innerLoopReadPos) *
        ArrayOfUInt8Arrays(#doneEl, #doneElList, #fCount - #fLeft)
        [bind: #doneEl, #innerLoopReadPos, #fLeft, #remElList, #remElLength, #doneElList, #doneElLength] */
    while (fieldCount--) {
      /* @tactic
          unfold innerLoopInvariantFacts(#definition, #remElsList, #view, #innerLoopReadPos, #fLeft, #remElList, #eLength, #remElsLength, #doneElLength, #remElLength);
          if (#definition = "Complete") then {
              unfold CElement(#view, #innerLoopReadPos, #fLeft, #remElList, #remElLength)
          } else {
              if (#remElsList = {{ }}) then {
                  unfold IElement(#view, #innerLoopReadPos, #fLeft, #remElList, #remElLength)
              } else {
                  unfold CElement(#view, #innerLoopReadPos, #fLeft, #remElList, #remElLength)
              }
          } */
      if (readPos + 2 > dataView.byteLength)
        /* @tactic
            apply PrependCElementI(#view, #outerLoopReadPos, (#fCount - #fLeft), #doneElList, #doneElLength, #fLeft, #remElList, #remElLength);
            assert IElement(#view, #outerLoopReadPos, #fCount, #fList, #remElsLength);
            assert Elements(#definition, #view, #outerLoopReadPos, #eLeft, #fCount, #remElsList, #remElsLength);
            apply PrependCElementsE(#definition, #view, #readPos, (#eCount - #eLeft), #fCount, #doneElsList, #doneElsLength, #eLeft, #remElsList, #remElsLength)
        */
        return false

      var length = dataView.getUint16(readPos, false) // big endian
      readPos += 2

      if (readPos + length > dataView.byteLength)
        /* @tactic
            apply PrependCElementI(#view, #outerLoopReadPos, (#fCount - #fLeft), #doneElList, #doneElLength, #fLeft, #remElList, #remElLength);
            assert IElement(#view, #outerLoopReadPos, #fCount, #fList, #remElsLength);
            assert Elements(#definition, #view, #outerLoopReadPos, #eLeft, #fCount, #remElsList, #remElsLength);
            apply PrependCElementsE(#definition, #view, #readPos, (#eCount - #eLeft), #fCount, #doneElsList, #doneElsLength, #eLeft, #remElsList, #remElsLength)
        */
        return false

      var fieldBinary = buffer.slice(readPos, readPos + length)
      readPos += length
      /* @tactic
          assert (#remElList == #fld :: #rfld) [bind: #fld, #rfld];
          if ((#definition = "Complete") or (not (#remElsList = {{ }}))) then {
              apply AppendFieldCC(#view, #outerLoopReadPos, #fCount - #fLeft, #doneElList, #doneElLength, #fLeft, #fld, #rfld, #remElLength)
          } else {
              apply AppendFieldCI(#view, #outerLoopReadPos, #fCount - #fLeft, #doneElList, #doneElLength, #fLeft, #fld, #rfld, #remElLength)
          } */
      element.push(fieldBinary)
    }

    /* @tactic
        unfold CElement(#view, #innerLoopReadPos, #fLeft, #remElList, #remElLength);
        apply CElementsAppend(#view, #readPos, (#eCount - #eLeft), #fCount, #doneElsList, #doneElsLength, #doneElList, #doneElLength) */
    elements.push(element);
  }

  /* @tactic
      unfold Elements(#definition, #view, #outerLoopReadPos, #eLeft, #fCount, #remElsList, #remElsLength);
      if (#definition = "Complete") then {
          unfold CElements(#view, #outerLoopReadPos, #eLeft, #fCount, #remElsList, #remElsLength)
      } else {
          unfold IElements(#view, #outerLoopReadPos, #eLeft, #fCount, #remElsList, #remElsLength)
      } */
  return { elements, readPos }
}

/************************************
 ************************************
 *******                      *******
 *******   EncryptedDataKey   *******
 *******                      *******
 ************************************
 ************************************/

/**
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

/**
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

/*******************************************
 *******************************************
 *******                             *******
 *******   decodeEncryptionContext   *******
 *******                             *******
 *******************************************
 *******************************************/

/**

    @pred nounfold UniqueOrDuplicated(definition:Str, lst1:List, lst2:List, lst3:List) :
        (definition == "Complete") * Unique(lst1),
        (definition == "Broken") * Duplicated(lst2, lst3);

    @id decodeEncryptionContext

    @pre (this == undefined) * (encodedEncryptionContext == #eEC) *
         Uint8Array(#eEC, #aBuffer, #byteOffset, #byteLength) * ArrayBuffer(#aBuffer, #data) *
         (#EC == l-sub(#data, #byteOffset, #byteLength)) *
         (#definition == "Complete") *
         RawEncryptionContext(#definition, #EC, #ECKs, #errorMessage) *

         scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
         scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
         scope(toUtf8: #toUtf8) * JSFunctionObject(#toUtf8, "toUtf8", #t_sc, #t_len, #t_proto) *
         JSInternals()

    @post Uint8Array(#eEC, #aBuffer, #byteOffset, #byteLength) * ArrayBuffer(#aBuffer, #data) *
          RawEncryptionContext(#definition, #EC, #ECKs, #errorMessage) *

          DecodedEncryptionContext(ret, #ECKs) *

          scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
          scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
          scope(toUtf8: #toUtf8) * JSFunctionObject(#toUtf8, "toUtf8", #t_sc, #t_len, #t_proto) *
          JSInternals ()

    @pre (this == undefined) * (encodedEncryptionContext == #eEC) *
         Uint8Array(#eEC, #aBuffer, #byteOffset, #byteLength) * ArrayBuffer(#aBuffer, #data) *
         (#EC == l-sub(#data, #byteOffset, #byteLength)) *
         (#definition == "Broken") *
         RawEncryptionContext(#definition, #EC, #ECKs, #errorMessage) *

         scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
         scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
         scope(toUtf8: #toUtf8) * JSFunctionObject(#toUtf8, "toUtf8", #t_sc, #t_len, #t_proto) *
         JSInternals()

    @posterr
          Uint8Array(#eEC, #aBuffer, #byteOffset, #byteLength) * ArrayBuffer(#aBuffer, #data) *
          RawEncryptionContext(#definition, #EC, #ECKs, #errorMessage) *

          ErrorObjectWithMessage(ret, #errorMessage) *

          scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
          scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
          scope(toUtf8: #toUtf8) * JSFunctionObject(#toUtf8, "toUtf8", #t_sc, #t_len, #t_proto) *
          JSInternals ()
*/
function decodeEncryptionContext(encodedEncryptionContext) {
  /* @tactic
      if (#definition = "Complete") then {
        unfold CRawEncryptionContext(#EC, #ECKs)
      } else {
        unfold BRawEncryptionContext(#errorMessage, #EC, #ECKs)
      }
   */

  var encryptionContext = Object.create(null)
  /* Check for early return (Postcondition): The case of 0 length is defined as an empty object. */
  if (!encodedEncryptionContext.byteLength) {
    /* @tactic use_subst [object_table : (#PVPairs: {{ }}) ] */
    Object.freeze(encryptionContext);
    return encryptionContext
  }
  /* Uint8Array is a view on top of the underlying ArrayBuffer.
    * This means that raw underlying memory stored in the ArrayBuffer
    * may be larger than the Uint8Array.  This is especially true of
    * the Node.js Buffer object.  The offset and length *must* be
    * passed to the DataView otherwise I will get unexpected results.
    */
  var dataView = new DataView(
    encodedEncryptionContext.buffer,
    encodedEncryptionContext.byteOffset,
    encodedEncryptionContext.byteLength
  )
  var pairsCount = dataView.getUint16(0, false) // big endian
  var elementInfo = readElements(pairsCount, 2, encodedEncryptionContext, 2)
  /* Postcondition: Since the encryption context has a length, it must have pairs.
    * Unlike the encrypted data key section, the encryption context has a length
    * element.  This means I should always pass the entire section.
    */
  if (!elementInfo) throw new Error('decodeEncryptionContext: Underflow, not enough data.')
  var { elements, readPos } = elementInfo

  /* Postcondition: The byte length of the encodedEncryptionContext must match the readPos. */
  needs(
    encodedEncryptionContext.byteLength === readPos,
    'decodeEncryptionContext: Overflow, too much data.'
  )

  /*
    @tactic
        assert (
            (#EC == l+ ({{ #b0, #b1 }}, #rest)) *
            Elements("Complete", #EC, 2, ((256 * #b0) + #b1), 2, #ECKs, l-len #rest)
        ) [bind: #b0, #b1, #rest];
        unfold Elements("Complete", #EC, 2, ((256 * #b0) + #b1), 2, #ECKs, l-len #rest);
        assert (
            scope(pairsCount: #pairsCount) * (#pairsCount == l-len #ECKs) *
            scope(elements: #elements) * ArrayOfArraysOfUInt8Arrays(#elements, #ECKs) *
            scope(encryptionContext: #dECObj) * JSObjWithProto(#dECObj, null) * empty_fields(#dECObj : -{ }-) *
            toUtf8PairMap(#ECKs, #utf8ECKs) * FirstProj(#ECKs, #rProps) * UniqueOrDuplicated(#definition, #rProps, {{ }}, #rProps)
        ) [bind: #pairsCount, #elements, #dECObj, #utf8ECKs, #rProps]

    @invariant
        scope(pairsCount: #pairsCount) * scope(elements: #elements) * scope(encryptionContext: #dECObj) *
        scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
        scope(toUtf8: #toUtf8) * JSFunctionObject(#toUtf8, "toUtf8", #t_sc, #t_len, #t_proto) *
        scope(key: _) * scope(value: _) *
        toUtf8PairMap(#ECKs, #utf8ECKs) * FirstProj(#ECKs, #rProps) *
        CElements(#EC, 2., ((256. * #b0) + #b1), 2., #ECKs, (l-len #rest)) *
        UniqueOrDuplicated(#definition, #rProps, {{ }}, #rProps) *
        JSInternals() *

        scope(count: #count) * (#count <=# #pairsCount) *
        ArrayStructure(#elements, #pairsCount) *
        ArrayOfArraysOfUInt8ArraysContents(#elements, #done, 0, #count) *
        ArrayOfArraysOfUInt8ArraysContents(#elements, #left, #count, #pairsCount - #count) *
        (#ECKs == l+ (#done, #left)) *
        FirstProj(#done, #doneRProps) * Unique(#doneRProps) *
        FirstProj(#left, #leftRProps) * UniqueOrDuplicated(#definition, #leftRProps, #doneRProps, #leftRProps) *
        toUtf8PairMap(#done, #utf8Done) *
        JSObjWithProto(#dECObj, null) * ObjectTable(#dECObj, #utf8Done)
        [bind: #count, #done, #left, #doneRProps, #leftRProps, #utf8Done] */
  for (var count = 0; count < pairsCount; count++) {
    /*
        @tactic
            unfold ArrayStructure(#elements, #pairsCount);
            unfold UniqueOrDuplicated(#definition, #rProps, {{ }}, #rProps);
            unfold UniqueOrDuplicated(#definition, #leftRProps, #doneRProps, #leftRProps);
            unfold ArrayOfArraysOfUInt8ArraysContents(#elements, #left, #count, #pairsCount - #count) [bind: (#ECK := #elementContents) and (#rest_left := #rest)];
            apply CElementsElementLength(#EC, 2., ((256. * #b0) + #b1), 2., #ECKs, #done, #ECK, #rest_left);
            assert (#ECK == {{ #new_prop, #new_value }})
    */

    // FIXME: FIX THE PARSER!
    var [key, value] = elements[count].map(toUtf8)

    /* Postcondition: The number of keys in the encryptionContext must match the pairsCount.
     * If the same Key value is serialized...
     */
    /*
        @tactic
            assert (toUtf8(#new_prop, #utf8NProp)) [bind: #utf8NProp];
            assert (toUtf8(#new_value, #utf8NVal)) [bind: #utf8NVal];
            unfold ObjectTable(#dECObj, #utf8Done) [bind: (#doneProps := #pList) and (#donePropsSet := #pSet)];
            apply FirstProjConcatSplit(#ECKs, #done, #left);
            apply ProduceListToSet(#doneRProps); apply ProduceListToSet(#leftRProps);
            assert (ListToSet(#doneRProps, #doneRPropsSet)) [bind: #doneRPropsSet];
            unfold FirstProj(#left, #leftRProps);
            apply HeadInSet(#leftRProps);
            if (#definition = "Complete") then {
                apply UniqueConcatSplitNotInSuffix(#rProps, #doneRProps, #leftRProps, #new_prop);
                apply FirstProjToUtf8MapPairCompat(#done);
                apply NotInListToUtf8(#new_prop, #doneRProps);
                apply ObjectTableAbsentProperty(#dECObj, #utf8Done, #utf8NProp)
            } else {
                apply FirstProjToUtf8MapPairCompat(#done);
                unfold Duplicated(#doneRProps, #leftRProps) [bind: (#doneRPropsSet2 := #preSet)];
                apply ListToSetFunction(#doneRProps, #doneRPropsSet, #doneRProps, #doneRPropsSet2);
                if (#new_prop -e- #doneRPropsSet) then {
                    apply InListToUtf8(#new_prop, #doneRProps);
                    apply ObjectTablePresentProperty(#dECObj, #utf8Done, #utf8NProp)
                } else {
                    apply NotInListToUtf8(#new_prop, #doneRProps);
                    apply ObjectTableAbsentProperty(#dECObj, #utf8Done, #utf8NProp)
                }
            }
    */
    needs(
      encryptionContext[key] === undefined,
      'decodeEncryptionContext: Duplicate encryption context key value.'
    )
    encryptionContext[key] = value

    /*
        @tactic
            apply ArrayOfArraysOfUInt8ArraysContentsAppend(#elements, #done, 0, #count);
            apply ObjectTableStructureAppendPVPair(#dECObj, #utf8Done, #utf8NProp, #utf8NVal);
            apply toUtf8PairMapAppendPair(#done, #utf8Done, #new_prop, #new_value);
            apply FirstProjAppendPair(#done, #doneRProps, #new_prop, #new_value);
            apply FirstProjAppendPair(#utf8Done, #doneProps, #utf8NProp, #utf8NVal);
            apply ListToSetAddElement(#doneProps, #donePropsSet, #utf8NProp);
            apply UniqueAppendElement(#doneRProps, #new_prop);
            if (#definition = "Complete") then {
                unfold Unique(#leftRProps)
            }
    */
    0 === 0;
  }

  /*
    @tactic
        unfold ArrayOfArraysOfUInt8ArraysContents(#elements, #left, #count, #pairsCount - #count);
        apply toUtf8PairMapInjective(#ECKs, #utf8ECKs, #done, #utf8Done);
        if (#definition = "Broken") then {
            unfold FirstProj(#left, #leftRProps);
            unfold UniqueOrDuplicated(#definition, #leftRProps, #doneRProps, #leftRProps);
            unfold Duplicated(#doneRProps, #leftRProps)
        };
        use_subst [object_table : (#PVPairs: #utf8ECKs) ]
  */
  Object.freeze(encryptionContext)
  return encryptionContext
}

/************************************************
 ************************************************
 *******                                  *******
 *******   deserializeEncryptedDataKeys   *******
 *******                                  *******
 ************************************************
 ************************************************/

/**
    @id deserializeEncryptedDataKeys

    @pre (this == undefined) * (buffer == #buffer) * (startPos == #startPos) *
         Uint8Array(#buffer, #aBuffer, #byteOffset, #byteLength) * ArrayBuffer(#aBuffer, #data) *
         (#view == l-sub(#data, #byteOffset, #byteLength)) *
         ((#definition == "Complete") \/ (#definition == "Incomplete")) *
         RawEncryptedDataKeys(#definition, #view, #startPos, #EDKs, #EDKsLength, #errorMessage) *

         scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
         scope(toUtf8: #toUtf8) * JSFunctionObject(#toUtf8, "toUtf8", #t_sc, #t_len, #t_proto) *
         scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
         JSInternals()

    @post Uint8Array(#buffer, #aBuffer, #byteOffset, #byteLength) * ArrayBuffer(#aBuffer, #data) *
          (#view == l-sub(#data, #byteOffset, #byteLength)) *
          RawEncryptedDataKeys(#definition, #view, #startPos, #EDKs, #EDKsLength, #errorMessage) *

          (#definition == "Complete") *
          JSObject(ret) *
            DataProp(ret, "encryptedDataKeys", #dEDKs) *
                DeserialisedEncryptedDataKeys(#dEDKs, #EDKs) *
            DataProp(ret, "readPos", #startPos + #EDKsLength) *

          scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
          scope(toUtf8: #toUtf8) * JSFunctionObject(#toUtf8, "toUtf8", #t_sc, #t_len, #t_proto) *
          scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
          JSInternals ();

          Uint8Array(#buffer, #aBuffer, #byteOffset, #byteLength) * ArrayBuffer(#aBuffer, #data) *
          (#view == l-sub(#data, #byteOffset, #byteLength)) *
          RawEncryptedDataKeys(#definition, #view, #startPos, #EDKs, #EDKsLength, #errorMessage) *

          (#definition == "Incomplete") * (ret == false) *

          scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
          scope(toUtf8: #toUtf8) * JSFunctionObject(#toUtf8, "toUtf8", #t_sc, #t_len, #t_proto) *
          scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
          JSInternals ()

    @pre (this == undefined) * (buffer == #buffer) * (startPos == #startPos) *
         Uint8Array(#buffer, #aBuffer, #byteOffset, #byteLength) * ArrayBuffer(#aBuffer, #data) *
         (#view == l-sub(#data, #byteOffset, #byteLength)) *
         (#definition == "Broken") *
         RawEncryptedDataKeys(#definition, #view, #startPos, #EDKs, #EDKsLength, #errorMessage) *

         scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
         JSInternals()

    @posterr
          Uint8Array(#buffer, #aBuffer, #byteOffset, #byteLength) * ArrayBuffer(#aBuffer, #data) *
          RawEncryptedDataKeys(#definition, #view, #startPos, #EDKs, #EDKsLength, #errorMessage) *

          ErrorObjectWithMessage(ret, #errorMessage) *

          scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
          JSInternals ()
*/
function deserializeEncryptedDataKeys(buffer, startPos) {
  /* @tactic
      unfold RawEncryptedDataKeys(#definition, #view, #startPos, #EDKs, #EDKsLength, #errorMessage);
      if (#definition = "Complete") then {
        unfold CRawEncryptedDataKeys(#view, #startPos, #EDKs, #EDKsLength)
      } else {
          if (#definition = "Incomplete") then {
            unfold IRawEncryptedDataKeys(#view, #startPos)
          } else {
            unfold BRawEncryptedDataKeys(#errorMessage, #view, #startPos)
          }
      } */

  /* Precondition: startPos must be within the byte length of the buffer given. */
  needs(
    buffer.byteLength >= startPos && startPos >= 0,
    'deserializeMessageHeader: startPos out of bounds.'
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
  needs(encryptedDataKeysCount, 'Malformed Header: No EncryptedDataKey found.')

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
      /* Implement the array deconstructor */
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

var AlgorithmSuiteIdentifier;

/****************************
 ****************************
 *******              *******
 *******   SdkSuite   *******
 *******              *******
 ****************************
 ****************************/

/*
    @onlyspec SdkSuite (suiteId)
        [[
            JSObject(this) * ((this, "ivLength") -> none) * ((this, "tagLength") -> none) *
            CAlgorithmSuite(suiteId, #stringId, #ivLength, #tagLength)
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
    @id deserializeMessageHeader

    @pre (messageBuffer == #messageBuffer) *
         Uint8Array(#messageBuffer, #buffer, #byteOffset, #byteLength) * ArrayBuffer(#buffer, #data) *
         (#view == l-sub(#data, #byteOffset, #byteLength)) *
         (#byteOffset + #byteLength <=# l-len #data) *
         ((#definition == "Complete") \/ (#definition == "Incomplete")) *
         Header(#definition,
                #view, #part_one, #version, #type, #suiteId, #messageId, #ECLength,
                       #part_two, #ECKs,
                       #part_three, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #headerIv, #headerAuthTag,
                #errorMessage) *

         scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
         scope(AlgorithmSuiteIdentifier : #ASIObject) * AlgorithmSuiteIdentifierObject(#ASIObject) *
         scope(decodeEncryptionContext : #DECObject) * JSFunctionObject(#DECObject, "decodeEncryptionContext", #dc_sc, #dc_len, #dc_proto) *
         scope(deserializeEncryptedDataKeys : #DEDKObject) * JSFunctionObject(#DEDKObject, "deserializeEncryptedDataKeys", #ds_sc, #ds_len, #ds_proto) *
         scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
         scope(SdkSuite : #SdkObject) * JSFunctionObject(#SdkObject, "SdkSuite", #s_sc, #s_len, $lobj_proto) *
         scope(toUtf8: #toUtf8) * JSFunctionObject(#toUtf8, "toUtf8", #t_sc, #t_len, #t_proto) *
         JSInternals ()

    @post Uint8Array(#messageBuffer, #buffer, #byteOffset, #byteLength) * ArrayBuffer(#buffer, #data) *
          Header(#definition,
                 #view, #part_one, #version, #type, #suiteId, #messageId, #ECLength,
                        #part_two, #ECKs,
                        #part_three, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #headerIv, #headerAuthTag,
                 #errorMessage) *

          (#definition == "Complete") *
          HeaderInfo(ret, #version, #type, #suiteId, #messageId, #ECKs, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #rawHeaderData, #headerIv, #headerAuthTag) *
          (#rawHeaderData == l-sub(#view, 0, #headerLength)) *

          scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
          scope(AlgorithmSuiteIdentifier : #ASIObject) * AlgorithmSuiteIdentifierObject(#ASIObject) *
          scope(decodeEncryptionContext : #DECObject) * JSFunctionObject(#DECObject, "decodeEncryptionContext", #dc_sc, #dc_len, #dc_proto) *
          scope(deserializeEncryptedDataKeys : #DEDKObject) * JSFunctionObject(#DEDKObject, "deserializeEncryptedDataKeys", #ds_sc, #ds_len, #ds_proto) *
          scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
          scope(SdkSuite : #SdkObject) * JSFunctionObject(#SdkObject, "SdkSuite", #s_sc, #s_len, $lobj_proto) *
          scope(toUtf8: #toUtf8) * JSFunctionObject(#toUtf8, "toUtf8", #t_sc, #t_len, #t_proto) *
          JSInternals();

          Uint8Array(#messageBuffer, #buffer, #byteOffset, #byteLength) * ArrayBuffer(#buffer, #data) *
          Header(#definition,
                 #view, #part_one, #version, #type, #suiteId, #messageId, #ECLength,
                        #part_two, #ECKs,
                        #part_three, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #headerIv, #headerAuthTag,
                 #errorMessage) *

          (definition == "Incomplete") * (ret == false) *

          scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
          scope(AlgorithmSuiteIdentifier : #ASIObject) * AlgorithmSuiteIdentifierObject(#ASIObject) *
          scope(decodeEncryptionContext : #DECObject) * JSFunctionObject(#DECObject, "decodeEncryptionContext", #dc_sc, #dc_len, #dc_proto) *
          scope(deserializeEncryptedDataKeys : #DEDKObject) * JSFunctionObject(#DEDKObject, "deserializeEncryptedDataKeys", #ds_sc, #ds_len, #ds_proto) *
          scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
          scope(SdkSuite : #SdkObject) * JSFunctionObject(#SdkObject, "SdkSuite", #s_sc, #s_len, $lobj_proto) *
          JSInternals()

    @pre (messageBuffer == #messageBuffer) *
         Uint8Array(#messageBuffer, #buffer, #byteOffset, #byteLength) * ArrayBuffer(#buffer, #data) *
         (#view == l-sub(#data, #byteOffset, #byteLength)) *
         (#byteOffset + #byteLength <=# l-len #data) *
         (#definition == "Broken") *
         Header(#definition,
                #view, #part_one, #version, #type, #suiteId, #messageId, #ECLength,
                       #part_two, #ECKs,
                       #part_three, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #headerIv, #headerAuthTag,
                #errorMessage) *

         scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
         scope(AlgorithmSuiteIdentifier : #ASIObject) * AlgorithmSuiteIdentifierObject(#ASIObject) *
         scope(decodeEncryptionContext : #DECObject) * JSFunctionObject(#DECObject, "decodeEncryptionContext", #dc_sc, #dc_len, #dc_proto) *
         scope(deserializeEncryptedDataKeys : #DEDKObject) * JSFunctionObject(#DEDKObject, "deserializeEncryptedDataKeys", #ds_sc, #ds_len, #ds_proto) *
         scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
         scope(SdkSuite : #SdkObject) * JSFunctionObject(#SdkObject, "SdkSuite", #s_sc, #s_len, $lobj_proto) *
         scope(toUtf8: #toUtf8) * JSFunctionObject(#toUtf8, "toUtf8", #t_sc, #t_len, #t_proto) *
         JSInternals ()

    @posterr
          Uint8Array(#messageBuffer, #buffer, #byteOffset, #byteLength) * ArrayBuffer(#buffer, #data) *
          Header(#definition,
                 #view, #part_one, #version, #type, #suiteId, #messageId, #ECLength,
                        #part_two, #ECKs,
                        #part_three, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #headerIv, #headerAuthTag,
                 #errorMessage) *

          ErrorObjectWithMessage(ret, #errorMessage) *

          scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
          scope(AlgorithmSuiteIdentifier : #ASIObject) * AlgorithmSuiteIdentifierObject(#ASIObject) *
          scope(decodeEncryptionContext : #DECObject) * JSFunctionObject(#DECObject, "decodeEncryptionContext", #dc_sc, #dc_len, #dc_proto) *
          scope(deserializeEncryptedDataKeys : #DEDKObject) * JSFunctionObject(#DEDKObject, "deserializeEncryptedDataKeys", #ds_sc, #ds_len, #ds_proto) *
          scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
          scope(SdkSuite : #SdkObject) * JSFunctionObject(#SdkObject, "SdkSuite", #s_sc, #s_len, $lobj_proto) *
          scope(toUtf8: #toUtf8) * JSFunctionObject(#toUtf8, "toUtf8", #t_sc, #t_len, #t_proto) *
          JSInternals()
*/
function deserializeMessageHeader(messageBuffer) {
  /* Uint8Array is a view on top of the underlying ArrayBuffer.
  * This means that raw underlying memory stored in the ArrayBuffer
  * may be larger than the Uint8Array.  This is especially true of
  * the Node.js Buffer object.  The offset and length *must* be
  * passed to the DataView otherwise I will get unexpected results.
  */
  var dataView = new DataView(
    messageBuffer.buffer,
    messageBuffer.byteOffset,
    messageBuffer.byteLength
  )

  /* @tactic unfold Header(#definition, #view, #part_one, #version, #type, #suiteId, #messageId, #ECLength, #part_two, #ECKs, #part_three, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #headerIv, #headerAuthTag, #errorMessage) */
  /* @tactic
      if (#definition = "Complete") then {
        unfold CHeader(#view, #part_one, #version, #type, #suiteId, #messageId, #ECLength, #part_two, #ECKs, #part_three, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #headerIv, #headerAuthTag)
          [bind: (#stringId := #stringId) and (#tagLength := #tagLength)]
      } else { if (#definition = "Incomplete") then {
          unfold IHeader(#view, #part_one, #version, #type, #suiteId, #messageId, #ECLength, #part_two, #ECKs, #part_three, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #headerIv, #headerAuthTag)
        } else {
          unfold BHeader(#view, #part_one, #version, #type, #suiteId, #messageId, #ECLength, #part_two, #ECKs, #part_three, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #headerIv, #headerAuthTag, #errorMessage)
        }
      } */

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
      : 'Malformed Header: Unsupported version and/or type.'
  )

  var suiteId = dataView.getUint16(2, false)  // big endian
  /* Precondition: suiteId must match supported algorithm suite */
  /*
    @tactic
        if (#definition = "Complete") then {
            unfold CAlgorithmSuite(#suiteId, #stringId, #headerIvLength, #tagLength)
        }
  */
  needs(AlgorithmSuiteIdentifier[suiteId], 'Malformed Header: Unsupported algorithm suite.')
  /*
    @tactic
        if (#definition = "Complete") then {
            fold CAlgorithmSuite(#suiteId, #stringId, #headerIvLength, #tagLength)
        }
  */
  var messageId = messageBuffer.slice(4, 20)
  var contextLength = dataView.getUint16(20, false) // big endian

  /* Check for early return (Postcondition): Not Enough Data. Need to have all of the context in bytes before we can parse the next section.
  * This is the first variable length section.
  */
  if (22 + contextLength > dataView.byteLength) return false // not enough data

  var encryptionContext = decodeEncryptionContext(
    messageBuffer.slice(22, 22 + contextLength)
  )

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
  needs(reservedBytes === 0, 'Malformed Header: Reserved bytes not equal to zero.')
  var headerIvLength = dataView.getUint8(readPos + 1 + 4)
  /* Postcondition: The headerIvLength must match the algorithm suite specification. */
  needs(headerIvLength === ivLength, 'Malformed Header: Mismatch between expected and obtained IV length.')
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