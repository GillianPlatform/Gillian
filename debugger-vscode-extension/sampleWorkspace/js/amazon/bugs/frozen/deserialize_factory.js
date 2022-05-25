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
            unfold CElements(#view, #outerLoopReadPos, #eLeft, #fCount, #remElsList, #remElsLength) [bind: (#element := #fList) and (#eLength := #eLength)]
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
    /* ERROR: OBJECT NOT FROZEN */

    /* @tactic
        assert (
          scope(encryptionContext: #encryptionContext) *
          JSObjGeneral(#encryptionContext, null, "Object", false) *
          toUtf8PairMap(#ECKs, #utf8ECKs) *
          FrozenObjectTable(#encryptionContext, #utf8ECKs)
        ) [bind: #encryptionContext, #utf8ECKs]
    */
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
            unfold ArrayOfArraysOfUInt8ArraysContents(#elements, #left, #count, #pairsCount - #count) [bind: (#elementContents := #ECK) and (#rest := #rest_left)];
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
            unfold ObjectTable(#dECObj, #utf8Done) [bind: (#pList := #doneProps) and (#pSet := #donePropsSet)];
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
                unfold Duplicated(#doneRProps, #leftRProps) [bind: (#preSet := #doneRPropsSet2)];
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

  /**
   * dECOb == encryptionContext *
   * toUtf8PairMap(ECKs, #utf8ECKs) *
    FrozenObjectTable(dECObj, #utf8ECKs); */
  return encryptionContext
}