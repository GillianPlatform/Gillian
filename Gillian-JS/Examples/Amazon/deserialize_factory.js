'use strict';

/*****************************
 *****************************
 *******               *******
 *******   Internals   *******
 *******               *******
 *****************************
 *****************************/

/*
    @pred JSInternals () :
        GlobalObject () *
        ObjectPrototype($lobj_proto) *
        ArrayPrototype ($larr_proto) *
        ArrayBufferPrototype ($lab_proto) *
        BI_DataViewObject () *
        DataViewPrototype($ldv_proto) *
        Uint8ArrayPrototype($lui8ar_proto);
*/

/*************************
 *************************
 *******           *******
 *******   needs   *******
 *******           *******
 *************************
 *************************/

/**
 * TODO: default parameters
 *   - had third function parameter: [Err = Error]
 *   - new Err(...) supposed to be thrown instead of new Error(...)
 */
/*
  @id needs
*/
function needs(condition, errorMessage) {
  if (!condition) {
    throw new Error(errorMessage)
  }
}

/*
    @pred nounfold Element(+view:List, +readPos:Num, +fieldCount: Num, fieldList:List, elementLength:Num) :
        (fieldCount == 0) * (fieldList == {{ }}) * (elementLength == 0),

        (0 <# fieldCount) *
        (#rawFL == l-sub (view, readPos, 2)) *
        rawToUInt16(#rawFL, false, #fieldLength) *
        (#field == l-sub(view, readPos + 2, #fieldLength)) *
        (#restFieldCount == fieldCount - 1) *
        Element(view, readPos + 2 + #fieldLength, #restFieldCount, #restFields, #restElementLength) *
        (0 <=# #restElementLength) *
        (fieldList == #field :: #restFields) *
        (elementLength == 2 + #fieldLength + #restElementLength);

    @pred nounfold Elements(+view:List, +readPos:Num, +elementCount: Num, +fieldCount: Num, elementList:List, elementsLength:Num) :
        (elementCount == 0) * (elementList == {{ }}) * (elementsLength == 0),

        (0 <# elementCount) *
        Element(view, readPos, fieldCount, #element, #elementLength) *
        (#restElementCount == elementCount - 1) *
        Elements(view, readPos + #elementLength, #restElementCount, fieldCount, #restElements, #restElementsLength) *
        (0 <=# #restElementsLength) *
        (elementList == #element :: #restElements) *
        (elementsLength == #elementLength + #restElementsLength);
*/

/**
 * TODO: default parameters
 *   - fourth function parameter: [readPos = 0]
 * TODO: const and let
 *   - let fieldCount
 *   - const <all others>
 */
/*
/*
    @id readElements

    @pre
        (elementCount == #elementCount) * (fieldsPerElement == #fieldsPerElement) * (buffer == #buffer) * (readPos == #readPos) *

        (0 <=# elementCount) * (0 <=# #fieldsPerElement) * (0 <=# #readPos) *

        Uint8Array (#buffer, #ab, #viewOffset, #viewSize) *
        ArrayBuffer(#ab, #data) *
        (#view == l-sub(#data, #viewOffset, #viewSize)) *
        Elements(#view, #readPos, #elementCount, #fieldsPerElement, #elementList, #elementsLength) *

        (0 <=# #elementsLength) *
        (#readPos + #elementsLength <=# #viewSize) *

        scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
        JSInternals ()

    @post
        Uint8Array (#buffer, #ab, #viewOffset, #viewSize) *
        ArrayBuffer(#ab, #data) *
        Elements(#view, #readPos, #elementCount, #fieldsPerElement, #elementList, #elementsLength) *scope(needs : #needs) *
        JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
        JSInternals () *

        JSObject(ret) *
        DataProp(ret, "elements", #elements) *
            ArrayOfArraysOfUInt8Arrays(#elements, #elementList, #elementCount) *
        DataProp(ret, "readPos", #ret_readPos) *
            (#ret_readPos == #readPos + #elementsLength)
*/
function readElements(elementCount, fieldsPerElement, buffer, readPos) {
  var dataView = new DataView(
    buffer.buffer,
    buffer.byteOffset,
    buffer.byteLength
  )
  var elements = []

  /* Precondition: readPos must be non-negative and within the byte length of the buffer given. */
  needs(
    readPos >= 0 && dataView.byteLength >= readPos,
    'readPos out of bounds.'
  )

  /* Precondition: elementCount and fieldsPerElement must be non-negative. */
  needs(
    elementCount >= 0 && fieldsPerElement >= 0,
    'elementCount and fieldsPerElement must be positive.'
  )

  while (elementCount--) {
    var element = []
    var fieldCount = fieldsPerElement
    while (fieldCount--) {
      /* Check for early return (Postcondition): Enough data must exist to read the Uint16 length value. */
      if (readPos + 2 > dataView.byteLength) return false
      var length = dataView.getUint16(readPos, false) // big endian
      readPos += 2
      /* Check for early return (Postcondition): Enough data must exist length of the value. */
      if (readPos + length > dataView.byteLength) return false
      var fieldBinary = buffer.slice(readPos, readPos + length)
      element.push(fieldBinary)
      readPos += length
    }
    elements.push(element)
  }
  return { elements, readPos }
}