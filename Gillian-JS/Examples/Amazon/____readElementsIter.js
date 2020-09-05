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
    );

    /* Well-formedness: readPos must be within the byte length of the buffer given. */
    needs(readPos >= 0 && dataView.byteLength >= readPos, 'readPos out of bounds.')

    /* Well-formedness: elementCount must not be negative. */
    needs(elementCount >= 0, 'elementCount must be positive.')

    var elements = [];

    /*
      @tactic apply lemma ElementsPureFacts(#view, #readPos, #elementCount, #fieldsPerElement, #elementList, #elementsLength)
    */
    /*
      @invariant
        scope(elements : #readElements) * scope(readPos : #currentReadPos) * scope(elementCount : #currentElementCount) *
        Elements(#view, #currentReadPos, #currentElementCount, #fieldsPerElement, #remainingElementsList, #remainingElementsLength) *
        Elements(#view, #readPos, #elementCount - #currentElementCount, #fieldsPerElement, #doneElementsList, #doneElementsLength) *
        (#elementList == l+ (#doneElementsList, #remainingElementsList)) *
        (#elementsLength == #doneElementsLength + #remainingElementsLength) *
        (#readPos + #doneElementsLength == #currentReadPos) *
        ArrayOfArraysOfUInt8Arrays(#readElements, #doneElementsList, #elementCount - #currentElementCount)
        [bind : #readElements, #currentReadPos, #currentElementCount, #remainingElementsList, #remainingElementsLength, #doneElementsList, #doneElementsLength]
    */
    while (elementCount--) {

      var element = []
      var fieldCount = fieldsPerElement

      /* @invariant True */
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
      /* @tactic
          assert Element(#view, (#readPos + #doneElementsLength), #fieldsPerElement, #readElementFieldList, #readElementLength) [bind: #readElementFieldList, #readElementLength];
          apply lemma ElementsAppend(#view, #readPos, (#elementCount - #currentElementCount), #fieldsPerElement, #doneElementsList, #doneElementsLength, #readElementFieldList, #readElementLength) */
      elements.push(element);
    }

    return { elements, readPos }

    /*
        @id aux_readElement

        @pre (element == #element) * (fieldCount == #fieldCount) *
             scope(buffer : #buffer) *
             scope(dataView : #dataView) *
             scope(readPos : #readPos) *
             scope(aux_readElement : #aux_readElement) *
             (#are_sc == $$scope) *

             DataView(#dataView, #ab, #viewOffset, #viewSize) *
             Uint8Array (#buffer, #ab, #viewOffset, #viewSize) *
             ArrayBuffer(#ab, #data) *
             (#view == l-sub(#data, #viewOffset, #viewSize)) *
             Element(#view, #readPos, #fieldCount, #fieldList, #fieldsLength) *
             ArrayOfUInt8Arrays(#element, #oldFields, #oldFieldCount) *

             JSFunctionObject(#aux_readElement, "aux_readElement", #are_sc, #are_len,  #are_proto) *
             DataViewPrototype($ldv_proto) * Uint8ArrayPrototype($lui8ar_proto) * ArrayPrototype ($larr_proto)

        @post scope(buffer : #buffer) *
              scope(dataView : #dataView) *
              scope(readPos : #ret_readPos) *
              scope(aux_readElement : #aux_readElement) *

              (#ret_readPos == #readPos + #fieldsLength) *
              DataView(#dataView, #ab, #viewOffset, #viewSize) *
              Uint8Array (#buffer, #ab, #viewOffset, #viewSize) *
              ArrayBuffer(#ab, #data) *
              (#view == l-sub(#data, #viewOffset, #viewSize)) *
              Element(#view, #readPos, #fieldCount, #fieldList, #fieldsLength) *
              ArrayOfUInt8Arrays(#element, l+ (#oldFields, #fieldList), #oldFieldCount + #fieldCount) *

              JSFunctionObject(#aux_readElement, "aux_readElement", #are_sc, #are_len, #are_proto) *
              DataViewPrototype($ldv_proto) * Uint8ArrayPrototype($lui8ar_proto) * ArrayPrototype ($larr_proto) *
              (ret == #element)
    */
   /*
    function aux_readElement (element, fieldCount) {

        if (fieldCount--) {

            if (readPos + 2 > dataView.byteLength) return false
            var length = dataView.getUint16(readPos, false) // big endian
            readPos += 2
            /* Check for early return (Postcondition): Enough data must exist length of the value.
            if (readPos + length > dataView.byteLength) return false
            var fieldBinary = buffer.slice(readPos, readPos + length)
            element.push(fieldBinary)
            readPos += length

            return aux_readElement (element, fieldCount);
        }

        return element
    } */
}
