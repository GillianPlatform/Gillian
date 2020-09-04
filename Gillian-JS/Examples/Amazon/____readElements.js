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
    @pred Element(+view:List, +readPos:Num, +fieldCount: Num, fieldList:List, elementLength:Num) :
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

    @pred Elements(+view:List, +readPos:Num, +elementCount: Num, +fieldCount: Num, elementList:List, elementsLength:Num) :
        (elementCount == 0) * (elementList == {{ }}) * (elementsLength == 0),

        (0 <# elementCount) *
        Element(view, readPos, fieldCount, #element, #elementLength) *
        (#restElementCount == elementCount - 1) *
        Elements(view, readPos + #elementLength, #restElementCount, fieldCount, #restElements, #restElementsLength) *
        (0 <=# #restElementsLength) *
        (elementList == #element :: #restElements) *
        (elementsLength == #elementLength + #restElementsLength);
*/

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

    /* Well-formedness: readElements readPos must be within the byte length of the buffer given. */
    needs(readPos >= 0 && dataView.byteLength >= readPos, 'readPos out of bounds.')

    /* Well-formedness: elementCount must not be negative. */
    needs(elementCount >= 0, 'elementCount must be positive.')

    var elements = aux_readElements([], elementCount);
    return (elements ? { elements, readPos } : false)

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
             (#readPos + #fieldsLength <=# #viewSize) *
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

              JSFunctionObject(#aux_readElement, "aux_readElement", #are_sc, #are_len,  #are_proto) *
              DataViewPrototype($ldv_proto) * Uint8ArrayPrototype($lui8ar_proto) * ArrayPrototype ($larr_proto) *
              (ret == #element)
    */
    function aux_readElement (element, fieldCount) {

        if (fieldCount--) {

            if (readPos + 2 > dataView.byteLength) return false
            var length = dataView.getUint16(readPos, false) // big endian
            readPos += 2
            /* Check for early return (Postcondition): Enough data must exist length of the value. */
            if (readPos + length > dataView.byteLength) return false
            var fieldBinary = buffer.slice(readPos, readPos + length)
            element.push(fieldBinary)
            readPos += length

            return aux_readElement (element, fieldCount);
        }

        return element
    }

    /*
        @id aux_readElements

        @pre (elements == #elements) * (elementCount == #elementCount) *
             scope(buffer : #buffer) *
             scope(fieldsPerElement : #fieldsPerElement) *
             scope(dataView         : #dataView) *
             scope(readPos          : #readPos) *
             scope(aux_readElement  : #aux_readElement) *
             scope(aux_readElements : #aux_readElements) *
             o_chains(aux_readElements : $$scope, aux_readElement : #are_sc) *

             DataView(#dataView, #ab, #viewOffset, #viewSize) *
             Uint8Array (#buffer, #ab, #viewOffset, #viewSize) *
             ArrayBuffer(#ab, #data) *
             (#view == l-sub(#data, #viewOffset, #viewSize)) *
             (#viewOffset + #viewSize <=# l-len #data) *
             (#readPos + #elementsLength <=# #viewSize) *
             Elements(#view, #readPos, #elementCount, #fieldsPerElement, #elementList, #elementsLength) *
             ArrayOfArraysOfUInt8Arrays(#elements, #oldElements, #oldElementCount) *

             JSFunctionObject(#aux_readElement,  "aux_readElement",  #are_sc,  #are_len,  #are_proto) *
             JSFunctionObject(#aux_readElements, "aux_readElements", $$scope, #ares_len, #ares_proto) *
             DataViewPrototype($ldv_proto) * Uint8ArrayPrototype($lui8ar_proto) * ArrayPrototype ($larr_proto)

        @post scope(buffer : #buffer) *
              scope(fieldsPerElement : #fieldsPerElement) *
              scope(dataView         : #dataView) *
              scope(readPos          : #ret_readPos) *
              scope(aux_readElement  : #aux_readElement) *
              scope(aux_readElements : #aux_readElements) *
              o_chains(aux_readElements : #ares_sc, aux_readElement : #are_sc) *

              (#ret_readPos == #readPos + #elementsLength) *
              DataView(#dataView, #ab, #viewOffset, #viewSize) *
              Uint8Array (#buffer, #ab, #viewOffset, #viewSize) *
              ArrayBuffer(#ab, #data) *
              (#view == l-sub(#data, #viewOffset, #viewSize)) *
              Elements(#view, #readPos, #elementCount, #fieldsPerElement, #elementList, #elementsLength) *
              ArrayOfArraysOfUInt8Arrays(#elements, l+ (#oldElements, #elementList), #oldElementCount + #elementCount) *

              JSFunctionObject(#aux_readElement,  "aux_readElement",  #are_sc,  #are_len,  #are_proto) *
              JSFunctionObject(#aux_readElements, "aux_readElements", #ares_sc, #ares_len, #ares_proto) *
              DataViewPrototype($ldv_proto) * Uint8ArrayPrototype($lui8ar_proto) * ArrayPrototype ($larr_proto) *
              (ret == #elements)
    */
   function aux_readElements (elements, elementCount) {

        if (elementCount--) {
            var element = aux_readElement([], fieldsPerElement);

            /* Post-condition: Enough elements must exist. */
            if (!element) return false;

            elements.push(element);
            return aux_readElements (elements, elementCount);
        }

        return elements
    }
}
