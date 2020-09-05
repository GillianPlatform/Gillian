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

        (0 <=# #elementCount) * (0 <=# #fieldsPerElement) * (0 <=# #readPos) *

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
      @tactic
        apply lemma ElementsPureFacts(#view, #readPos, #elementCount, #fieldsPerElement, #elementList, #elementsLength)

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
      /* @tactic
          apply lemma ElementsPureFacts(#view, #readPos, #elementCount - #elementsLeft, #fieldsPerElement, #doneElsList, #doneElsLength);
          assert Element(#view, #outerLoopReadPos, #fieldsPerElement, #fieldsList, #elementLength) [bind: #fieldsList, #elementLength];
          apply lemma ElementPureFacts(#view, #outerLoopReadPos, #fieldsPerElement, #fieldsList, #elementLength) */

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
        /* @tactic
            apply lemma ElementPureFacts(#view, #outerLoopReadPos, #fieldsPerElement - #fieldsLeft, #doneElList, #doneElLength);
            apply lemma ElementPureFacts(#view, #innerLoopReadPos, #fieldsLeft, #remainingElList, #remainingElLength) */
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
