# Verfication of header deserialization in the AWS-Encryption-SDK for JavaScript

## Running the verification

To make the task simple, we created a `Makefile` that lets you run everything. In the current folder, running:

- `$ make js` will verify the correctness of all of the functions that were specified in the JS aws-encryption-sdk. The three most important ones are: `decodeEncryptionContext`, `deserializeEncryptedDataKeys`, and the main deserialisation function, `deserializeMessageHeader`. This verification takes approximately 45 seconds on a machine with an Intel Core i7-4980HQ CPU 2.80 GHz, DDR3 RAM 16GB, and a 256GB solid-state hard-drive, running macOS.
- `$ make pp-bug` will trigger the prototype poisoning bug (details [here](#js-bug-pp)).
- `$ make frozen-bug` will trigger the bug in which the encryption context is returned non-frozen (details [here](#js-bug-nf)).

The bug rules will be a bit slower to execute as they will be producing the appropriate logs.

## Structure of this folder

This folder contains the files related to the verification of the deserialisation module of the JS implementation of the AWS Encryption SDK, and they are:
- the main files:
  - `deserialize_factory.js`: the **main** main file, containing all of the functions of the deserialisation module and their specifications
  - `AmazonLogic.jsil`: language-dependent predicates and lemmas describing the deserialised AWS header
  - `ByteLogic.jsil`: basic conversion from lists of bytes to various numerics
  - `EncryptionHeaderLogic.jsil`: language-independent predicates and lemmas describing the serialised AWS Message Header
  - `ListLogic.jsil`: predicates and lemmas for advanced list management
  - `Utf8Logic.jsil`: axiomatisation of conversion from bytes to UTF-8 strings
- files inside `bugs/pp`: the main files adapted to reproduce the prototype poisoning bug
- files inside `bugs/frozen`: the main files adapted to reproduce the non-frozen encryption context bug


## Reading Gillian-JS annotations: Predicates

Gillian-JS also comes with many built-in predicates for describing standard JavaScript objects and their properties (e.g, `JSObject`, `DataProp`), as well as other built-in objects (e.g., `JSFunctionObject`, `ArrayBuffer`, `Uint8Array`, etc.), scoping (`scope`), and many more. The user-defined predicates are declared as in C; for example, the following predicate describes what it means for an object at location `l`to contain a list of property-value pairs described by the list `PVPairs`: 

```gil
pred ObjectTableStructure(+l:Obj, +PVPairs:List) :
    (* Base case - no properties left *)
    (PVPairs == {{ }}),

    (* Recursive case - one property and the rest *)
    (PVPairs == {{ #prop, #value }} :: #restPVPairs) *
    DataProp(l, #prop, #value) * types(#value : Str) *
    ObjectTableStructure(l, #restPVPairs);
```

### Reading Gillian-JS annotations: Specifications

Specifications and lemmas are written slightly differently in JavaScript than in C, as illustrated by the following example of the specification of the `decodeEncryptionContext` function:

```javascript
/**
    @pre (this == undefined) * (encodedEncryptionContext == #eEC) *
         Uint8Array(#eEC, #aBuffer, #byteOffset, #byteLength) * 
         ArrayBuffer(#aBuffer, #data) *
         (#EC == l-sub(#data, #byteOffset, #byteLength)) *
         (#definition == "Complete") *
         RawEncryptionContext(#definition, #EC, #ECKs, #errorMessage) *

         scope(needs : #needs) * 
         JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
         scope(readElements : #readElements) * 
         JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
         scope(toUtf8: #toUtf8) * 
         JSFunctionObject(#toUtf8, "toUtf8", #t_sc, #t_len, #t_proto) *
         JSInternals()

    @post Uint8Array(#eEC, #aBuffer, #byteOffset, #byteLength) * 
    			ArrayBuffer(#aBuffer, #data) *
          RawEncryptionContext(#definition, #EC, #ECKs, #errorMessage) *

          DecodedEncryptionContext(ret, #ECKs) *

          scope(needs : #needs) * 
          JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
          scope(readElements : #readElements) * 
          JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
          scope(toUtf8: #toUtf8) * 
          JSFunctionObject(#toUtf8, "toUtf8", #t_sc, #t_len, #t_proto) *
          JSInternals ()

    @pre (this == undefined) * (encodedEncryptionContext == #eEC) *
         Uint8Array(#eEC, #aBuffer, #byteOffset, #byteLength) * 
         ArrayBuffer(#aBuffer, #data) *
         (#EC == l-sub(#data, #byteOffset, #byteLength)) *
         (#definition == "Broken") *
         RawEncryptionContext(#definition, #EC, #ECKs, #errorMessage) *

         scope(needs : #needs) * 
         JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
         scope(readElements : #readElements) * 
         JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
         scope(toUtf8: #toUtf8) * 
         JSFunctionObject(#toUtf8, "toUtf8", #t_sc, #t_len, #t_proto) *
         JSInternals()

    @posterr
          Uint8Array(#eEC, #aBuffer, #byteOffset, #byteLength) * 
          ArrayBuffer(#aBuffer, #data) *
          RawEncryptionContext(#definition, #EC, #ECKs, #errorMessage) *

          ErrorObjectWithMessage(ret, #errorMessage) *

          scope(needs : #needs) * 
          JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
          scope(readElements : #readElements) * 
          JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
          scope(toUtf8: #toUtf8) * 
          JSFunctionObject(#toUtf8, "toUtf8", #t_sc, #t_len, #t_proto) *
          JSInternals ()
*/
function decodeEncryptionContext(encodedEncryptionContext) {
  ...
}
```

Specifications are  written in comments, using `@`-annotations. Pre-conditions are denoted by `@pre`; post-conditions are denoted by `@post` when the function is intended to terminate normally, or `@posterr`, when the function is intended to terminate with an error. The actual assertions are written as in C, with building blocks separated by the separating conjunction, `*`. Note that the part capturing the functions using the built-in `scope` and  `JSFunctionObject` predicates could potentially be elided and generated automatically, together with the `JSInternals ()` predicate that captures the built-in objects, such as `Array`,  `ArrayBuffer`,  `Object`,  etc., together with their prototypes.

Axiomatic specifications are written even more differently, as illustrated by the following axiomatic specification of the `toUtf8` function:

```javascript
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
```

where, after the function identifier, the keyword `@onlyspec` indicates an axiomatic specification, followed by a pre- and post-condition delimited by ``[[ ... ]]`` and the mode in which the function terminates (normal or error).

## Understanding the discovered bugs

We found two bugs in the AWS implementation, and we have already explained earlier how to reproduce those bugs. In this section, for each bug, we explain how it can happen and provide a fix. 

### Prototype poisoning in decodeEncryptionContext <a name="js-bug-pp"></a>

The `decodeEncryptionContext` deserialises the encryption context into a key-value map, initially implemented as a standard JavaScript object, encoding keys as property names and values as property values:

```javascript
var encryptionContext = {};
```

However, given the prototype inheritance mechanism of JavaScript, object property lookup may succeed if the property in question exists further along the prototype chain (say, `"hasOwnProperty"`in `Object.prototype`). This, combined with the runtime correctness check:

```javascript
needs(
	encryptionContext[key] === undefined,
	'decodeEncryptionContext: Duplicate encryption context key value.'
)
```

where the `needs` function is defined as follows

```javascript
function needs(condition, errorMessage) {
  if (!condition) {
    throw new Error(errorMessage)
  }
}
```

results in the function throwing an error if the key coincides with a property of `Object.prototype`. 

In the created log file, reachable by typing `open file.log`, line 2781259 says:

```
VERIFICATION FAILURE: Spec decodeEncryptionContext 0 terminated with flag error instead of normal
```

which can be traced back to the above correctness check using the`needs` function throwing an error. The simplest way to fix this is to create the encryption context object with the prototype `null`:

```javascript
var encryptionContext = Object.create(null)
```

Pull request: https://github.com/aws/aws-encryption-sdk-javascript/pull/216

### Authenticated encryption context can be edited by third parties  <a name="js-bug-nf"></a>

Again in the `decodeEncryptionContext` function, in the case when the encryption context is empty, the returned object is returned non-frozen:

```javascript
var encryptionContext = Object.create(null)

if (!encodedEncryptionContext.byteLength) {
  /* ERROR: OBJECT NOT FROZEN */
  return encryptionContext
}
```

In the created log file, which is this time at the `verbose` level and is reachable by typing `open file.log`, line 6757449 says:

```
VERIFICATION FAILURE: Spec decodeEncryptionContext 0 - post condition not unifiable
```

which can then be traced back to line the line which says:

```
WARNING: Unify Assertion Failed: (<Cell>(_lvar_3039, "@extensible"; false), ) with subst 
```

revealing that the object is not non-extensible as intended, which is then understood to come from the object not being frozen.

This allows third parties to edit the deserialised encryption context, which constitutes a security breach. As we are not security experts, we cannot estimate the severity of this breach. The fix is to freeze the object before the return statement using

```javascript
Object.freeze(encryptionContext);
```

This bug was communicated to the developers personally and we are expecting it to be fixed soon.

#### BONUS: Improved implementation of the readElements function

In a nutshell, the implementation of the readElements function was not aligned with the underlying data structure, making its specification and verification very difficult. We suggested an appropriate change; more details are available in the pull request below.

Pull request: [https://github.com/aws/aws-encryption-sdk-javascript/pull/215](https://github.com/aws/aws-encryption-sdk-javascript/pull/215)