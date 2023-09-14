type t =
  | FreeformString of string
  | EmptyString
  | Let
  | LetBinding
  | Nil
  | Type
  | Bool
  | CBool
  | ProperBool
  | Signedbv
  | Unsignedbv
  | VerilogSignedbv
  | VerilogUnsignedbv
  | Floatbv
  | Fixedbv
  | X86Extended
  | CSourceLocation
  | CEndLocation
  | CIsPadding
  | CDoNotDump
  | File
  | Line
  | Column
  | Comment
  | Property
  | PropertyClass
  | PropertyId
  | Function
  | MathematicalFunction
  | Code
  | Typecast
  | StaticCast
  | DynamicCast
  | ConstCast
  | ReinterpretCast
  | Index
  | Ptrmember
  | Member
  | MemberName
  | CMemberName
  | Equal
  | Implies
  | And
  | Nand
  | Or
  | Nor
  | Xor
  | Not
  | Bitand
  | Bitor
  | Bitnot
  | Bitxor
  | Bitnand
  | Bitnor
  | Bitxnor
  | Notequal
  | If
  | Symbol
  | NextSymbol
  | NondetSymbol
  | PredicateSymbol
  | PredicateNextSymbol
  | NondetBool
  | Empty
  | SideEffect
  | Statement
  | StatementExpression
  | Value
  | Constant
  | Block
  | Decl
  | Dead
  | Assign
  | AssignDiv
  | AssignMult
  | AssignPlus
  | AssignMinus
  | AssignMod
  | AssignShl
  | AssignShr
  | AssignAshr
  | AssignLshr
  | AssignBitand
  | AssignBitxor
  | AssignBitor
  | Assume
  | Assert
  | Assertion
  | Precondition
  | Postcondition
  | PreconditionInstance
  | Goto
  | GccComputedGoto
  | Ifthenelse
  | Label
  | Break
  | Continue
  | FunctionCall
  | Return
  | Skip
  | Arguments
  | Array
  | Size
  | FrontendPointer
  | Pointer
  | BlockPointer
  | Switch
  | SwitchCase
  | GccSwitchCaseRange
  | For
  | While
  | Dowhile
  | Int
  | Integer
  | Natural
  | Real
  | Rational
  | Complex
  | Signed
  | Unsigned
  | Asm
  | GccAsmInput
  | GccAsmOutput
  | GccAsmClobberedRegister
  | Incomplete
  | IncompleteClass
  | CIncomplete
  | Identifier
  | Name
  | InnerName
  | CppName
  | ComponentCppName
  | CIdClass
  | Declaration
  | DeclarationList
  | Declarator
  | Struct
  | CBitField
  | Union
  | Class
  | MergedType
  | Range
  | From
  | To
  | Module
  | Parameter
  | ComponentName
  | ComponentNumber
  | Tag
  | Default
  | CDefaultValue
  | BaseName
  | CBaseName
  | String
  | CStringConstant
  | StringConstant
  | Width
  | Components
  | Bv
  | F
  | With
  | Trans
  | Throw
  | TryCatch
  | Noexcept
  | CPROVERThrow
  | CPROVERTryCatch
  | CPROVERTryFinally
  | Protection
  | Private
  | Public
  | Protected
  | Virtual
  | Volatile
  | Const
  | Constexpr
  | Inline
  | Forall
  | Exists
  | Repeat
  | Extractbit
  | Extractbits
  | CReference
  | CRvalueReference
  | True
  | False
  | AddressOf
  | Dereference
  | CLvalue
  | CBase
  | Destination
  | Main
  | Expression
  | Allocate
  | CCxxAllocType
  | CppNew
  | CppDelete
  | CppNewArray
  | CppDeleteArray
  | JavaNew
  | JavaNewArray
  | JavaNewArrayData
  | JavaStringLiteral
  | Printf
  | Input
  | Output
  | Nondet
  | NULL
  | Null
  | Nullptr
  | CEnum
  | Enumeration
  | Elements
  | Unknown
  | Uninitialized
  | Invalid
  | CInvalidObject
  | PointerOffset
  | PointerObject
  | IsInvalidPointer
  | IeeeFloatEqual
  | IeeeFloatNotequal
  | Isnan
  | Lambda
  | ArrayComprehension
  | ArrayOf
  | ArrayEqual
  | ArraySet
  | ArrayCopy
  | ArrayList
  | Mod
  | Rem
  | Shr
  | Ashr
  | Lshr
  | Shl
  | Rol
  | Ror
  | Comma
  | Concatenation
  | Infinity
  | ReturnType
  | Typedef
  | TypedefType
  | CTypedef
  | Extern
  | Static
  | Auto
  | Register
  | ThreadLocal
  | Thread
  | CThreadLocal
  | CStaticLifetime
  | Mutable
  | Void
  | Int8
  | Int16
  | Int32
  | Int64
  | Ptr32
  | Ptr64
  | Char
  | Short
  | Long
  | Float
  | Double
  | Byte
  | Boolean
  | LongDouble
  | SignedChar
  | UnsignedChar
  | SignedInt
  | UnsignedInt
  | SignedLongInt
  | UnsignedLongInt
  | SignedShortInt
  | UnsignedShortInt
  | SignedLongLongInt
  | UnsignedLongLongInt
  | SignedInt128
  | UnsignedInt128
  | Case
  | CInlined
  | CHide
  | Hide
  | Abs
  | Sign
  | Access
  | CAccess
  | Postincrement
  | Postdecrement
  | Preincrement
  | Predecrement
  | IntegerBits
  | KnR
  | CKnR
  | ConstraintSelectOne
  | Cond
  | BvLiterals
  | IsFinite
  | Isinf
  | Isnormal
  | Alignof
  | ClangBuiltinConvertvector
  | GccBuiltinVaArg
  | GccBuiltinTypesCompatibleP
  | VaStart
  | GccFloat16
  | GccFloat32
  | GccFloat32x
  | GccFloat64
  | GccFloat64x
  | GccFloat80
  | GccFloat128
  | GccFloat128x
  | GccInt128
  | GccDecimal32
  | GccDecimal64
  | GccDecimal128
  | BuiltinOffsetof
  | Id0
  | Id1
  | Sizeof
  | TypeArg
  | ExprArg
  | ExpressionList
  | InitializerList
  | GccConditionalExpression
  | GccLocalLabel
  | Gcc
  | Msc
  | Typeof
  | Ellipsis
  | Flavor
  | Ge
  | Le
  | Gt
  | Lt
  | Plus
  | Minus
  | UnaryMinus
  | UnaryPlus
  | Mult
  | Div
  | Power
  | FactorialPower
  | PrettyName
  | CClass
  | CField
  | CInterface
  | DesignatedInitializer
  | Designator
  | MemberDesignator
  | IndexDesignator
  | CConstant
  | CVolatile
  | CRestricted
  | CIdentifier
  | CImplicit
  | CPtr32
  | CPtr64
  | CAtomic
  | Restrict
  | ByteExtractBigEndian
  | ByteExtractLittleEndian
  | ByteUpdateBigEndian
  | ByteUpdateLittleEndian
  | Replication
  | CproverAtomic
  | Atomic
  | AtomicTypeSpecifier
  | AtomicBegin
  | AtomicEnd
  | StartThread
  | EndThread
  | CoverageCriterion
  | Initializer
  | Anonymous
  | CIsAnonymous
  | IsEnumConstant
  | IsInline
  | IsExtern
  | IsSynchronized
  | IsNativeMethod
  | IsVarargsMethod
  | IsGlobal
  | IsThreadLocal
  | IsParameter
  | IsMember
  | IsType
  | IsRegister
  | IsTypedef
  | IsStatic
  | IsTemplate
  | IsStaticAssert
  | IsVirtual
  | CIsVirtual
  | Literal
  | MemberInitializers
  | MemberInitializer
  | MethodQualifier
  | Methods
  | StaticMembers
  | Constructor
  | Destructor
  | Bases
  | Base
  | FromBase
  | Operator
  | Template
  | TemplateClassInstance
  | TemplateFunctionInstance
  | TemplateType
  | TemplateArgs
  | TemplateParameter
  | TemplateParameterSymbolType
  | TemplateParameters
  | CTemplate
  | CTemplateArguments
  | CTemplateCase
  | Typename
  | C
  | Cpp
  | Java
  | DeclBlock
  | DeclType
  | Parameters
  | WcharT
  | Char16T
  | Char32T
  | SizeT
  | SsizeT
  | Mode
  | This
  | CThis
  | ReductionAnd
  | ReductionOr
  | ReductionNand
  | ReductionNor
  | ReductionXor
  | ReductionXnor
  | CZeroInitializer
  | Body
  | TemporaryObject
  | OverflowPlus
  | OverflowMinus
  | OverflowMult
  | OverflowUnaryMinus
  | OverflowResultPlus
  | OverflowResultMinus
  | OverflowResultMult
  | ObjectDescriptor
  | IsDynamicObject
  | DynamicObject
  | CDynamic
  | ObjectSize
  | GoodPointer
  | IntegerAddress
  | IntegerAddressObject
  | NullObject
  | StaticObject
  | StackObject
  | CIsFailedSymbol
  | CFailedSymbol
  | Friend
  | CFriends
  | Explicit
  | StorageSpec
  | MemberSpec
  | MscDeclspec
  | Packed
  | CPacked
  | TransparentUnion
  | CTransparentUnion
  | Aligned
  | CAlignment
  | FrontendVector
  | Vector
  | Abstract
  | FunctionApplication
  | CppDeclarator
  | CppLinkageSpec
  | CppNamespaceSpec
  | CppStorageSpec
  | CppUsing
  | CppDeclaration
  | CppStaticAssert
  | CppMemberSpec
  | CCType
  | Namespace
  | Linkage
  | Decltype
  | CTagOnlyDeclaration
  | StructTag
  | UnionTag
  | CEnumTag
  | VerilogCaseEquality
  | VerilogCaseInequality
  | UserSpecifiedPredicate
  | UserSpecifiedParameterPredicates
  | UserSpecifiedReturnPredicates
  | Unassigned
  | NewObject
  | ComplexReal
  | ComplexImag
  | Imag
  | MscTryExcept
  | MscTryFinally
  | MscLeave
  | MscUuidof
  | MscIfExists
  | MscIfNotExists
  | MscUnderlyingType
  | MscBased
  | Alias
  | PtrObject
  | CCSizeofType
  | ArrayUpdate
  | Update
  | StaticAssert
  | GccAttributeMode
  | BuiltIn
  | ExceptionList
  | ExceptionId
  | PredicatePassiveSymbol
  | CwVaArgTypeof
  | Fence
  | Sync
  | Lwsync
  | Isync
  | WRfence
  | RRfence
  | RWfence
  | WWfence
  | RRcumul
  | RWcumul
  | WWcumul
  | WRcumul
  | GenericSelection
  | GenericAssociations
  | GenericAssociation
  | FloatbvPlus
  | FloatbvMinus
  | FloatbvMult
  | FloatbvDiv
  | FloatbvRem
  | FloatbvTypecast
  | CompoundLiteral
  | CustomBv
  | CustomUnsignedbv
  | CustomSignedbv
  | CustomFixedbv
  | CustomFloatbv
  | CSSASymbol
  | L0
  | L1
  | L2
  | L1ObjectIdentifier
  | AlreadyTypechecked
  | CVaArgType
  | Smt2Symbol
  | Onehot
  | Onehot0
  | Popcount
  | CountLeadingZeros
  | CountTrailingZeros
  | FunctionType
  | Noreturn
  | CNoreturn
  | Weak
  | IsWeak
  | Used
  | IsUsed
  | CSpecLoopInvariant
  | CSpecRequires
  | CSpecEnsures
  | VirtualFunction
  | ElementType
  | WorkingDirectory
  | Section
  | Bswap
  | BitReverse
  | JavaBytecodeIndex
  | JavaInstanceof
  | JavaSuperMethodCall
  | JavaEnumStaticUnwind
  | PushCatch
  | PopCatch
  | ExceptionLandingpad
  | LengthUpperBound
  | CproverAssociateArrayToPointerFunc
  | CproverAssociateLengthToArrayFunc
  | CproverCharLiteralFunc
  | CproverStringLiteralFunc
  | CproverStringCharAtFunc
  | CproverStringCharSetFunc
  | CproverStringCodePointAtFunc
  | CproverStringCodePointBeforeFunc
  | CproverStringCodePointCountFunc
  | CproverStringOffsetByCodePointFunc
  | CproverStringCompareToFunc
  | CproverStringConcatFunc
  | CproverStringConcatCharFunc
  | CproverStringConcatCodePointFunc
  | CproverStringConstrainCharactersFunc
  | CproverStringContainsFunc
  | CproverStringCopyFunc
  | CproverStringDeleteFunc
  | CproverStringDeleteCharAtFunc
  | CproverStringEqualFunc
  | CproverStringEqualsIgnoreCaseFunc
  | CproverStringEmptyStringFunc
  | CproverStringEndswithFunc
  | CproverStringFormatFunc
  | CproverStringIndexOfFunc
  | CproverStringInsertFunc
  | CproverStringIsPrefixFunc
  | CproverStringIsSuffixFunc
  | CproverStringIsEmptyFunc
  | CproverStringLastIndexOfFunc
  | CproverStringLengthFunc
  | CproverStringOfIntFunc
  | CproverStringOfIntHexFunc
  | CproverStringOfLongFunc
  | CproverStringOfFloatFunc
  | CproverStringOfFloatScientificNotationFunc
  | CproverStringOfDoubleFunc
  | CproverStringParseIntFunc
  | CproverStringIsValidIntFunc
  | CproverStringIsValidLongFunc
  | CproverStringReplaceFunc
  | CproverStringSetLengthFunc
  | CproverStringStartswithFunc
  | CproverStringSubstringFunc
  | CproverStringToLowerCaseFunc
  | CproverStringToUpperCaseFunc
  | CproverStringTrimFunc
  | SkipInitialize
  | BasicBlockCoveredLines
  | BasicBlockSourceLines
  | IsNondetNullable
  | ArrayReplace
  | SwitchCaseNumber
  | JavaArrayAccess
  | JavaMemberAccess
  | CJavaGenericParameter
  | CJavaGenericsClassType
  | CJavaImplicitlyGenericClassType
  | CJavaGenericSymbol
  | GenericTypes
  | ImplicitGenericTypes
  | TypeVariables
  | HandleType
  | JavaLambdaMethodHandle
  | JavaLambdaMethodHandleIndex
  | JavaLambdaMethodHandles
  | HavocObject
  | OverflowShl
  | CNoInitializationRequired
  | CNoNondetInitialization
  | OverlayClass
  | OverlayMethod
  | IgnoredMethod
  | IsAnnotation
  | CAnnotations
  | Final
  | BitsPerByte
  | CAbstract
  | Synthetic
  | Interface
  | CMustNotThrow
  | IsInnerClass
  | IsAnonymous
  | OuterClass
  | IsBridgeMethod
  | CIsOperator
  | CNotAccessible
  | COverrideConstantness
  | CBound
  | CBoundsCheck
  | CIsStatic
  | CCallByValue
  | CVirtualName
  | CUnnamedObject
  | CTemporaryAvoided
  | CQualifier
  | CArrayIni
  | ROk
  | WOk
  | SuperClass
  | ExceptionsThrownList
  | CJavaMethodType
  | Compiled
  | PartialSpecializationArgs
  | SpecializationOf
  | InitArgs
  | Ambiguous
  | SpecializationTemplateArgs
  | FullTemplateArgs
  | InstantiatedWith
  | TemplateMethods
  | CppNotTypechecked
  | Noaccess
  | IsOperator
  | IsCastOperator
  | IsExplicit
  | IsMutable
  | VirtualName
  | IsPureVirtual
  | IsVtptr
  | Prefix
  | Cv
  | CppDummyDestructor
  | CastExpression
  | PodConstructor
  | TemplateDecls
  | ThrowDecl
  | Typeid
  | CQuoted
  | ToMember
  | PointerToMember
  | Tuple
  | FunctionBody
  | GetMay
  | SetMay
  | ClearMay
  | GetMust
  | SetMust
  | ClearMust
  | Pragma
  | StatementList
  | StatementListType
  | StatementListFunction
  | StatementListFunctionBlock
  | StatementListMainFunction
  | StatementListDataBlock
  | StatementListVersion
  | StatementListVarInput
  | StatementListVarInout
  | StatementListVarOutput
  | StatementListVarConstant
  | StatementListVarTemp
  | StatementListVarStatic
  | StatementListReturn
  | StatementListReturnValueId
  | StatementListVarEntry
  | StatementListVarDecls
  | StatementListNetwork
  | StatementListNetworks
  | StatementListTitle
  | StatementListIdentifier
  | StatementListLoad
  | StatementListTransfer
  | StatementListCall
  | StatementListNop
  | StatementListConstAdd
  | StatementListAccuIntAdd
  | StatementListAccuIntSub
  | StatementListAccuIntMul
  | StatementListAccuIntDiv
  | StatementListAccuIntEq
  | StatementListAccuIntNeq
  | StatementListAccuIntGt
  | StatementListAccuIntLt
  | StatementListAccuIntGte
  | StatementListAccuIntLte
  | StatementListAccuRealAdd
  | StatementListAccuRealSub
  | StatementListAccuRealMul
  | StatementListAccuRealDiv
  | StatementListAccuRealEq
  | StatementListAccuRealNeq
  | StatementListAccuRealGt
  | StatementListAccuRealLt
  | StatementListAccuRealGte
  | StatementListAccuRealLte
  | StatementListAccuDintAdd
  | StatementListAccuDintSub
  | StatementListAccuDintMul
  | StatementListAccuDintDiv
  | StatementListAccuDintEq
  | StatementListAccuDintNeq
  | StatementListAccuDintGt
  | StatementListAccuDintLt
  | StatementListAccuDintGte
  | StatementListAccuDintLte
  | StatementListAnd
  | StatementListAndNot
  | StatementListOr
  | StatementListOrNot
  | StatementListXor
  | StatementListXorNot
  | StatementListAndNested
  | StatementListAndNotNested
  | StatementListOrNested
  | StatementListOrNotNested
  | StatementListXorNested
  | StatementListXorNotNested
  | StatementListNestingClosed
  | StatementListAssign
  | StatementListSetRlo
  | StatementListClrRlo
  | StatementListSet
  | StatementListReset
  | StatementListNot
  | StatementListInstruction
  | StatementListInstructions

let of_string str =
  match str with
  | "" -> EmptyString
  | "let" -> Let
  | "let_binding" -> LetBinding
  | "nil" -> Nil
  | "type" -> Type
  | "bool" -> Bool
  | "c_bool" -> CBool
  | "proper_bool" -> ProperBool
  | "signedbv" -> Signedbv
  | "unsignedbv" -> Unsignedbv
  | "verilog_signedbv" -> VerilogSignedbv
  | "verilog_unsignedbv" -> VerilogUnsignedbv
  | "floatbv" -> Floatbv
  | "fixedbv" -> Fixedbv
  | "x86_extended" -> X86Extended
  | "#source_location" -> CSourceLocation
  | "#end_location" -> CEndLocation
  | "#is_padding" -> CIsPadding
  | "C_do_not_dump" -> CDoNotDump
  | "file" -> File
  | "line" -> Line
  | "column" -> Column
  | "comment" -> Comment
  | "property" -> Property
  | "property_class" -> PropertyClass
  | "property_id" -> PropertyId
  | "function" -> Function
  | "mathematical_function" -> MathematicalFunction
  | "code" -> Code
  | "typecast" -> Typecast
  | "static_cast" -> StaticCast
  | "dynamic_cast" -> DynamicCast
  | "const_cast" -> ConstCast
  | "reinterpret_cast" -> ReinterpretCast
  | "index" -> Index
  | "ptrmember" -> Ptrmember
  | "member" -> Member
  | "member_name" -> MemberName
  | "#member_name" -> CMemberName
  | "=" -> Equal
  | "=>" -> Implies
  | "and" -> And
  | "nand" -> Nand
  | "or" -> Or
  | "nor" -> Nor
  | "xor" -> Xor
  | "not" -> Not
  | "bitand" -> Bitand
  | "bitor" -> Bitor
  | "bitnot" -> Bitnot
  | "bitxor" -> Bitxor
  | "bitnand" -> Bitnand
  | "bitnor" -> Bitnor
  | "bitxnor" -> Bitxnor
  | "notequal" -> Notequal
  | "if" -> If
  | "symbol" -> Symbol
  | "next_symbol" -> NextSymbol
  | "nondet_symbol" -> NondetSymbol
  | "predicate_symbol" -> PredicateSymbol
  | "predicate_next_symbol" -> PredicateNextSymbol
  | "nondet_bool" -> NondetBool
  | "empty" -> Empty
  | "side_effect" -> SideEffect
  | "statement" -> Statement
  | "statement_expression" -> StatementExpression
  | "value" -> Value
  | "constant" -> Constant
  | "block" -> Block
  | "decl" -> Decl
  | "dead" -> Dead
  | "assign" -> Assign
  | "assign_div" -> AssignDiv
  | "assign*" -> AssignMult
  | "assign+" -> AssignPlus
  | "assign-" -> AssignMinus
  | "assign_mod" -> AssignMod
  | "assign_shl" -> AssignShl
  | "assign_shr" -> AssignShr
  | "assign_ashr" -> AssignAshr
  | "assign_lshr" -> AssignLshr
  | "assign_bitand" -> AssignBitand
  | "assign_bitxor" -> AssignBitxor
  | "assign_bitor" -> AssignBitor
  | "assume" -> Assume
  | "assert" -> Assert
  | "assertion" -> Assertion
  | "precondition" -> Precondition
  | "postcondition" -> Postcondition
  | "precondition_instance" -> PreconditionInstance
  | "goto" -> Goto
  | "gcc_computed_goto" -> GccComputedGoto
  | "ifthenelse" -> Ifthenelse
  | "label" -> Label
  | "break" -> Break
  | "continue" -> Continue
  | "function_call" -> FunctionCall
  | "return" -> Return
  | "skip" -> Skip
  | "arguments" -> Arguments
  | "array" -> Array
  | "size" -> Size
  | "frontend_pointer" -> FrontendPointer
  | "pointer" -> Pointer
  | "block_pointer" -> BlockPointer
  | "switch" -> Switch
  | "switch_case" -> SwitchCase
  | "gcc_switch_case_range" -> GccSwitchCaseRange
  | "for" -> For
  | "while" -> While
  | "dowhile" -> Dowhile
  | "int" -> Int
  | "integer" -> Integer
  | "natural" -> Natural
  | "real" -> Real
  | "rational" -> Rational
  | "complex" -> Complex
  | "signed" -> Signed
  | "unsigned" -> Unsigned
  | "asm" -> Asm
  | "gcc_asm_input" -> GccAsmInput
  | "gcc_asm_output" -> GccAsmOutput
  | "gcc_asm_clobbered_register" -> GccAsmClobberedRegister
  | "incomplete" -> Incomplete
  | "incomplete_class" -> IncompleteClass
  | "#incomplete" -> CIncomplete
  | "identifier" -> Identifier
  | "name" -> Name
  | "inner_name" -> InnerName
  | "cpp_name" -> CppName
  | "component_cpp_name" -> ComponentCppName
  | "#id_class" -> CIdClass
  | "declaration" -> Declaration
  | "declaration_list" -> DeclarationList
  | "declarator" -> Declarator
  | "struct" -> Struct
  | "c_bit_field" -> CBitField
  | "union" -> Union
  | "class" -> Class
  | "merged_type" -> MergedType
  | "range" -> Range
  | "from" -> From
  | "to" -> To
  | "module" -> Module
  | "parameter" -> Parameter
  | "component_name" -> ComponentName
  | "component_number" -> ComponentNumber
  | "tag" -> Tag
  | "default" -> Default
  | "#default_value" -> CDefaultValue
  | "base_name" -> BaseName
  | "#base_name" -> CBaseName
  | "string" -> String
  | "#string_constant" -> CStringConstant
  | "string_constant" -> StringConstant
  | "width" -> Width
  | "components" -> Components
  | "bv" -> Bv
  | "f" -> F
  | "with" -> With
  | "trans" -> Trans
  | "throw" -> Throw
  | "try_catch" -> TryCatch
  | "noexcept" -> Noexcept
  | "CPROVER_throw" -> CPROVERThrow
  | "CPROVER_try_catch" -> CPROVERTryCatch
  | "CPROVER_try_finally" -> CPROVERTryFinally
  | "protection" -> Protection
  | "private" -> Private
  | "public" -> Public
  | "protected" -> Protected
  | "virtual" -> Virtual
  | "volatile" -> Volatile
  | "const" -> Const
  | "constexpr" -> Constexpr
  | "inline" -> Inline
  | "forall" -> Forall
  | "exists" -> Exists
  | "repeat" -> Repeat
  | "extractbit" -> Extractbit
  | "extractbits" -> Extractbits
  | "#reference" -> CReference
  | "#rvalue_reference" -> CRvalueReference
  | "true" -> True
  | "false" -> False
  | "address_of" -> AddressOf
  | "dereference" -> Dereference
  | "#lvalue" -> CLvalue
  | "#base" -> CBase
  | "destination" -> Destination
  | "main" -> Main
  | "expression" -> Expression
  | "allocate" -> Allocate
  | "#cxx_alloc_type" -> CCxxAllocType
  | "cpp_new" -> CppNew
  | "cpp_delete" -> CppDelete
  | "cpp_new[]" -> CppNewArray
  | "cpp_delete[]" -> CppDeleteArray
  | "java_new" -> JavaNew
  | "java_new_array" -> JavaNewArray
  | "java_new_array_data" -> JavaNewArrayData
  | "java_string_literal" -> JavaStringLiteral
  | "printf" -> Printf
  | "input" -> Input
  | "output" -> Output
  | "nondet" -> Nondet
  | "NULL" -> NULL
  | "null" -> Null
  | "nullptr" -> Nullptr
  | "c_enum" -> CEnum
  | "enumeration" -> Enumeration
  | "elements" -> Elements
  | "unknown" -> Unknown
  | "uninitialized" -> Uninitialized
  | "invalid" -> Invalid
  | "#invalid_object" -> CInvalidObject
  | "pointer_offset" -> PointerOffset
  | "pointer_object" -> PointerObject
  | "is_invalid_pointer" -> IsInvalidPointer
  | "ieee_float_equal" -> IeeeFloatEqual
  | "ieee_float_notequal" -> IeeeFloatNotequal
  | "isnan" -> Isnan
  | "lambda" -> Lambda
  | "array_comprehension" -> ArrayComprehension
  | "array_of" -> ArrayOf
  | "array_equal" -> ArrayEqual
  | "array_set" -> ArraySet
  | "array_copy" -> ArrayCopy
  | "array_list" -> ArrayList
  | "mod" -> Mod
  | "rem" -> Rem
  | "shr" -> Shr
  | "ashr" -> Ashr
  | "lshr" -> Lshr
  | "shl" -> Shl
  | "rol" -> Rol
  | "ror" -> Ror
  | "comma" -> Comma
  | "concatenation" -> Concatenation
  | "infinity" -> Infinity
  | "return_type" -> ReturnType
  | "typedef" -> Typedef
  | "typedef_type" -> TypedefType
  | "#typedef" -> CTypedef
  | "extern" -> Extern
  | "static" -> Static
  | "auto" -> Auto
  | "register" -> Register
  | "thread_local" -> ThreadLocal
  | "thread" -> Thread
  | "#thread_local" -> CThreadLocal
  | "#static_lifetime" -> CStaticLifetime
  | "mutable" -> Mutable
  | "void" -> Void
  | "int8" -> Int8
  | "int16" -> Int16
  | "int32" -> Int32
  | "int64" -> Int64
  | "ptr32" -> Ptr32
  | "ptr64" -> Ptr64
  | "char" -> Char
  | "short" -> Short
  | "long" -> Long
  | "float" -> Float
  | "double" -> Double
  | "byte" -> Byte
  | "boolean" -> Boolean
  | "long_double" -> LongDouble
  | "signed_char" -> SignedChar
  | "unsigned_char" -> UnsignedChar
  | "signed_int" -> SignedInt
  | "unsigned_int" -> UnsignedInt
  | "signed_long_int" -> SignedLongInt
  | "unsigned_long_int" -> UnsignedLongInt
  | "signed_short_int" -> SignedShortInt
  | "unsigned_short_int" -> UnsignedShortInt
  | "signed_long_long_int" -> SignedLongLongInt
  | "unsigned_long_long_int" -> UnsignedLongLongInt
  | "signed_int128" -> SignedInt128
  | "unsigned_int128" -> UnsignedInt128
  | "case" -> Case
  | "#inlined" -> CInlined
  | "#hide" -> CHide
  | "hide" -> Hide
  | "abs" -> Abs
  | "sign" -> Sign
  | "access" -> Access
  | "#access" -> CAccess
  | "postincrement" -> Postincrement
  | "postdecrement" -> Postdecrement
  | "preincrement" -> Preincrement
  | "predecrement" -> Predecrement
  | "integer_bits" -> IntegerBits
  | "KnR" -> KnR
  | "#KnR" -> CKnR
  | "constraint_select_one" -> ConstraintSelectOne
  | "cond" -> Cond
  | "bv_literals" -> BvLiterals
  | "isfinite" -> IsFinite
  | "isinf" -> Isinf
  | "isnormal" -> Isnormal
  | "alignof" -> Alignof
  | "clang_builtin_convertvector" -> ClangBuiltinConvertvector
  | "gcc_builtin_va_arg" -> GccBuiltinVaArg
  | "gcc_builtin_types_compatible_p" -> GccBuiltinTypesCompatibleP
  | "va_start" -> VaStart
  | "gcc_float16" -> GccFloat16
  | "gcc_float32" -> GccFloat32
  | "gcc_float32x" -> GccFloat32x
  | "gcc_float64" -> GccFloat64
  | "gcc_float64x" -> GccFloat64x
  | "gcc_float80" -> GccFloat80
  | "gcc_float128" -> GccFloat128
  | "gcc_float128x" -> GccFloat128x
  | "gcc_int128" -> GccInt128
  | "gcc_decimal32" -> GccDecimal32
  | "gcc_decimal64" -> GccDecimal64
  | "gcc_decimal128" -> GccDecimal128
  | "builtin_offsetof" -> BuiltinOffsetof
  | "0" -> Id0
  | "1" -> Id1
  | "sizeof" -> Sizeof
  | "type_arg" -> TypeArg
  | "expr_arg" -> ExprArg
  | "expression_list" -> ExpressionList
  | "initializer_list" -> InitializerList
  | "gcc_conditional_expression" -> GccConditionalExpression
  | "gcc_local_label" -> GccLocalLabel
  | "gcc" -> Gcc
  | "msc" -> Msc
  | "typeof" -> Typeof
  | "ellipsis" -> Ellipsis
  | "flavor" -> Flavor
  | ">=" -> Ge
  | "<=" -> Le
  | ">" -> Gt
  | "<" -> Lt
  | "+" -> Plus
  | "-" -> Minus
  | "unary-" -> UnaryMinus
  | "unary+" -> UnaryPlus
  | "*" -> Mult
  | "/" -> Div
  | "**" -> Power
  | "factorial_power" -> FactorialPower
  | "pretty_name" -> PrettyName
  | "#class" -> CClass
  | "#field" -> CField
  | "#interface" -> CInterface
  | "designated_initializer" -> DesignatedInitializer
  | "designator" -> Designator
  | "member_designator" -> MemberDesignator
  | "index_designator" -> IndexDesignator
  | "#constant" -> CConstant
  | "#volatile" -> CVolatile
  | "#restricted" -> CRestricted
  | "#identifier" -> CIdentifier
  | "#implicit" -> CImplicit
  | "#ptr32" -> CPtr32
  | "#ptr64" -> CPtr64
  | "#atomic" -> CAtomic
  | "restrict" -> Restrict
  | "byte_extract_big_endian" -> ByteExtractBigEndian
  | "byte_extract_little_endian" -> ByteExtractLittleEndian
  | "byte_update_big_endian" -> ByteUpdateBigEndian
  | "byte_update_little_endian" -> ByteUpdateLittleEndian
  | "replication" -> Replication
  | "cprover_atomic" -> CproverAtomic
  | "atomic" -> Atomic
  | "atomic_type_specifier" -> AtomicTypeSpecifier
  | "atomic_begin" -> AtomicBegin
  | "atomic_end" -> AtomicEnd
  | "start_thread" -> StartThread
  | "end_thread" -> EndThread
  | "coverage_criterion" -> CoverageCriterion
  | "initializer" -> Initializer
  | "anonymous" -> Anonymous
  | "#is_anonymous" -> CIsAnonymous
  | "is_enum_constant" -> IsEnumConstant
  | "is_inline" -> IsInline
  | "is_extern" -> IsExtern
  | "is_synchronized" -> IsSynchronized
  | "is_native_method" -> IsNativeMethod
  | "is_varargs_method" -> IsVarargsMethod
  | "is_global" -> IsGlobal
  | "is_thread_local" -> IsThreadLocal
  | "is_parameter" -> IsParameter
  | "is_member" -> IsMember
  | "is_type" -> IsType
  | "is_register" -> IsRegister
  | "is_typedef" -> IsTypedef
  | "is_static" -> IsStatic
  | "is_template" -> IsTemplate
  | "is_static_assert" -> IsStaticAssert
  | "is_virtual" -> IsVirtual
  | "#is_virtual" -> CIsVirtual
  | "literal" -> Literal
  | "member_initializers" -> MemberInitializers
  | "member_initializer" -> MemberInitializer
  | "method_qualifier" -> MethodQualifier
  | "methods" -> Methods
  | "static_members" -> StaticMembers
  | "constructor" -> Constructor
  | "destructor" -> Destructor
  | "bases" -> Bases
  | "base" -> Base
  | "from_base" -> FromBase
  | "operator" -> Operator
  | "template" -> Template
  | "template_class_instance" -> TemplateClassInstance
  | "template_function_instance" -> TemplateFunctionInstance
  | "template_type" -> TemplateType
  | "template_args" -> TemplateArgs
  | "template_parameter" -> TemplateParameter
  | "template_parameter_symbol_type" -> TemplateParameterSymbolType
  | "template_parameters" -> TemplateParameters
  | "#template" -> CTemplate
  | "#template_arguments" -> CTemplateArguments
  | "#template_case" -> CTemplateCase
  | "typename" -> Typename
  | "C" -> C
  | "cpp" -> Cpp
  | "java" -> Java
  | "decl_block" -> DeclBlock
  | "decl_type" -> DeclType
  | "parameters" -> Parameters
  | "wchar_t" -> WcharT
  | "char16_t" -> Char16T
  | "char32_t" -> Char32T
  | "size_t" -> SizeT
  | "ssize_t" -> SsizeT
  | "mode" -> Mode
  | "this" -> This
  | "#this" -> CThis
  | "reduction_and" -> ReductionAnd
  | "reduction_or" -> ReductionOr
  | "reduction_nand" -> ReductionNand
  | "reduction_nor" -> ReductionNor
  | "reduction_xor" -> ReductionXor
  | "reduction_xnor" -> ReductionXnor
  | "#zero_initializer" -> CZeroInitializer
  | "body" -> Body
  | "temporary_object" -> TemporaryObject
  | "overflow-+" -> OverflowPlus
  | "overflow--" -> OverflowMinus
  | "overflow-*" -> OverflowMult
  | "overflow-unary-" -> OverflowUnaryMinus
  | "overflow_result-+" -> OverflowResultPlus
  | "overflow_result--" -> OverflowResultMinus
  | "overflow_result-*" -> OverflowResultMult
  | "object_descriptor" -> ObjectDescriptor
  | "is_dynamic_object" -> IsDynamicObject
  | "dynamic_object" -> DynamicObject
  | "#dynamic" -> CDynamic
  | "object_size" -> ObjectSize
  | "good_pointer" -> GoodPointer
  | "integer_address" -> IntegerAddress
  | "integer_address_object" -> IntegerAddressObject
  | "NULL-object" -> NullObject
  | "static_object" -> StaticObject
  | "stack_object" -> StackObject
  | "#is_failed_symbol" -> CIsFailedSymbol
  | "#failed_symbol" -> CFailedSymbol
  | "friend" -> Friend
  | "#friends" -> CFriends
  | "explicit" -> Explicit
  | "storage_spec" -> StorageSpec
  | "member_spec" -> MemberSpec
  | "msc_declspec" -> MscDeclspec
  | "packed" -> Packed
  | "#packed" -> CPacked
  | "transparent_union" -> TransparentUnion
  | "#transparent_union" -> CTransparentUnion
  | "aligned" -> Aligned
  | "#alignment" -> CAlignment
  | "frontend_vector" -> FrontendVector
  | "vector" -> Vector
  | "abstract" -> Abstract
  | "function_application" -> FunctionApplication
  | "cpp_declarator" -> CppDeclarator
  | "cpp_linkage_spec" -> CppLinkageSpec
  | "cpp_namespace_spec" -> CppNamespaceSpec
  | "cpp_storage_spec" -> CppStorageSpec
  | "cpp_using" -> CppUsing
  | "cpp_declaration" -> CppDeclaration
  | "cpp_static_assert" -> CppStaticAssert
  | "cpp_member_spec" -> CppMemberSpec
  | "#c_type" -> CCType
  | "namespace" -> Namespace
  | "linkage" -> Linkage
  | "decltype" -> Decltype
  | "#tag_only_declaration" -> CTagOnlyDeclaration
  | "struct_tag" -> StructTag
  | "union_tag" -> UnionTag
  | "c_enum_tag" -> CEnumTag
  | "verilog_case_equality" -> VerilogCaseEquality
  | "verilog_case_inequality" -> VerilogCaseInequality
  | "user_specified_predicate" -> UserSpecifiedPredicate
  | "user_specified_parameter_predicates" -> UserSpecifiedParameterPredicates
  | "user_specified_return_predicates" -> UserSpecifiedReturnPredicates
  | "unassigned" -> Unassigned
  | "new_object" -> NewObject
  | "complex_real" -> ComplexReal
  | "complex_imag" -> ComplexImag
  | "imag" -> Imag
  | "msc_try_except" -> MscTryExcept
  | "msc_try_finally" -> MscTryFinally
  | "msc_leave" -> MscLeave
  | "msc_uuidof" -> MscUuidof
  | "msc_if_exists" -> MscIfExists
  | "msc_if_not_exists" -> MscIfNotExists
  | "msc_underlying_type" -> MscUnderlyingType
  | "msc_based" -> MscBased
  | "alias" -> Alias
  | "ptr_object" -> PtrObject
  | "#c_sizeof_type" -> CCSizeofType
  | "array_update" -> ArrayUpdate
  | "update" -> Update
  | "static_assert" -> StaticAssert
  | "gcc_attribute_mode" -> GccAttributeMode
  | "<built-in>" -> BuiltIn
  | "exception_list" -> ExceptionList
  | "exception_id" -> ExceptionId
  | "predicate_passive_symbol" -> PredicatePassiveSymbol
  | "cw_va_arg_typeof" -> CwVaArgTypeof
  | "fence" -> Fence
  | "sync" -> Sync
  | "lwsync" -> Lwsync
  | "isync" -> Isync
  | "WRfence" -> WRfence
  | "RRfence" -> RRfence
  | "RWfence" -> RWfence
  | "WWfence" -> WWfence
  | "RRcumul" -> RRcumul
  | "RWcumul" -> RWcumul
  | "WWcumul" -> WWcumul
  | "WRcumul" -> WRcumul
  | "generic_selection" -> GenericSelection
  | "generic_associations" -> GenericAssociations
  | "generic_association" -> GenericAssociation
  | "floatbv_plus" -> FloatbvPlus
  | "floatbv_minus" -> FloatbvMinus
  | "floatbv_mult" -> FloatbvMult
  | "floatbv_div" -> FloatbvDiv
  | "floatbv_rem" -> FloatbvRem
  | "floatbv_typecast" -> FloatbvTypecast
  | "compound_literal" -> CompoundLiteral
  | "custom_bv" -> CustomBv
  | "custom_unsignedbv" -> CustomUnsignedbv
  | "custom_signedbv" -> CustomSignedbv
  | "custom_fixedbv" -> CustomFixedbv
  | "custom_floatbv" -> CustomFloatbv
  | "#SSA_symbol" -> CSSASymbol
  | "L0" -> L0
  | "L1" -> L1
  | "L2" -> L2
  | "L1_object_identifier" -> L1ObjectIdentifier
  | "already_typechecked" -> AlreadyTypechecked
  | "#va_arg_type" -> CVaArgType
  | "smt2_symbol" -> Smt2Symbol
  | "onehot" -> Onehot
  | "onehot0" -> Onehot0
  | "popcount" -> Popcount
  | "count_leading_zeros" -> CountLeadingZeros
  | "count_trailing_zeros" -> CountTrailingZeros
  | "function_type" -> FunctionType
  | "noreturn" -> Noreturn
  | "#noreturn" -> CNoreturn
  | "weak" -> Weak
  | "is_weak" -> IsWeak
  | "used" -> Used
  | "is_used" -> IsUsed
  | "#spec_loop_invariant" -> CSpecLoopInvariant
  | "#spec_requires" -> CSpecRequires
  | "#spec_ensures" -> CSpecEnsures
  | "virtual_function" -> VirtualFunction
  | "element_type" -> ElementType
  | "working_directory" -> WorkingDirectory
  | "section" -> Section
  | "bswap" -> Bswap
  | "bitreverse" -> BitReverse
  | "java_bytecode_index" -> JavaBytecodeIndex
  | "java_instanceof" -> JavaInstanceof
  | "java_super_method_call" -> JavaSuperMethodCall
  | "java_enum_static_unwind" -> JavaEnumStaticUnwind
  | "push_catch" -> PushCatch
  | "pop_catch" -> PopCatch
  | "exception_landingpad" -> ExceptionLandingpad
  | "length_upper_bound" -> LengthUpperBound
  | "cprover_associate_array_to_pointer_func" ->
      CproverAssociateArrayToPointerFunc
  | "cprover_associate_length_to_array_func" ->
      CproverAssociateLengthToArrayFunc
  | "cprover_char_literal_func" -> CproverCharLiteralFunc
  | "cprover_string_literal_func" -> CproverStringLiteralFunc
  | "cprover_string_char_at_func" -> CproverStringCharAtFunc
  | "cprover_string_char_set_func" -> CproverStringCharSetFunc
  | "cprover_string_code_point_at_func" -> CproverStringCodePointAtFunc
  | "cprover_string_code_point_before_func" -> CproverStringCodePointBeforeFunc
  | "cprover_string_code_point_count_func" -> CproverStringCodePointCountFunc
  | "cprover_string_offset_by_code_point_func" ->
      CproverStringOffsetByCodePointFunc
  | "cprover_string_compare_to_func" -> CproverStringCompareToFunc
  | "cprover_string_concat_func" -> CproverStringConcatFunc
  | "cprover_string_concat_char_func" -> CproverStringConcatCharFunc
  | "cprover_string_concat_code_point_func" -> CproverStringConcatCodePointFunc
  | "cprover_string_constrain_characters_func" ->
      CproverStringConstrainCharactersFunc
  | "cprover_string_contains_func" -> CproverStringContainsFunc
  | "cprover_string_copy_func" -> CproverStringCopyFunc
  | "cprover_string_delete_func" -> CproverStringDeleteFunc
  | "cprover_string_delete_char_at_func" -> CproverStringDeleteCharAtFunc
  | "cprover_string_equal_func" -> CproverStringEqualFunc
  | "cprover_string_equals_ignore_case_func" ->
      CproverStringEqualsIgnoreCaseFunc
  | "cprover_string_empty_string_func" -> CproverStringEmptyStringFunc
  | "cprover_string_endswith_func" -> CproverStringEndswithFunc
  | "cprover_string_format_func" -> CproverStringFormatFunc
  | "cprover_string_index_of_func" -> CproverStringIndexOfFunc
  | "cprover_string_insert_func" -> CproverStringInsertFunc
  | "cprover_string_is_prefix_func" -> CproverStringIsPrefixFunc
  | "cprover_string_is_suffix_func" -> CproverStringIsSuffixFunc
  | "cprover_string_is_empty_func" -> CproverStringIsEmptyFunc
  | "cprover_string_last_index_of_func" -> CproverStringLastIndexOfFunc
  | "cprover_string_length_func" -> CproverStringLengthFunc
  | "cprover_string_of_int_func" -> CproverStringOfIntFunc
  | "cprover_string_of_int_hex_func" -> CproverStringOfIntHexFunc
  | "cprover_string_of_long_func" -> CproverStringOfLongFunc
  | "cprover_string_of_float_func" -> CproverStringOfFloatFunc
  | "cprover_string_of_float_scientific_notation_func" ->
      CproverStringOfFloatScientificNotationFunc
  | "cprover_string_of_double_func" -> CproverStringOfDoubleFunc
  | "cprover_string_parse_int_func" -> CproverStringParseIntFunc
  | "cprover_string_is_valid_int_func" -> CproverStringIsValidIntFunc
  | "cprover_string_is_valid_long_func" -> CproverStringIsValidLongFunc
  | "cprover_string_replace_func" -> CproverStringReplaceFunc
  | "cprover_string_set_length_func" -> CproverStringSetLengthFunc
  | "cprover_string_startswith_func" -> CproverStringStartswithFunc
  | "cprover_string_substring_func" -> CproverStringSubstringFunc
  | "cprover_string_to_lower_case_func" -> CproverStringToLowerCaseFunc
  | "cprover_string_to_upper_case_func" -> CproverStringToUpperCaseFunc
  | "cprover_string_trim_func" -> CproverStringTrimFunc
  | "skip_initialize" -> SkipInitialize
  | "basic_block_covered_lines" -> BasicBlockCoveredLines
  | "basic_block_source_lines" -> BasicBlockSourceLines
  | "is_nondet_nullable" -> IsNondetNullable
  | "array_replace" -> ArrayReplace
  | "switch_case_number" -> SwitchCaseNumber
  | "java_array_access" -> JavaArrayAccess
  | "java_member_access" -> JavaMemberAccess
  | "#java_generic_parameter" -> CJavaGenericParameter
  | "#java_generics_class_type" -> CJavaGenericsClassType
  | "#java_implicitly_generic_class_type" -> CJavaImplicitlyGenericClassType
  | "#java_generic_symbol" -> CJavaGenericSymbol
  | "generic_types" -> GenericTypes
  | "#implicit_generic_types" -> ImplicitGenericTypes
  | "type_variables" -> TypeVariables
  | "handle_type" -> HandleType
  | "java_lambda_method_handle" -> JavaLambdaMethodHandle
  | "lambda_method_handle_index" -> JavaLambdaMethodHandleIndex
  | "lambda_method_handles" -> JavaLambdaMethodHandles
  | "havoc_object" -> HavocObject
  | "overflow-shl" -> OverflowShl
  | "#no_initialization_required" -> CNoInitializationRequired
  | "#no_nondet_initialization" -> CNoNondetInitialization
  | "java::org.cprover.OverlayClassImplementation" -> OverlayClass
  | "java::org.cprover.OverlayMethodImplementation" -> OverlayMethod
  | "java::org.cprover.IgnoredMethodImplementation" -> IgnoredMethod
  | "is_annotation" -> IsAnnotation
  | "#annotations" -> CAnnotations
  | "final" -> Final
  | "bits_per_byte" -> BitsPerByte
  | "#abstract" -> CAbstract
  | "synthetic" -> Synthetic
  | "interface" -> Interface
  | "#must_not_throw" -> CMustNotThrow
  | "is_inner_class" -> IsInnerClass
  | "is_anonymous" -> IsAnonymous
  | "outer_class" -> OuterClass
  | "is_bridge_method" -> IsBridgeMethod
  | "#is_operator" -> CIsOperator
  | "#not_accessible" -> CNotAccessible
  | "#override_constantness" -> COverrideConstantness
  | "#bound" -> CBound
  | "#bounds_check" -> CBoundsCheck
  | "#is_static" -> CIsStatic
  | "#call_by_value" -> CCallByValue
  | "#virtual_name" -> CVirtualName
  | "#unnamed_object" -> CUnnamedObject
  | "#temporary_avoided" -> CTemporaryAvoided
  | "#qualifier" -> CQualifier
  | "#array_ini" -> CArrayIni
  | "r_ok" -> ROk
  | "w_ok" -> WOk
  | "super_class" -> SuperClass
  | "exceptions_thrown_list" -> ExceptionsThrownList
  | "#java_method_type" -> CJavaMethodType
  | "compiled" -> Compiled
  | "partial_specialization_args" -> PartialSpecializationArgs
  | "specialization_of" -> SpecializationOf
  | "init_args" -> InitArgs
  | "ambiguous" -> Ambiguous
  | "specialization_template_args" -> SpecializationTemplateArgs
  | "full_template_args" -> FullTemplateArgs
  | "instantiated_with" -> InstantiatedWith
  | "template_methods" -> TemplateMethods
  | "cpp_not_typechecked" -> CppNotTypechecked
  | "noaccess" -> Noaccess
  | "is_operator" -> IsOperator
  | "is_cast_operator" -> IsCastOperator
  | "is_explicit" -> IsExplicit
  | "is_mutable" -> IsMutable
  | "virtual_name" -> VirtualName
  | "is_pure_virtual" -> IsPureVirtual
  | "is_vtptr" -> IsVtptr
  | "prefix" -> Prefix
  | "cv" -> Cv
  | "cpp_dummy_destructor" -> CppDummyDestructor
  | "cast_expression" -> CastExpression
  | "pod_constructor" -> PodConstructor
  | "template_decls" -> TemplateDecls
  | "throw_decl" -> ThrowDecl
  | "typeid" -> Typeid
  | "#quoted" -> CQuoted
  | "to_member" -> ToMember
  | "pointer_to_member" -> PointerToMember
  | "tuple" -> Tuple
  | "function_body" -> FunctionBody
  | "get_may" -> GetMay
  | "set_may" -> SetMay
  | "clear_may" -> ClearMay
  | "get_must" -> GetMust
  | "set_must" -> SetMust
  | "clear_must" -> ClearMust
  | "pragma" -> Pragma
  | "Statement List" -> StatementList
  | "statement_list_type" -> StatementListType
  | "statement_list_function" -> StatementListFunction
  | "statement_list_function_block" -> StatementListFunctionBlock
  | "Main" -> StatementListMainFunction
  | "statement_list_data_block" -> StatementListDataBlock
  | "statement_list_version" -> StatementListVersion
  | "statement_list_var_input" -> StatementListVarInput
  | "statement_list_var_inout" -> StatementListVarInout
  | "statement_list_var_output" -> StatementListVarOutput
  | "statement_list_var_constant" -> StatementListVarConstant
  | "statement_list_var_temp" -> StatementListVarTemp
  | "statement_list_var_static" -> StatementListVarStatic
  | "statement_list_return" -> StatementListReturn
  | "Ret_Val" -> StatementListReturnValueId
  | "statement_list_var_entry" -> StatementListVarEntry
  | "statement_list_var_decls" -> StatementListVarDecls
  | "statement_list_network" -> StatementListNetwork
  | "statement_list_networks" -> StatementListNetworks
  | "statement_list_title" -> StatementListTitle
  | "statement_list_identifier" -> StatementListIdentifier
  | "statement_list_load" -> StatementListLoad
  | "statement_list_transfer" -> StatementListTransfer
  | "statement_list_call" -> StatementListCall
  | "statement_list_nop" -> StatementListNop
  | "statement_list_const_add" -> StatementListConstAdd
  | "statement_list_accu_int_add" -> StatementListAccuIntAdd
  | "statement_list_accu_int_sub" -> StatementListAccuIntSub
  | "statement_list_accu_int_mul" -> StatementListAccuIntMul
  | "statement_list_accu_int_div" -> StatementListAccuIntDiv
  | "statement_list_accu_int_eq" -> StatementListAccuIntEq
  | "statement_list_accu_int_neq" -> StatementListAccuIntNeq
  | "statement_list_accu_int_gt" -> StatementListAccuIntGt
  | "statement_list_accu_int_lt" -> StatementListAccuIntLt
  | "statement_list_accu_int_gte" -> StatementListAccuIntGte
  | "statement_list_accu_int_lte" -> StatementListAccuIntLte
  | "statement_list_accu_real_add" -> StatementListAccuRealAdd
  | "statement_list_accu_real_sub" -> StatementListAccuRealSub
  | "statement_list_accu_real_mul" -> StatementListAccuRealMul
  | "statement_list_accu_real_div" -> StatementListAccuRealDiv
  | "statement_list_accu_real_eq" -> StatementListAccuRealEq
  | "statement_list_accu_real_neq" -> StatementListAccuRealNeq
  | "statement_list_accu_real_gt" -> StatementListAccuRealGt
  | "statement_list_accu_real_lt" -> StatementListAccuRealLt
  | "statement_list_accu_real_gte" -> StatementListAccuRealGte
  | "statement_list_accu_real_lte" -> StatementListAccuRealLte
  | "statement_list_accu_dint_add" -> StatementListAccuDintAdd
  | "statement_list_accu_dint_sub" -> StatementListAccuDintSub
  | "statement_list_accu_dint_mul" -> StatementListAccuDintMul
  | "statement_list_accu_dint_div" -> StatementListAccuDintDiv
  | "statement_list_accu_dint_eq" -> StatementListAccuDintEq
  | "statement_list_accu_dint_neq" -> StatementListAccuDintNeq
  | "statement_list_accu_dint_gt" -> StatementListAccuDintGt
  | "statement_list_accu_dint_lt" -> StatementListAccuDintLt
  | "statement_list_accu_dint_gte" -> StatementListAccuDintGte
  | "statement_list_accu_dint_lte" -> StatementListAccuDintLte
  | "statement_list_and" -> StatementListAnd
  | "statement_list_and_not" -> StatementListAndNot
  | "statement_list_or" -> StatementListOr
  | "statement_list_or_not" -> StatementListOrNot
  | "statement_list_xor" -> StatementListXor
  | "statement_list_xor_not" -> StatementListXorNot
  | "statement_list_and_nested" -> StatementListAndNested
  | "statement_list_and_not_nested" -> StatementListAndNotNested
  | "statement_list_or_nested" -> StatementListOrNested
  | "statement_list_or_not_nested" -> StatementListOrNotNested
  | "statement_list_xor_nested" -> StatementListXorNested
  | "statement_list_xor_not_nested" -> StatementListXorNotNested
  | "statement_list_nesting_closed" -> StatementListNestingClosed
  | "statement_list_assign" -> StatementListAssign
  | "statement_list_set_rlo" -> StatementListSetRlo
  | "statement_list_clr_rlo" -> StatementListClrRlo
  | "statement_list_set" -> StatementListSet
  | "statement_list_reset" -> StatementListReset
  | "statement_list_not" -> StatementListNot
  | "statement_list_instruction" -> StatementListInstruction
  | "statement_list_instructions" -> StatementListInstructions
  | s -> FreeformString s

let to_string str =
  match str with
  | EmptyString -> ""
  | Let -> "let"
  | LetBinding -> "let_binding"
  | Type -> "type"
  | Nil -> "nil"
  | Bool -> "bool"
  | CBool -> "c_bool"
  | ProperBool -> "proper_bool"
  | Signedbv -> "signedbv"
  | Unsignedbv -> "unsignedbv"
  | VerilogSignedbv -> "verilog_signedbv"
  | VerilogUnsignedbv -> "verilog_unsignedbv"
  | Floatbv -> "floatbv"
  | Fixedbv -> "fixedbv"
  | X86Extended -> "x86_extended"
  | CSourceLocation -> "#source_location"
  | CEndLocation -> "#end_location"
  | CIsPadding -> "#is_padding"
  | CDoNotDump -> "C_do_not_dump"
  | File -> "file"
  | Line -> "line"
  | Column -> "column"
  | Comment -> "comment"
  | Property -> "property"
  | PropertyClass -> "property_class"
  | PropertyId -> "property_id"
  | Function -> "function"
  | MathematicalFunction -> "mathematical_function"
  | Code -> "code"
  | Typecast -> "typecast"
  | StaticCast -> "static_cast"
  | DynamicCast -> "dynamic_cast"
  | ConstCast -> "const_cast"
  | ReinterpretCast -> "reinterpret_cast"
  | Index -> "index"
  | Ptrmember -> "ptrmember"
  | Member -> "member"
  | MemberName -> "member_name"
  | CMemberName -> "#member_name"
  | Equal -> "="
  | Implies -> "=>"
  | And -> "and"
  | Nand -> "nand"
  | Or -> "or"
  | Nor -> "nor"
  | Xor -> "xor"
  | Not -> "not"
  | Bitand -> "bitand"
  | Bitor -> "bitor"
  | Bitnot -> "bitnot"
  | Bitxor -> "bitxor"
  | Bitnand -> "bitnand"
  | Bitnor -> "bitnor"
  | Bitxnor -> "bitxnor"
  | Notequal -> "notequal"
  | If -> "if"
  | Symbol -> "symbol"
  | NextSymbol -> "next_symbol"
  | NondetSymbol -> "nondet_symbol"
  | PredicateSymbol -> "predicate_symbol"
  | PredicateNextSymbol -> "predicate_next_symbol"
  | NondetBool -> "nondet_bool"
  | Empty -> "empty"
  | SideEffect -> "side_effect"
  | Statement -> "statement"
  | StatementExpression -> "statement_expression"
  | Value -> "value"
  | Constant -> "constant"
  | Block -> "block"
  | Decl -> "decl"
  | Dead -> "dead"
  | Assign -> "assign"
  | AssignDiv -> "assign_div"
  | AssignMult -> "assign*"
  | AssignPlus -> "assign+"
  | AssignMinus -> "assign-"
  | AssignMod -> "assign_mod"
  | AssignShl -> "assign_shl"
  | AssignShr -> "assign_shr"
  | AssignAshr -> "assign_ashr"
  | AssignLshr -> "assign_lshr"
  | AssignBitand -> "assign_bitand"
  | AssignBitxor -> "assign_bitxor"
  | AssignBitor -> "assign_bitor"
  | Assume -> "assume"
  | Assert -> "assert"
  | Assertion -> "assertion"
  | Precondition -> "precondition"
  | Postcondition -> "postcondition"
  | PreconditionInstance -> "precondition_instance"
  | Goto -> "goto"
  | GccComputedGoto -> "gcc_computed_goto"
  | Ifthenelse -> "ifthenelse"
  | Label -> "label"
  | Break -> "break"
  | Continue -> "continue"
  | FunctionCall -> "function_call"
  | Return -> "return"
  | Skip -> "skip"
  | Arguments -> "arguments"
  | Array -> "array"
  | Size -> "size"
  | FrontendPointer -> "frontend_pointer"
  | Pointer -> "pointer"
  | BlockPointer -> "block_pointer"
  | Switch -> "switch"
  | SwitchCase -> "switch_case"
  | GccSwitchCaseRange -> "gcc_switch_case_range"
  | For -> "for"
  | While -> "while"
  | Dowhile -> "dowhile"
  | Int -> "int"
  | Integer -> "integer"
  | Natural -> "natural"
  | Real -> "real"
  | Rational -> "rational"
  | Complex -> "complex"
  | Signed -> "signed"
  | Unsigned -> "unsigned"
  | Asm -> "asm"
  | GccAsmInput -> "gcc_asm_input"
  | GccAsmOutput -> "gcc_asm_output"
  | GccAsmClobberedRegister -> "gcc_asm_clobbered_register"
  | Incomplete -> "incomplete"
  | IncompleteClass -> "incomplete_class"
  | CIncomplete -> "#incomplete"
  | Identifier -> "identifier"
  | Name -> "name"
  | InnerName -> "inner_name"
  | CppName -> "cpp_name"
  | ComponentCppName -> "component_cpp_name"
  | CIdClass -> "#id_class"
  | Declaration -> "declaration"
  | DeclarationList -> "declaration_list"
  | Declarator -> "declarator"
  | Struct -> "struct"
  | CBitField -> "c_bit_field"
  | Union -> "union"
  | Class -> "class"
  | MergedType -> "merged_type"
  | Range -> "range"
  | From -> "from"
  | To -> "to"
  | Module -> "module"
  | Parameter -> "parameter"
  | ComponentName -> "component_name"
  | ComponentNumber -> "component_number"
  | Tag -> "tag"
  | Default -> "default"
  | CDefaultValue -> "#default_value"
  | BaseName -> "base_name"
  | CBaseName -> "#base_name"
  | String -> "string"
  | CStringConstant -> "#string_constant"
  | StringConstant -> "string_constant"
  | Width -> "width"
  | Components -> "components"
  | Bv -> "bv"
  | F -> "f"
  | With -> "with"
  | Trans -> "trans"
  | Throw -> "throw"
  | TryCatch -> "try_catch"
  | Noexcept -> "noexcept"
  | CPROVERThrow -> "CPROVER_throw"
  | CPROVERTryCatch -> "CPROVER_try_catch"
  | CPROVERTryFinally -> "CPROVER_try_finally"
  | Protection -> "protection"
  | Private -> "private"
  | Public -> "public"
  | Protected -> "protected"
  | Virtual -> "virtual"
  | Volatile -> "volatile"
  | Const -> "const"
  | Constexpr -> "constexpr"
  | Inline -> "inline"
  | Forall -> "forall"
  | Exists -> "exists"
  | Repeat -> "repeat"
  | Extractbit -> "extractbit"
  | Extractbits -> "extractbits"
  | CReference -> "#reference"
  | CRvalueReference -> "#rvalue_reference"
  | True -> "true"
  | False -> "false"
  | AddressOf -> "address_of"
  | Dereference -> "dereference"
  | CLvalue -> "#lvalue"
  | CBase -> "#base"
  | Destination -> "destination"
  | Main -> "main"
  | Expression -> "expression"
  | Allocate -> "allocate"
  | CCxxAllocType -> "#cxx_alloc_type"
  | CppNew -> "cpp_new"
  | CppDelete -> "cpp_delete"
  | CppNewArray -> "cpp_new[]"
  | CppDeleteArray -> "cpp_delete[]"
  | JavaNew -> "java_new"
  | JavaNewArray -> "java_new_array"
  | JavaNewArrayData -> "java_new_array_data"
  | JavaStringLiteral -> "java_string_literal"
  | Printf -> "printf"
  | Input -> "input"
  | Output -> "output"
  | Nondet -> "nondet"
  | NULL -> "NULL"
  | Null -> "null"
  | Nullptr -> "nullptr"
  | CEnum -> "c_enum"
  | Enumeration -> "enumeration"
  | Elements -> "elements"
  | Unknown -> "unknown"
  | Uninitialized -> "uninitialized"
  | Invalid -> "invalid"
  | CInvalidObject -> "#invalid_object"
  | PointerOffset -> "pointer_offset"
  | PointerObject -> "pointer_object"
  | IsInvalidPointer -> "is_invalid_pointer"
  | IeeeFloatEqual -> "ieee_float_equal"
  | IeeeFloatNotequal -> "ieee_float_notequal"
  | Isnan -> "isnan"
  | Lambda -> "lambda"
  | ArrayComprehension -> "array_comprehension"
  | ArrayOf -> "array_of"
  | ArrayEqual -> "array_equal"
  | ArraySet -> "array_set"
  | ArrayCopy -> "array_copy"
  | ArrayList -> "array_list"
  | Mod -> "mod"
  | Rem -> "rem"
  | Shr -> "shr"
  | Ashr -> "ashr"
  | Lshr -> "lshr"
  | Shl -> "shl"
  | Rol -> "rol"
  | Ror -> "ror"
  | Comma -> "comma"
  | Concatenation -> "concatenation"
  | Infinity -> "infinity"
  | ReturnType -> "return_type"
  | Typedef -> "typedef"
  | TypedefType -> "typedef_type"
  | CTypedef -> "#typedef"
  | Extern -> "extern"
  | Static -> "static"
  | Auto -> "auto"
  | Register -> "register"
  | ThreadLocal -> "thread_local"
  | Thread -> "thread"
  | CThreadLocal -> "#thread_local"
  | CStaticLifetime -> "#static_lifetime"
  | Mutable -> "mutable"
  | Void -> "void"
  | Int8 -> "int8"
  | Int16 -> "int16"
  | Int32 -> "int32"
  | Int64 -> "int64"
  | Ptr32 -> "ptr32"
  | Ptr64 -> "ptr64"
  | Char -> "char"
  | Short -> "short"
  | Long -> "long"
  | Float -> "float"
  | Double -> "double"
  | Byte -> "byte"
  | Boolean -> "boolean"
  | LongDouble -> "long_double"
  | SignedChar -> "signed_char"
  | UnsignedChar -> "unsigned_char"
  | SignedInt -> "signed_int"
  | UnsignedInt -> "unsigned_int"
  | SignedLongInt -> "signed_long_int"
  | UnsignedLongInt -> "unsigned_long_int"
  | SignedShortInt -> "signed_short_int"
  | UnsignedShortInt -> "unsigned_short_int"
  | SignedLongLongInt -> "signed_long_long_int"
  | UnsignedLongLongInt -> "unsigned_long_long_int"
  | SignedInt128 -> "signed_int128"
  | UnsignedInt128 -> "unsigned_int128"
  | Case -> "case"
  | CInlined -> "#inlined"
  | CHide -> "#hide"
  | Hide -> "hide"
  | Abs -> "abs"
  | Sign -> "sign"
  | Access -> "access"
  | CAccess -> "#access"
  | Postincrement -> "postincrement"
  | Postdecrement -> "postdecrement"
  | Preincrement -> "preincrement"
  | Predecrement -> "predecrement"
  | IntegerBits -> "integer_bits"
  | KnR -> "KnR"
  | CKnR -> "#KnR"
  | ConstraintSelectOne -> "constraint_select_one"
  | Cond -> "cond"
  | BvLiterals -> "bv_literals"
  | IsFinite -> "isfinite"
  | Isinf -> "isinf"
  | Isnormal -> "isnormal"
  | Alignof -> "alignof"
  | ClangBuiltinConvertvector -> "clang_builtin_convertvector"
  | GccBuiltinVaArg -> "gcc_builtin_va_arg"
  | GccBuiltinTypesCompatibleP -> "gcc_builtin_types_compatible_p"
  | VaStart -> "va_start"
  | GccFloat16 -> "gcc_float16"
  | GccFloat32 -> "gcc_float32"
  | GccFloat32x -> "gcc_float32x"
  | GccFloat64 -> "gcc_float64"
  | GccFloat64x -> "gcc_float64x"
  | GccFloat80 -> "gcc_float80"
  | GccFloat128 -> "gcc_float128"
  | GccFloat128x -> "gcc_float128x"
  | GccInt128 -> "gcc_int128"
  | GccDecimal32 -> "gcc_decimal32"
  | GccDecimal64 -> "gcc_decimal64"
  | GccDecimal128 -> "gcc_decimal128"
  | BuiltinOffsetof -> "builtin_offsetof"
  | Id0 -> "0"
  | Id1 -> "1"
  | Sizeof -> "sizeof"
  | TypeArg -> "type_arg"
  | ExprArg -> "expr_arg"
  | ExpressionList -> "expression_list"
  | InitializerList -> "initializer_list"
  | GccConditionalExpression -> "gcc_conditional_expression"
  | GccLocalLabel -> "gcc_local_label"
  | Gcc -> "gcc"
  | Msc -> "msc"
  | Typeof -> "typeof"
  | Ellipsis -> "ellipsis"
  | Flavor -> "flavor"
  | Ge -> ">="
  | Le -> "<="
  | Gt -> ">"
  | Lt -> "<"
  | Plus -> "+"
  | Minus -> "-"
  | UnaryMinus -> "unary-"
  | UnaryPlus -> "unary+"
  | Mult -> "*"
  | Div -> "/"
  | Power -> "**"
  | FactorialPower -> "factorial_power"
  | PrettyName -> "pretty_name"
  | CClass -> "#class"
  | CField -> "#field"
  | CInterface -> "#interface"
  | DesignatedInitializer -> "designated_initializer"
  | Designator -> "designator"
  | MemberDesignator -> "member_designator"
  | IndexDesignator -> "index_designator"
  | CConstant -> "#constant"
  | CVolatile -> "#volatile"
  | CRestricted -> "#restricted"
  | CIdentifier -> "#identifier"
  | CImplicit -> "#implicit"
  | CPtr32 -> "#ptr32"
  | CPtr64 -> "#ptr64"
  | CAtomic -> "#atomic"
  | Restrict -> "restrict"
  | ByteExtractBigEndian -> "byte_extract_big_endian"
  | ByteExtractLittleEndian -> "byte_extract_little_endian"
  | ByteUpdateBigEndian -> "byte_update_big_endian"
  | ByteUpdateLittleEndian -> "byte_update_little_endian"
  | Replication -> "replication"
  | CproverAtomic -> "cprover_atomic"
  | Atomic -> "atomic"
  | AtomicTypeSpecifier -> "atomic_type_specifier"
  | AtomicBegin -> "atomic_begin"
  | AtomicEnd -> "atomic_end"
  | StartThread -> "start_thread"
  | EndThread -> "end_thread"
  | CoverageCriterion -> "coverage_criterion"
  | Initializer -> "initializer"
  | Anonymous -> "anonymous"
  | CIsAnonymous -> "#is_anonymous"
  | IsEnumConstant -> "is_enum_constant"
  | IsInline -> "is_inline"
  | IsExtern -> "is_extern"
  | IsSynchronized -> "is_synchronized"
  | IsNativeMethod -> "is_native_method"
  | IsVarargsMethod -> "is_varargs_method"
  | IsGlobal -> "is_global"
  | IsThreadLocal -> "is_thread_local"
  | IsParameter -> "is_parameter"
  | IsMember -> "is_member"
  | IsType -> "is_type"
  | IsRegister -> "is_register"
  | IsTypedef -> "is_typedef"
  | IsStatic -> "is_static"
  | IsTemplate -> "is_template"
  | IsStaticAssert -> "is_static_assert"
  | IsVirtual -> "is_virtual"
  | CIsVirtual -> "#is_virtual"
  | Literal -> "literal"
  | MemberInitializers -> "member_initializers"
  | MemberInitializer -> "member_initializer"
  | MethodQualifier -> "method_qualifier"
  | Methods -> "methods"
  | StaticMembers -> "static_members"
  | Constructor -> "constructor"
  | Destructor -> "destructor"
  | Bases -> "bases"
  | Base -> "base"
  | FromBase -> "from_base"
  | Operator -> "operator"
  | Template -> "template"
  | TemplateClassInstance -> "template_class_instance"
  | TemplateFunctionInstance -> "template_function_instance"
  | TemplateType -> "template_type"
  | TemplateArgs -> "template_args"
  | TemplateParameter -> "template_parameter"
  | TemplateParameterSymbolType -> "template_parameter_symbol_type"
  | TemplateParameters -> "template_parameters"
  | CTemplate -> "#template"
  | CTemplateArguments -> "#template_arguments"
  | CTemplateCase -> "#template_case"
  | Typename -> "typename"
  | C -> "C"
  | Cpp -> "cpp"
  | Java -> "java"
  | DeclBlock -> "decl_block"
  | DeclType -> "decl_type"
  | Parameters -> "parameters"
  | WcharT -> "wchar_t"
  | Char16T -> "char16_t"
  | Char32T -> "char32_t"
  | SizeT -> "size_t"
  | SsizeT -> "ssize_t"
  | Mode -> "mode"
  | This -> "this"
  | CThis -> "#this"
  | ReductionAnd -> "reduction_and"
  | ReductionOr -> "reduction_or"
  | ReductionNand -> "reduction_nand"
  | ReductionNor -> "reduction_nor"
  | ReductionXor -> "reduction_xor"
  | ReductionXnor -> "reduction_xnor"
  | CZeroInitializer -> "#zero_initializer"
  | Body -> "body"
  | TemporaryObject -> "temporary_object"
  | OverflowResultPlus -> "overflow_result-+"
  | OverflowResultMinus -> "overflow_result--"
  | OverflowResultMult -> "overflow_result-*"
  | OverflowPlus -> "overflow-+"
  | OverflowMinus -> "overflow--"
  | OverflowMult -> "overflow-*"
  | OverflowUnaryMinus -> "overflow-unary-"
  | ObjectDescriptor -> "object_descriptor"
  | IsDynamicObject -> "is_dynamic_object"
  | DynamicObject -> "dynamic_object"
  | CDynamic -> "#dynamic"
  | ObjectSize -> "object_size"
  | GoodPointer -> "good_pointer"
  | IntegerAddress -> "integer_address"
  | IntegerAddressObject -> "integer_address_object"
  | NullObject -> "NULL-object"
  | StaticObject -> "static_object"
  | StackObject -> "stack_object"
  | CIsFailedSymbol -> "#is_failed_symbol"
  | CFailedSymbol -> "#failed_symbol"
  | Friend -> "friend"
  | CFriends -> "#friends"
  | Explicit -> "explicit"
  | StorageSpec -> "storage_spec"
  | MemberSpec -> "member_spec"
  | MscDeclspec -> "msc_declspec"
  | Packed -> "packed"
  | CPacked -> "#packed"
  | TransparentUnion -> "transparent_union"
  | CTransparentUnion -> "#transparent_union"
  | Aligned -> "aligned"
  | CAlignment -> "#alignment"
  | FrontendVector -> "frontend_vector"
  | Vector -> "vector"
  | Abstract -> "abstract"
  | FunctionApplication -> "function_application"
  | CppDeclarator -> "cpp_declarator"
  | CppLinkageSpec -> "cpp_linkage_spec"
  | CppNamespaceSpec -> "cpp_namespace_spec"
  | CppStorageSpec -> "cpp_storage_spec"
  | CppUsing -> "cpp_using"
  | CppDeclaration -> "cpp_declaration"
  | CppStaticAssert -> "cpp_static_assert"
  | CppMemberSpec -> "cpp_member_spec"
  | CCType -> "#c_type"
  | Namespace -> "namespace"
  | Linkage -> "linkage"
  | Decltype -> "decltype"
  | CTagOnlyDeclaration -> "#tag_only_declaration"
  | StructTag -> "struct_tag"
  | UnionTag -> "union_tag"
  | CEnumTag -> "c_enum_tag"
  | VerilogCaseEquality -> "verilog_case_equality"
  | VerilogCaseInequality -> "verilog_case_inequality"
  | UserSpecifiedPredicate -> "user_specified_predicate"
  | UserSpecifiedParameterPredicates -> "user_specified_parameter_predicates"
  | UserSpecifiedReturnPredicates -> "user_specified_return_predicates"
  | Unassigned -> "unassigned"
  | NewObject -> "new_object"
  | ComplexReal -> "complex_real"
  | ComplexImag -> "complex_imag"
  | Imag -> "imag"
  | MscTryExcept -> "msc_try_except"
  | MscTryFinally -> "msc_try_finally"
  | MscLeave -> "msc_leave"
  | MscUuidof -> "msc_uuidof"
  | MscIfExists -> "msc_if_exists"
  | MscIfNotExists -> "msc_if_not_exists"
  | MscUnderlyingType -> "msc_underlying_type"
  | MscBased -> "msc_based"
  | Alias -> "alias"
  | PtrObject -> "ptr_object"
  | CCSizeofType -> "#c_sizeof_type"
  | ArrayUpdate -> "array_update"
  | Update -> "update"
  | StaticAssert -> "static_assert"
  | GccAttributeMode -> "gcc_attribute_mode"
  | BuiltIn -> "<built-in>"
  | ExceptionList -> "exception_list"
  | ExceptionId -> "exception_id"
  | PredicatePassiveSymbol -> "predicate_passive_symbol"
  | CwVaArgTypeof -> "cw_va_arg_typeof"
  | Fence -> "fence"
  | Sync -> "sync"
  | Lwsync -> "lwsync"
  | Isync -> "isync"
  | WRfence -> "WRfence"
  | RRfence -> "RRfence"
  | RWfence -> "RWfence"
  | WWfence -> "WWfence"
  | RRcumul -> "RRcumul"
  | RWcumul -> "RWcumul"
  | WWcumul -> "WWcumul"
  | WRcumul -> "WRcumul"
  | GenericSelection -> "generic_selection"
  | GenericAssociations -> "generic_associations"
  | GenericAssociation -> "generic_association"
  | FloatbvPlus -> "floatbv_plus"
  | FloatbvMinus -> "floatbv_minus"
  | FloatbvMult -> "floatbv_mult"
  | FloatbvDiv -> "floatbv_div"
  | FloatbvRem -> "floatbv_rem"
  | FloatbvTypecast -> "floatbv_typecast"
  | CompoundLiteral -> "compound_literal"
  | CustomBv -> "custom_bv"
  | CustomUnsignedbv -> "custom_unsignedbv"
  | CustomSignedbv -> "custom_signedbv"
  | CustomFixedbv -> "custom_fixedbv"
  | CustomFloatbv -> "custom_floatbv"
  | CSSASymbol -> "#SSA_symbol"
  | L0 -> "L0"
  | L1 -> "L1"
  | L2 -> "L2"
  | L1ObjectIdentifier -> "L1_object_identifier"
  | AlreadyTypechecked -> "already_typechecked"
  | CVaArgType -> "#va_arg_type"
  | Smt2Symbol -> "smt2_symbol"
  | Onehot -> "onehot"
  | Onehot0 -> "onehot0"
  | Popcount -> "popcount"
  | CountLeadingZeros -> "count_leading_zeros"
  | CountTrailingZeros -> "count_trailing_zeros"
  | FunctionType -> "function_type"
  | Noreturn -> "noreturn"
  | CNoreturn -> "#noreturn"
  | Weak -> "weak"
  | IsWeak -> "is_weak"
  | Used -> "used"
  | IsUsed -> "is_used"
  | CSpecLoopInvariant -> "#spec_loop_invariant"
  | CSpecRequires -> "#spec_requires"
  | CSpecEnsures -> "#spec_ensures"
  | VirtualFunction -> "virtual_function"
  | ElementType -> "element_type"
  | WorkingDirectory -> "working_directory"
  | Section -> "section"
  | Bswap -> "bswap"
  | BitReverse -> "bitreverse"
  | JavaBytecodeIndex -> "java_bytecode_index"
  | JavaInstanceof -> "java_instanceof"
  | JavaSuperMethodCall -> "java_super_method_call"
  | JavaEnumStaticUnwind -> "java_enum_static_unwind"
  | PushCatch -> "push_catch"
  | PopCatch -> "pop_catch"
  | ExceptionLandingpad -> "exception_landingpad"
  | LengthUpperBound -> "length_upper_bound"
  | CproverAssociateArrayToPointerFunc ->
      "cprover_associate_array_to_pointer_func"
  | CproverAssociateLengthToArrayFunc ->
      "cprover_associate_length_to_array_func"
  | CproverCharLiteralFunc -> "cprover_char_literal_func"
  | CproverStringLiteralFunc -> "cprover_string_literal_func"
  | CproverStringCharAtFunc -> "cprover_string_char_at_func"
  | CproverStringCharSetFunc -> "cprover_string_char_set_func"
  | CproverStringCodePointAtFunc -> "cprover_string_code_point_at_func"
  | CproverStringCodePointBeforeFunc -> "cprover_string_code_point_before_func"
  | CproverStringCodePointCountFunc -> "cprover_string_code_point_count_func"
  | CproverStringOffsetByCodePointFunc ->
      "cprover_string_offset_by_code_point_func"
  | CproverStringCompareToFunc -> "cprover_string_compare_to_func"
  | CproverStringConcatFunc -> "cprover_string_concat_func"
  | CproverStringConcatCharFunc -> "cprover_string_concat_char_func"
  | CproverStringConcatCodePointFunc -> "cprover_string_concat_code_point_func"
  | CproverStringConstrainCharactersFunc ->
      "cprover_string_constrain_characters_func"
  | CproverStringContainsFunc -> "cprover_string_contains_func"
  | CproverStringCopyFunc -> "cprover_string_copy_func"
  | CproverStringDeleteFunc -> "cprover_string_delete_func"
  | CproverStringDeleteCharAtFunc -> "cprover_string_delete_char_at_func"
  | CproverStringEqualFunc -> "cprover_string_equal_func"
  | CproverStringEqualsIgnoreCaseFunc ->
      "cprover_string_equals_ignore_case_func"
  | CproverStringEmptyStringFunc -> "cprover_string_empty_string_func"
  | CproverStringEndswithFunc -> "cprover_string_endswith_func"
  | CproverStringFormatFunc -> "cprover_string_format_func"
  | CproverStringIndexOfFunc -> "cprover_string_index_of_func"
  | CproverStringInsertFunc -> "cprover_string_insert_func"
  | CproverStringIsPrefixFunc -> "cprover_string_is_prefix_func"
  | CproverStringIsSuffixFunc -> "cprover_string_is_suffix_func"
  | CproverStringIsEmptyFunc -> "cprover_string_is_empty_func"
  | CproverStringLastIndexOfFunc -> "cprover_string_last_index_of_func"
  | CproverStringLengthFunc -> "cprover_string_length_func"
  | CproverStringOfIntFunc -> "cprover_string_of_int_func"
  | CproverStringOfIntHexFunc -> "cprover_string_of_int_hex_func"
  | CproverStringOfLongFunc -> "cprover_string_of_long_func"
  | CproverStringOfFloatFunc -> "cprover_string_of_float_func"
  | CproverStringOfFloatScientificNotationFunc ->
      "cprover_string_of_float_scientific_notation_func"
  | CproverStringOfDoubleFunc -> "cprover_string_of_double_func"
  | CproverStringParseIntFunc -> "cprover_string_parse_int_func"
  | CproverStringIsValidIntFunc -> "cprover_string_is_valid_int_func"
  | CproverStringIsValidLongFunc -> "cprover_string_is_valid_long_func"
  | CproverStringReplaceFunc -> "cprover_string_replace_func"
  | CproverStringSetLengthFunc -> "cprover_string_set_length_func"
  | CproverStringStartswithFunc -> "cprover_string_startswith_func"
  | CproverStringSubstringFunc -> "cprover_string_substring_func"
  | CproverStringToLowerCaseFunc -> "cprover_string_to_lower_case_func"
  | CproverStringToUpperCaseFunc -> "cprover_string_to_upper_case_func"
  | CproverStringTrimFunc -> "cprover_string_trim_func"
  | SkipInitialize -> "skip_initialize"
  | BasicBlockCoveredLines -> "basic_block_covered_lines"
  | BasicBlockSourceLines -> "basic_block_source_lines"
  | IsNondetNullable -> "is_nondet_nullable"
  | ArrayReplace -> "array_replace"
  | SwitchCaseNumber -> "switch_case_number"
  | JavaArrayAccess -> "java_array_access"
  | JavaMemberAccess -> "java_member_access"
  | CJavaGenericParameter -> "#java_generic_parameter"
  | CJavaGenericsClassType -> "#java_generics_class_type"
  | CJavaImplicitlyGenericClassType -> "#java_implicitly_generic_class_type"
  | CJavaGenericSymbol -> "#java_generic_symbol"
  | GenericTypes -> "generic_types"
  | ImplicitGenericTypes -> "#implicit_generic_types"
  | TypeVariables -> "type_variables"
  | HandleType -> "handle_type"
  | JavaLambdaMethodHandle -> "java_lambda_method_handle"
  | JavaLambdaMethodHandleIndex -> "lambda_method_handle_index"
  | JavaLambdaMethodHandles -> "lambda_method_handles"
  | HavocObject -> "havoc_object"
  | OverflowShl -> "overflow-shl"
  | CNoInitializationRequired -> "#no_initialization_required"
  | CNoNondetInitialization -> "#no_nondet_initialization"
  | OverlayClass -> "java::org.cprover.OverlayClassImplementation"
  | OverlayMethod -> "java::org.cprover.OverlayMethodImplementation"
  | IgnoredMethod -> "java::org.cprover.IgnoredMethodImplementation"
  | IsAnnotation -> "is_annotation"
  | CAnnotations -> "#annotations"
  | Final -> "final"
  | BitsPerByte -> "bits_per_byte"
  | CAbstract -> "#abstract"
  | Synthetic -> "synthetic"
  | Interface -> "interface"
  | CMustNotThrow -> "#must_not_throw"
  | IsInnerClass -> "is_inner_class"
  | IsAnonymous -> "is_anonymous"
  | OuterClass -> "outer_class"
  | IsBridgeMethod -> "is_bridge_method"
  | CIsOperator -> "#is_operator"
  | CNotAccessible -> "#not_accessible"
  | COverrideConstantness -> "#override_constantness"
  | CBound -> "#bound"
  | CBoundsCheck -> "#bounds_check"
  | CIsStatic -> "#is_static"
  | CCallByValue -> "#call_by_value"
  | CVirtualName -> "#virtual_name"
  | CUnnamedObject -> "#unnamed_object"
  | CTemporaryAvoided -> "#temporary_avoided"
  | CQualifier -> "#qualifier"
  | CArrayIni -> "#array_ini"
  | ROk -> "r_ok"
  | WOk -> "w_ok"
  | SuperClass -> "super_class"
  | ExceptionsThrownList -> "exceptions_thrown_list"
  | CJavaMethodType -> "#java_method_type"
  | Compiled -> "compiled"
  | PartialSpecializationArgs -> "partial_specialization_args"
  | SpecializationOf -> "specialization_of"
  | InitArgs -> "init_args"
  | Ambiguous -> "ambiguous"
  | SpecializationTemplateArgs -> "specialization_template_args"
  | FullTemplateArgs -> "full_template_args"
  | InstantiatedWith -> "instantiated_with"
  | TemplateMethods -> "template_methods"
  | CppNotTypechecked -> "cpp_not_typechecked"
  | Noaccess -> "noaccess"
  | IsOperator -> "is_operator"
  | IsCastOperator -> "is_cast_operator"
  | IsExplicit -> "is_explicit"
  | IsMutable -> "is_mutable"
  | VirtualName -> "virtual_name"
  | IsPureVirtual -> "is_pure_virtual"
  | IsVtptr -> "is_vtptr"
  | Prefix -> "prefix"
  | Cv -> "cv"
  | CppDummyDestructor -> "cpp_dummy_destructor"
  | CastExpression -> "cast_expression"
  | PodConstructor -> "pod_constructor"
  | TemplateDecls -> "template_decls"
  | ThrowDecl -> "throw_decl"
  | Typeid -> "typeid"
  | CQuoted -> "#quoted"
  | ToMember -> "to_member"
  | PointerToMember -> "pointer_to_member"
  | Tuple -> "tuple"
  | FunctionBody -> "function_body"
  | GetMay -> "get_may"
  | SetMay -> "set_may"
  | ClearMay -> "clear_may"
  | GetMust -> "get_must"
  | SetMust -> "set_must"
  | ClearMust -> "clear_must"
  | Pragma -> "pragma"
  | StatementList -> "Statement List"
  | StatementListType -> "statement_list_type"
  | StatementListFunction -> "statement_list_function"
  | StatementListFunctionBlock -> "statement_list_function_block"
  | StatementListMainFunction -> "Main"
  | StatementListDataBlock -> "statement_list_data_block"
  | StatementListVersion -> "statement_list_version"
  | StatementListVarInput -> "statement_list_var_input"
  | StatementListVarInout -> "statement_list_var_inout"
  | StatementListVarOutput -> "statement_list_var_output"
  | StatementListVarConstant -> "statement_list_var_constant"
  | StatementListVarTemp -> "statement_list_var_temp"
  | StatementListVarStatic -> "statement_list_var_static"
  | StatementListReturn -> "statement_list_return"
  | StatementListReturnValueId -> "Ret_Val"
  | StatementListVarEntry -> "statement_list_var_entry"
  | StatementListVarDecls -> "statement_list_var_decls"
  | StatementListNetwork -> "statement_list_network"
  | StatementListNetworks -> "statement_list_networks"
  | StatementListTitle -> "statement_list_title"
  | StatementListIdentifier -> "statement_list_identifier"
  | StatementListLoad -> "statement_list_load"
  | StatementListTransfer -> "statement_list_transfer"
  | StatementListCall -> "statement_list_call"
  | StatementListNop -> "statement_list_nop"
  | StatementListConstAdd -> "statement_list_const_add"
  | StatementListAccuIntAdd -> "statement_list_accu_int_add"
  | StatementListAccuIntSub -> "statement_list_accu_int_sub"
  | StatementListAccuIntMul -> "statement_list_accu_int_mul"
  | StatementListAccuIntDiv -> "statement_list_accu_int_div"
  | StatementListAccuIntEq -> "statement_list_accu_int_eq"
  | StatementListAccuIntNeq -> "statement_list_accu_int_neq"
  | StatementListAccuIntGt -> "statement_list_accu_int_gt"
  | StatementListAccuIntLt -> "statement_list_accu_int_lt"
  | StatementListAccuIntGte -> "statement_list_accu_int_gte"
  | StatementListAccuIntLte -> "statement_list_accu_int_lte"
  | StatementListAccuRealAdd -> "statement_list_accu_real_add"
  | StatementListAccuRealSub -> "statement_list_accu_real_sub"
  | StatementListAccuRealMul -> "statement_list_accu_real_mul"
  | StatementListAccuRealDiv -> "statement_list_accu_real_div"
  | StatementListAccuRealEq -> "statement_list_accu_real_eq"
  | StatementListAccuRealNeq -> "statement_list_accu_real_neq"
  | StatementListAccuRealGt -> "statement_list_accu_real_gt"
  | StatementListAccuRealLt -> "statement_list_accu_real_lt"
  | StatementListAccuRealGte -> "statement_list_accu_real_gte"
  | StatementListAccuRealLte -> "statement_list_accu_real_lte"
  | StatementListAccuDintAdd -> "statement_list_accu_dint_add"
  | StatementListAccuDintSub -> "statement_list_accu_dint_sub"
  | StatementListAccuDintMul -> "statement_list_accu_dint_mul"
  | StatementListAccuDintDiv -> "statement_list_accu_dint_div"
  | StatementListAccuDintEq -> "statement_list_accu_dint_eq"
  | StatementListAccuDintNeq -> "statement_list_accu_dint_neq"
  | StatementListAccuDintGt -> "statement_list_accu_dint_gt"
  | StatementListAccuDintLt -> "statement_list_accu_dint_lt"
  | StatementListAccuDintGte -> "statement_list_accu_dint_gte"
  | StatementListAccuDintLte -> "statement_list_accu_dint_lte"
  | StatementListAnd -> "statement_list_and"
  | StatementListAndNot -> "statement_list_and_not"
  | StatementListOr -> "statement_list_or"
  | StatementListOrNot -> "statement_list_or_not"
  | StatementListXor -> "statement_list_xor"
  | StatementListXorNot -> "statement_list_xor_not"
  | StatementListAndNested -> "statement_list_and_nested"
  | StatementListAndNotNested -> "statement_list_and_not_nested"
  | StatementListOrNested -> "statement_list_or_nested"
  | StatementListOrNotNested -> "statement_list_or_not_nested"
  | StatementListXorNested -> "statement_list_xor_nested"
  | StatementListXorNotNested -> "statement_list_xor_not_nested"
  | StatementListNestingClosed -> "statement_list_nesting_closed"
  | StatementListAssign -> "statement_list_assign"
  | StatementListSetRlo -> "statement_list_set_rlo"
  | StatementListClrRlo -> "statement_list_clr_rlo"
  | StatementListSet -> "statement_list_set"
  | StatementListReset -> "statement_list_reset"
  | StatementListNot -> "statement_list_not"
  | StatementListInstruction -> "statement_list_instruction"
  | StatementListInstructions -> "statement_list_instructions"
  | FreeformString s -> s

let show = to_string
let pp = Fmt.of_to_string to_string

let of_yojson = function
  | `String x -> of_string x
  | j -> Kutils.J.parse_error j "Id is not a string"

let to_int_opt = function
  | Id0 -> Some 0
  | Id1 -> Some 1
  | FreeformString s -> int_of_string_opt s
  | _ -> None

let to_int s =
  match to_int_opt s with
  | Some i -> i
  | None -> raise (Invalid_argument "Invalid int id")

let to_z = function
  | Id0 -> Z.zero
  | Id1 -> Z.one
  | FreeformString s -> Z.of_string s
  | _ -> raise (Invalid_argument "Invalid Z id")

let to_z_opt s = try Some (to_z s) with Invalid_argument _ -> None

let to_bitpattern ~width ~signed id =
  let bits = Kutils.z_of_hex (to_string id) in
  if signed then Z.signed_extract bits 0 width else bits
