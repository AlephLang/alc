module ExprOperator : sig
  type group = | Binary | Compare | Boolean | Assign | Prefix
  and t =
    | BinaryAdd | BinarySub | BinaryMul | BinaryDiv | BinaryMod | BinaryShl | BinaryShr | BinaryAnd
    | BinaryOr | BinaryXor
    | CompareEq | CompareNotEq | CompareLThan | CompareGThan | CompareLThanEq | CompareGThanEq
    | BooleanAnd | BooleanOr
    | AssignEq | AssignAddEq | AssignSubEq | AssignMulEq | AssignDivEq | AssignModEq | AssignShlEq
    | AssignShrEq | AssignAndEq | AssignOrEq | AssignXorEq
    | PrefixNot | PrefixBooleanNot | PrefixNegative | PrefixDereference | PrefixAddress
  val get_group : t -> group
end

type kind =
  | Root of { toplevel_statements: t list }
  | Expr of { lhs: t; operator: ExprOperator.t; rhs: t }
  | PrefixExpr of { operators: ExprOperator.t list; operand: t }
  | Module of { name: string; subm: t option }
  | Import of { m: t }
  | TypeDef of { name: string
               ; generic_type_list: t option
               ; aliased_type: t
               ; attribute_list: t option }
  | ExternFunc of { name: string; argument_list: t; return_type: t option }
  | ExternVarDecl of { name: string; type_: t }
  | Qualifier of { name: string; qualified: t }
  | None
  | Variadic
  | Struct of { name: string; children: t list; attribute_list: t option }
  | Union of { name: string; children: t list; attribute_list: t option }
  | Enum of { name: string; elements: t list; attribute_list: t option }
  | EnumElement of { name: string; expression: t option }
  | Func of { name: string
            ; argument_list: t
            ; return_type: t option
            ; body: t
            ; attribute_list: t option }
  | ArgumentList of { arguments: t list }
  | Namespace of { name: string; subobject: t }
  | VisibilityMarker of { name: string }
  | Attribute of { name: string; arguments: t list }
  | AttributeList of { attributes: t list }
  | StmtBlock of { statements: t list }
  | StmtReturn of { expression: t option }
  | StmtGoto of { label_name: string }
  | StmtLabel of { name: string }
  | StmtBreak
  | StmtContinue
  | StmtFallthrough
  | StmtWhile of { condition: t; body: t; attribute_list: t option }
  | StmtFor of { init_statement: t option
               ; condition: t option
               ; advance_statement: t option
               ; body: t
               ; attribute_list: t option }
  | StmtDoWhile of { condition: t; body: t; attribute_list: t option }
  | StmtLoop of { body: t }
  | StmtExpr of { expression: t }
  | StmtSwitch of { expression: t; cases: t list }
  | StmtCase of { expression: t; body: t }
  | StmtDefault of { body: t }
  | StmtDefer of { body: t }
  | StmtIf of { condition: t; body: t; else_statement: t option; attribute_list: t option }
  | StmtElse of { body: t }
  | TypePlain of { name: string }
  | TypePointer of { type_: t }
  | TypeArray of { type_: t; size_expression: t option }
  | TypeFunctionPointer of { argument_list: t; return_type: t option }
  | TypeTypeOf of { expression: t }
  | VarDecl of { name: string; type_: t; attribute_list: t option }
  | VarDef of { name: string; type_: t option; expression: t; attribute_list: t option }
  | ExprOperandVoid
  | ExprOperandIdentifier of { name: string }
  | ExprOperandNumber of { value: int; typespec: string option }
  | ExprOperandNumberFloat of { value: float; typespec: string option }
  | ExprOperandArrayElement of { array: t; index_expression: t }
  | ExprOperandCastTo of { type_: t; expression: t }
  | ExprOperandCall of { callee_name: string; arguments: t list }
  | ExprOperandGenericCall of { callee_name: string; generic_type_list: t; arguments: t list }
  | ExprOperandString of { contents: string }
  | ExprOperandSymbol of { contents: string }
  | ExprOperandAccessMember of { from: t; what: t }
  | ExprOperandSizeOf of { type_: t }
  | ExprOperandAlignOf of { expression: t }
  | InitList of { entries: t list }
  | InitListEntry of { expression: t }
  | InitListEntryExplicit of { name: string; expression: t }
  | InitListEntryExplicitArrayElement of { index_expression: t; expression: t }
  | GenericStruct of { name: string
                     ; generic_placeholder_type_list: t
                     ; children: t list
                     ; attribute_list: t option }
  | GenericFunc of { name: string
                   ; generic_placeholder_type_list: t
                   ; argument_list: t
                   ; return_type: t option
                   ; body: t
                   ; attribute_list: t option }
  | GenericPlaceholderTypeList of { generic_placeholder_types: t list }
  | GenericPlaceholderType of { name: string; default_type: t option }
  | GenericTypeList of { types: t list }
  | GenericType of { name: string; generic_type_list: t }
  | GenericNamespace of { name: string; generic_type_list: t; subobject: t }
and t = {
  kind: kind;
  pos: int;
}

val show : t -> string
