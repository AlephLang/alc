type kind =
  | Root of { toplevel_statements: t list }
  | Expr of { lhs: t; operator: exprOperator; rhs: t }
  | PrefixExpr of { operators: exprOperator list; operand: t }
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
  | ExprOperandNumber of { value: int }
  | ExprOperandNumberFloat of { value: float }
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
and exprOperator =
  | BinaryAdd | BinarySub | BinaryMul | BinaryDiv | BinaryMod | BinaryShl | BinaryShr | BinaryAnd
  | BinaryOr | BinaryXor
  | CompareEq | CompareNotEq | CompareLThan | CompareGThan | CompareLThanEq | CompareGThanEq
  | BooleanAnd | BooleanOr
  | AssignEq | AssignAddEq | AssignSubEq | AssignMulEq | AssignDivEq | AssignModEq | AssignShlEq
  | AssignShrEq | AssignAndEq | AssignOrEq | AssignXorEq
  | PrefixNot | PrefixBooleanNot | PrefixNegative | PrefixDereference | PrefixAddress

let show ast =
  let open Printf in

  let buildtree header children =
    let outstrs = ref [header] in
    let children_len = List.length children in
    for i = 0 to children_len - 1 do
      let child = List.nth children i in
      let child_len = List.length child in
      for j = 0 to child_len - 1 do
        let prefix =
          if j = 0 then
            if i = children_len - 1 then
              "╰─"
            else
              "├─"
          else if i < children_len - 1 then
            "│ "
          else
            "  " in
        outstrs := !outstrs @ [prefix ^ List.nth child j];
      done;
    done;
    !outstrs in

  let expr_operator_to_string op =
    match op with
    | BinaryAdd -> "+"
    | BinarySub -> "-"
    | BinaryMul -> "*"
    | BinaryDiv -> "/"
    | BinaryMod -> "%"
    | BinaryShl -> "<<"
    | BinaryShr -> ">>"
    | BinaryAnd -> "&"
    | BinaryOr -> "|"
    | BinaryXor -> "^"
    | CompareEq -> "=="
    | CompareNotEq -> "!="
    | CompareLThan -> "<"
    | CompareGThan -> ">"
    | CompareLThanEq -> "<="
    | CompareGThanEq -> ">="
    | BooleanAnd -> "&&"
    | BooleanOr -> "||"
    | AssignEq -> "="
    | AssignAddEq -> "+="
    | AssignSubEq -> "-="
    | AssignMulEq -> "*="
    | AssignDivEq -> "/="
    | AssignModEq -> "%="
    | AssignShlEq -> "<<="
    | AssignShrEq -> ">>="
    | AssignAndEq -> "&="
    | AssignOrEq -> "|="
    | AssignXorEq -> "^="
    | PrefixNot -> "~"
    | PrefixBooleanNot -> "!"
    | PrefixNegative -> "-"
    | PrefixDereference -> "*"
    | PrefixAddress -> "&" in

  let rec to_string ast =
    let option_to_string x =
      match x with
      | Some x -> [to_string x]
      | None -> [] in
    let rec get_xs_of asts =
      match asts with
      | [] -> []
      | x :: xs -> [to_string x] @ get_xs_of xs in

    match ast.kind with
    | Root { toplevel_statements } -> buildtree "Root" @@ get_xs_of toplevel_statements
    | Expr { lhs; operator; rhs } ->
        buildtree
        (sprintf "Expr { op: %s }" @@ expr_operator_to_string operator)
        @@ [to_string lhs; to_string rhs]
    | PrefixExpr { operators; operand } ->
        buildtree
        (sprintf "PrefixExpr { ops: %s }"
         @@ let rec _ops_to_str ops =
           match ops with
           | [] -> ""
           | x :: xs -> expr_operator_to_string x ^ _ops_to_str xs in
         _ops_to_str operators)
        [to_string operand]
    | Module { name; subm } ->
        (let header = sprintf "Module { name: \"%s\" }" name in
         match subm with
         | None -> [header]
         | Some x -> buildtree header [to_string x])
    | Import { m } -> buildtree "Import" [to_string m]
    | TypeDef { name; generic_type_list; aliased_type; attribute_list } ->
        buildtree
        (sprintf "TypeDef { name: \"%s\" }" name)
        @@ option_to_string generic_type_list
        @ option_to_string attribute_list
        @ [to_string aliased_type]
    | ExternFunc { name; argument_list; return_type } ->
        buildtree
        (sprintf "ExternFunc { name: \"%s\" }" name)
        @@ [to_string argument_list] @ option_to_string return_type
    | ExternVarDecl { name; type_ } ->
        buildtree
        (sprintf "ExternVarDecl { name: \"%s\" }" name)
        @@ [to_string type_]
    | Qualifier { name; qualified } ->
        buildtree
        (sprintf "Qualifier { name: \"%s\" }" name)
        @@ [to_string qualified]
    | None -> ["None"]
    | Variadic -> ["Variadic"]
    | Struct { name; children; attribute_list } ->
        buildtree
        (sprintf "Struct { name: \"%s\" }" name)
        @@ option_to_string attribute_list
        @ get_xs_of children
    | Union { name; children; attribute_list } ->
        buildtree
        (sprintf "Union { name: \"%s\" }" name)
        @@ option_to_string attribute_list
        @ get_xs_of children
    | Enum { name; elements; attribute_list } ->
        buildtree
        (sprintf "Enum { name: \"%s\" }" name)
        @@ option_to_string attribute_list
        @ get_xs_of elements
    | EnumElement { name; expression } ->
        buildtree
        (sprintf "EnumElement { name: \"%s\" }" name)
        @@ option_to_string expression
    | Func {  name; argument_list; return_type; body; attribute_list } ->
        buildtree
        (sprintf "Func { name: \"%s\" }" name)
        @@ option_to_string attribute_list
        @ [to_string argument_list]
        @ option_to_string return_type
        @ [to_string body]
    | ArgumentList { arguments } -> buildtree "ArgumentList" @@ get_xs_of arguments
    | Namespace { name; subobject } ->
        buildtree
        (sprintf "Namespace { name: \"%s\" }" name)
        [to_string subobject]
    | VisibilityMarker { name } -> [sprintf "VisibilityMarker { name: \"%s\" }" name]
    | Attribute { name; arguments } ->
        buildtree
        (sprintf "Attribute { name: \"%s\" }" name)
        @@ get_xs_of arguments
    | AttributeList { attributes } -> buildtree "AttributeList" @@ get_xs_of attributes
    | StmtBlock { statements } -> buildtree "StmtBlock" @@ get_xs_of statements
    | StmtReturn { expression } -> buildtree "StmtReturn" @@ option_to_string expression
    | StmtGoto { label_name } -> [sprintf "StmtGoto { label_name: \"%s\" }" label_name]
    | StmtLabel { name } -> [sprintf "StmtLabel { name: \"%s\" }" name]
    | StmtBreak -> ["StmtBreak"]
    | StmtContinue -> ["StmtContinue"]
    | StmtFallthrough -> ["StmtFallthrough"]
    | StmtWhile { condition; body; attribute_list } ->
        buildtree "StmtWhile"
        @@ option_to_string attribute_list
        @ [to_string condition; to_string body]
    | StmtFor { init_statement; condition; advance_statement; body; attribute_list } ->
        buildtree "StmtFor"
        @@ option_to_string attribute_list
        @ option_to_string init_statement
        @ option_to_string condition
        @ option_to_string advance_statement
        @ [to_string body]
    | StmtDoWhile { condition; body; attribute_list } ->
        buildtree "StmtDoWhile"
        @@ option_to_string attribute_list
        @ [to_string condition; to_string body]
    | StmtLoop { body } -> buildtree "StmtLoop" [to_string body]
    | StmtExpr { expression } -> buildtree "StmtExpr" [to_string expression]
    | StmtSwitch { expression; cases } ->
        buildtree "StmtSwitch" @@ [to_string expression] @ get_xs_of cases
    | StmtCase { expression; body } ->
        buildtree "StmtCase" [to_string expression; to_string body]
    | StmtDefault { body } -> buildtree "StmtDefault" [to_string body]
    | StmtDefer { body } -> buildtree "StmtDefer" [to_string body]
    | StmtIf { condition; body; else_statement; attribute_list } ->
        buildtree "StmtIf"
        @@ option_to_string attribute_list
        @ [to_string condition; to_string body]
        @ option_to_string else_statement
    | StmtElse { body } -> buildtree "StmtElse" [to_string body]
    | TypePlain { name } -> [sprintf "TypePlain { name: \"%s\" }" name]
    | TypePointer { type_ } -> buildtree "TypePointer" [to_string type_]
    | TypeArray { type_; size_expression } ->
        buildtree "TypePointer" @@ [to_string type_] @ option_to_string size_expression
    | TypeFunctionPointer { argument_list; return_type } ->
        buildtree "TypeFunctionPointer" @@ [to_string argument_list] @ option_to_string return_type
    | TypeTypeOf { expression } -> buildtree "TypeTypeOf" [to_string expression]
    | VarDecl { name; type_; attribute_list } ->
        buildtree
        (sprintf "VarDecl { name: \"%s\" }" name)
        @@ option_to_string attribute_list @ [to_string type_]
    | VarDef { name; type_; expression; attribute_list } ->
        buildtree
        (sprintf "VarDef { name: \"%s\" }" name)
        @@ option_to_string attribute_list @ option_to_string type_ @ [to_string expression]
    | ExprOperandVoid -> ["ExprOperandVoid"]
    | ExprOperandIdentifier { name } -> [sprintf "ExprOperandIdentifier { name: \"%s\" }" name]
    | ExprOperandNumber { value } -> [sprintf "ExprOperandNumber { value: %i }" value]
    | ExprOperandNumberFloat { value } -> [sprintf "ExprOperandNumberFloat { value: %f }" value]
    | ExprOperandArrayElement { array; index_expression } ->
        buildtree "ExprOperandArrayElement" [to_string array; to_string index_expression]
    | ExprOperandCastTo { type_; expression } ->
        buildtree "ExprOperandCastTo" [to_string type_; to_string expression]
    | ExprOperandCall { callee_name; arguments } ->
        buildtree
        (sprintf "ExprOperandCall { callee_name: \"%s\" }" callee_name)
        @@ get_xs_of arguments
    | ExprOperandGenericCall { callee_name; generic_type_list; arguments } ->
        buildtree
        (sprintf "ExprOperandCall { callee_name: \"%s\" }" callee_name)
        @@ [to_string generic_type_list] @ get_xs_of arguments
    | ExprOperandString { contents } -> [sprintf "ExprOperandString { contents: \"%s\" }" contents]
    | ExprOperandSymbol { contents } -> [sprintf "ExprOperandSymbol { contents: '%s' }" contents]
    | ExprOperandAccessMember { from; what } ->
        buildtree "ExprOperandAccessMember" [to_string from; to_string what]
    | ExprOperandSizeOf { type_ } -> buildtree "ExprOperandSizeOf" [to_string type_]
    | ExprOperandAlignOf { expression } -> buildtree "ExprOperandAlignOf" [to_string expression]
    | InitList { entries } -> buildtree "InitList" @@ get_xs_of entries
    | InitListEntry { expression } -> buildtree "InitListEntry" [to_string expression]
    | InitListEntryExplicit { name; expression } ->
        buildtree (sprintf "InitListEntryExplicit { name: \"%s\" }" name) [to_string expression]
    | InitListEntryExplicitArrayElement { index_expression; expression } ->
        buildtree
        "InitListEntryExplicitArrayElement"
        [to_string index_expression; to_string expression]
    | GenericStruct { name; generic_placeholder_type_list; children; attribute_list } ->
        buildtree
        (sprintf "GenericStruct { name: \"%s\" }" name)
        @@ option_to_string attribute_list
        @ [to_string generic_placeholder_type_list]
        @ get_xs_of children
    | GenericFunc { name; generic_placeholder_type_list;
                    argument_list; return_type; body; attribute_list } ->
        buildtree
        (sprintf "GenericFunc { name: \"%s\" }" name)
        @@ option_to_string attribute_list
        @ [to_string generic_placeholder_type_list; to_string argument_list]
        @ option_to_string return_type
        @ [to_string body]
    | GenericPlaceholderTypeList { generic_placeholder_types } ->
        buildtree "GenericPlaceholderTypeList" @@ get_xs_of generic_placeholder_types
    | GenericPlaceholderType { name; default_type } ->
        buildtree
        (sprintf "GenericPlaceholderType { name: \"%s\" }" name)
        @@ option_to_string default_type
    | GenericTypeList { types } -> buildtree "GenericTypeList" @@ get_xs_of types
    | GenericType { name; generic_type_list } ->
        buildtree (sprintf "GenericType { name: \"%s\" }" name) [to_string generic_type_list]
    | GenericNamespace { name; generic_type_list; subobject } ->
        buildtree
        (sprintf "GenericNamespace { name: \"%s\" }" name)
        [to_string generic_type_list; to_string subobject] in

  let rec _strs_to_str strs =
    match strs with
    | [] -> ""
    | x :: xs -> x ^ "\n" ^ _strs_to_str xs in
  _strs_to_str @@ to_string ast
