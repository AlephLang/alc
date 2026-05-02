type kind =
  (* toplevel statements *)
  | Root of t list

  (* lhs, operator, rhs *)
  | Expr of t * exprOperator * t

  (* prefix operators, operand *)
  | PrefixExpr of exprOperator list * t

  (* name, submodule *)
  | Module of string * t option

  (* module *)
  | Import of t

  (* name, generic type list, attribute list, aliased type *)
  | TypeDef of string * t option * t option * t

  (* name, argument list, return type *)
  | ExternFunc of string * t * t option

  (* name, type *)
  | ExternVarDecl of string * t

  (* name, qualified *)
  | Qualifier of string * t

  | None

  | Variadic

  (* name, attribute list, children *)
  | Struct of string * t option * t list

  (* name, attribute list, children *)
  | Union of string * t option * t list

  (* name, attribute list, elements *)
  | Enum of string * t option * t list

  (* name, expression *)
  | EnumElement of string * t option

  (* attribute list, name, argument list, return type, body *)
  | Func of t option * string * t * t option * t

  (* arguments *)
  | ArgumentList of t list

  (* name, subobject *)
  | Namespace of string * t

  (* name *)
  | VisibilityMarker of string

  (* name, arguments *)
  | Attribute of string * t list

  (* attributes *)
  | AttributeList of t list

  (* statements *)
  | StmtBlock of t list

  (* expression *)
  | StmtReturn of t option

  (* label name *)
  | StmtGoto of string

  (* name *)
  | StmtLabel of string

  | StmtBreak

  | StmtContinue

  | StmtFallthrough

  (* attribute list, condition, body *)
  | StmtWhile of t option * t * t

  (* attribute list, init statement, condition, advance statement, body *)
  | StmtFor of t option * t option * t option * t option * t

  (* attribute list, condition, body *)
  | StmtDoWhile of t option * t * t

  (* body *)
  | StmtLoop of t

  (* expression *)
  | StmtExpr of t

  (* expression, cases *)
  | StmtSwitch of t * t list

  (* expression, body *)
  | StmtCase of t * t

  (* body *)
  | StmtDefault of t

  (* body *)
  | StmtDefer of t

  (* attribute list, condition, body, else statement *)
  | StmtIf of t option * t * t * t option

  (* body *)
  | StmtElse of t

  (* name *)
  | TypePlain of string

  (* type *)
  | TypePointer of t

  (* type, expression *)
  | TypeArray of t * t option

  (* argument list, return type *)
  | TypeFunctionPointer of t * t option

  (* expression *)
  | TypeTypeOf of t

  (* attribute list, name, type *)
  | VarDecl of t option * string * t

  (* attribute list, name, type, expression *)
  | VarDef of t option * string * t option * t

  | ExprOperandVoid

  (* name *)
  | ExprOperandIdentifier of string

  (* integer value *)
  | ExprOperandNumber of int

  (* floating-point value *)
  | ExprOperandNumberFloat of float

  (* array, expression *)
  | ExprOperandArrayElement of t * t

  (* type, expression *)
  | ExprOperandCastTo of t * t

  (* callee name, arguments *)
  | ExprOperandCall of string * t list

  (* callee name, generic type list, arguments *)
  | ExprOperandGenericCall of string * t * t list

  (* contents *)
  | ExprOperandString of string

  (* contents *)
  | ExprOperandSymbol of string

  (* from, what *)
  | ExprOperandAccessMember of t * t

  (* type *)
  | ExprOperandSizeOf of t

  (* expresion *)
  | ExprOperandAlignOf of t

  (* entries *)
  | InitList of t list

  (* expression *)
  | InitListEntry of t

  (* name, expression *)
  | InitListEntryExplicit of string * t

  (* index expression, expression *)
  | InitListEntryExplicitArrayElement of t * t

  (* name, attribute list, generic placeholder type list, children *)
  | GenericStruct of string * t option * t * t list

  (* attribute list, name, generic placeholder type list, argument list, return type, body *)
  | GenericFunc of t option * string * t * t * t option * t

  (* generic placeholder types *)
  | GenericPlaceholderTypeList of t list

  (* name, default_type *)
  | GenericPlaceholderType of string * t option

  (* types *)
  | GenericTypeList of t list

  (* name, generic type list *)
  | GenericType of string * t

  (* name, generic type list, subobject *)
  | GenericNamespace of string * t * t
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
    | Root children -> buildtree "Root" @@ get_xs_of children
    | Expr (lhs, op, rhs) ->
        buildtree
        (sprintf "Expr { op: %s }" @@ expr_operator_to_string op)
        @@ [to_string lhs; to_string rhs]
    | PrefixExpr (ops, operand) ->
        buildtree
        (sprintf "PrefixExpr { ops: %s }"
         @@ let rec _ops_to_str operators =
           match operators with
           | [] -> ""
           | x :: xs -> expr_operator_to_string x ^ _ops_to_str xs in
         _ops_to_str ops)
        [to_string operand]
    | Module (name, subm) ->
        (let header = sprintf "Module { name: \"%s\" }" name in
         match subm with
         | None -> [header]
         | Some x -> buildtree header [to_string x])
    | Import m -> buildtree "Import" [to_string m]
    | TypeDef (name, generic_type_list, attribute_list, aliased_type) ->
        buildtree
        (sprintf "TypeDef { name: \"%s\" }" name)
        @@ option_to_string generic_type_list
        @ option_to_string attribute_list
        @ [to_string aliased_type]
    | ExternFunc (name, argument_list, return_type) ->
        buildtree
        (sprintf "ExternFunc { name: \"%s\" }" name)
        @@ [to_string argument_list] @ option_to_string return_type
    | ExternVarDecl (name, type_) ->
        buildtree
        (sprintf "ExternVarDecl { name: \"%s\" }" name)
        @@ [to_string type_]
    | Qualifier (name, qualified) ->
        buildtree
        (sprintf "Qualifier { name: \"%s\" }" name)
        @@ [to_string qualified]
    | None -> ["None"]
    | Variadic -> ["Variadic"]
    | Struct (name, attribute_list, children) ->
        buildtree
        (sprintf "Struct { name: \"%s\" }" name)
        @@ option_to_string attribute_list
        @ get_xs_of children
    | Union (name, attribute_list, children) ->
        buildtree
        (sprintf "Union { name: \"%s\" }" name)
        @@ option_to_string attribute_list
        @ get_xs_of children
    | Enum (name, attribute_list, elements) ->
        buildtree
        (sprintf "Enum { name: \"%s\" }" name)
        @@ option_to_string attribute_list
        @ get_xs_of elements
    | EnumElement (name, expression) ->
        buildtree
        (sprintf "EnumElement { name: \"%s\" }" name)
        @@ option_to_string expression
    | Func (attribute_list, name, argument_list, return_type, body) ->
        buildtree
        (sprintf "Func { name: \"%s\" }" name)
        @@ option_to_string attribute_list
        @ [to_string argument_list]
        @ option_to_string return_type
        @ [to_string body]
    | ArgumentList arguments -> buildtree "ArgumentList" @@ get_xs_of arguments
    | Namespace (name, subobject) ->
        buildtree
        (sprintf "Namespace { name: \"%s\" }" name)
        [to_string subobject]
    | VisibilityMarker name -> [sprintf "VisibilityMarker { name: \"%s\" }" name]
    | Attribute (name, arguments) ->
        buildtree
        (sprintf "Attribute { name: \"%s\" }" name)
        @@ get_xs_of arguments
    | AttributeList attributes -> buildtree "AttributeList" @@ get_xs_of attributes
    | StmtBlock statements -> buildtree "StmtBlock" @@ get_xs_of statements
    | StmtReturn expression -> buildtree "StmtReturn" @@ option_to_string expression
    | StmtGoto label_name -> [sprintf "StmtGoto { label_name: \"%s\" }" label_name]
    | StmtLabel name -> [sprintf "StmtLabel { name: \"%s\" }" name]
    | StmtBreak -> ["StmtBreak"]
    | StmtContinue -> ["StmtContinue"]
    | StmtFallthrough -> ["StmtFallthrough"]
    | StmtWhile (attribute_list, condition, body) ->
        buildtree "StmtWhile"
        @@ option_to_string attribute_list
        @ [to_string condition; to_string body]
    | StmtFor (attribute_list, init_statement, condition, advance_statement, body) ->
        buildtree "StmtFor"
        @@ option_to_string attribute_list
        @ option_to_string init_statement
        @ option_to_string condition
        @ option_to_string advance_statement
        @ [to_string body]
    | StmtDoWhile (attribute_list, condition, body) ->
        buildtree "StmtDoWhile"
        @@ option_to_string attribute_list
        @ [to_string condition; to_string body]
    | StmtLoop body -> buildtree "StmtLoop" [to_string body]
    | StmtExpr expression -> buildtree "StmtExpr" [to_string expression]
    | StmtSwitch (expression, cases) ->
        buildtree "StmtSwitch" @@ [to_string expression] @ get_xs_of cases
    | StmtCase (expression, body) ->
        buildtree "StmtCase" [to_string expression; to_string body]
    | StmtDefault body -> buildtree "StmtDefault" [to_string body]
    | StmtDefer body -> buildtree "StmtDefer" [to_string body]
    | StmtIf (attribute_list, condition, body, else_statement) ->
        buildtree "StmtIf"
        @@ option_to_string attribute_list
        @ [to_string condition; to_string body]
        @ option_to_string else_statement
    | StmtElse body -> buildtree "StmtElse" [to_string body]
    | TypePlain name -> [sprintf "TypePlain { name: \"%s\" }" name]
    | TypePointer type_ -> buildtree "TypePointer" [to_string type_]
    | TypeArray (type_, expression) ->
        buildtree "TypePointer" @@ [to_string type_] @ option_to_string expression
    | TypeFunctionPointer (argument_list, return_type) ->
        buildtree "TypeFunctionPointer" @@ [to_string argument_list] @ option_to_string return_type
    | TypeTypeOf expression -> buildtree "TypeTypeOf" [to_string expression]
    | VarDecl (attribute_list, name, type_) ->
        buildtree
        (sprintf "VarDecl { name: \"%s\" }" name)
        @@ option_to_string attribute_list @ [to_string type_]
    | VarDef (attribute_list, name, type_, expression) ->
        buildtree
        (sprintf "VarDef { name: \"%s\" }" name)
        @@ option_to_string attribute_list @ option_to_string type_ @ [to_string expression]
    | ExprOperandVoid -> ["ExprOperandVoid"]
    | ExprOperandIdentifier name -> [sprintf "ExprOperandIdentifier { name: \"%s\" }" name]
    | ExprOperandNumber value -> [sprintf "ExprOperandNumber { value: %i }" value]
    | ExprOperandNumberFloat value -> [sprintf "ExprOperandNumberFloat { value: %f }" value]
    | ExprOperandArrayElement (array, expression) ->
        buildtree "ExprOperandArrayElement" [to_string array; to_string expression]
    | ExprOperandCastTo (type_, expression) ->
        buildtree "ExprOperandCastTo" [to_string type_; to_string expression]
    | ExprOperandCall (callee_name, arguments) ->
        buildtree
        (sprintf "ExprOperandCall { callee_name: \"%s\" }" callee_name)
        @@ get_xs_of arguments
    | ExprOperandGenericCall (callee_name, generic_type_list, arguments) ->
        buildtree
        (sprintf "ExprOperandCall { callee_name: \"%s\" }" callee_name)
        @@ [to_string generic_type_list] @ get_xs_of arguments
    | ExprOperandString contents -> [sprintf "ExprOperandString { contents: \"%s\" }" contents]
    | ExprOperandSymbol contents -> [sprintf "ExprOperandSymbol { contents: '%s' }" contents]
    | ExprOperandAccessMember (from, what) ->
        buildtree "ExprOperandAccessMember" [to_string from; to_string what]
    | ExprOperandSizeOf type_ -> buildtree "ExprOperandSizeOf" [to_string type_]
    | ExprOperandAlignOf expression -> buildtree "ExprOperandAlignOf" [to_string expression]
    | InitList entries -> buildtree "InitList" @@ get_xs_of entries
    | InitListEntry expression -> buildtree "InitListEntry" [to_string expression]
    | InitListEntryExplicit (name, expression) ->
        buildtree (sprintf "InitListEntryExplicit { name: \"%s\" }" name) [to_string expression]
    | InitListEntryExplicitArrayElement (index_expression, expression) ->
        buildtree
        "InitListEntryExplicitArrayElement"
        [to_string index_expression; to_string expression]
    | GenericStruct (name, attribute_list, generic_placeholder_type_list, children) ->
        buildtree
        (sprintf "GenericStruct { name: \"%s\" }" name)
        @@ option_to_string attribute_list
        @ [to_string generic_placeholder_type_list]
        @ get_xs_of children
    | GenericFunc (attribute_list, name, generic_placeholder_type_list,
                   argument_list, return_type, body) ->
        buildtree
        (sprintf "GenericFunc { name: \"%s\" }" name)
        @@ option_to_string attribute_list
        @ [to_string generic_placeholder_type_list; to_string argument_list]
        @ option_to_string return_type
        @ [to_string body]
    | GenericPlaceholderTypeList generic_placeholder_types ->
        buildtree "GenericPlaceholderTypeList" @@ get_xs_of generic_placeholder_types
    | GenericPlaceholderType (name, default_type) ->
        buildtree
        (sprintf "GenericPlaceholderType { name: \"%s\" }" name)
        @@ option_to_string default_type
    | GenericTypeList types -> buildtree "GenericTypeList" @@ get_xs_of types
    | GenericType (name, generic_type_list) ->
        buildtree (sprintf "GenericType { name: \"%s\" }" name) [to_string generic_type_list]
    | GenericNamespace (name, generic_type_list, subobject) ->
        buildtree
        (sprintf "GenericNamespace { name: \"%s\" }" name)
        [to_string generic_type_list; to_string subobject] in

  let rec _strs_to_str strs =
    match strs with
    | [] -> ""
    | x :: xs -> x ^ "\n" ^ _strs_to_str xs in
  _strs_to_str @@ to_string ast
