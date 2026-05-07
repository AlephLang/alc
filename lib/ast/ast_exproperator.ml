type group = | Binary | Compare | Boolean | Assign | Prefix
and t =
  | BinaryAdd | BinarySub | BinaryMul | BinaryDiv | BinaryMod | BinaryShl | BinaryShr | BinaryAnd
  | BinaryOr | BinaryXor
  | CompareEq | CompareNotEq | CompareLThan | CompareGThan | CompareLThanEq | CompareGThanEq
  | BooleanAnd | BooleanOr
  | AssignEq | AssignAddEq | AssignSubEq | AssignMulEq | AssignDivEq | AssignModEq | AssignShlEq
  | AssignShrEq | AssignAndEq | AssignOrEq | AssignXorEq
  | PrefixNot | PrefixBooleanNot | PrefixNegative | PrefixDereference | PrefixAddress

let get_group operator =
  match operator with
  | BinaryAdd | BinarySub | BinaryMul | BinaryDiv | BinaryMod | BinaryShl | BinaryShr | BinaryAnd
  | BinaryOr | BinaryXor -> Binary
  | CompareEq | CompareNotEq | CompareLThan | CompareGThan | CompareLThanEq | CompareGThanEq ->
      Compare
  | BooleanAnd | BooleanOr -> Boolean
  | AssignEq | AssignAddEq | AssignSubEq | AssignMulEq | AssignDivEq | AssignModEq | AssignShlEq
  | AssignShrEq | AssignAndEq | AssignOrEq | AssignXorEq -> Assign
  | PrefixNot | PrefixBooleanNot | PrefixNegative | PrefixDereference | PrefixAddress -> Prefix
