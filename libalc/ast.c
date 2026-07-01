#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/string.h"
#include "alc/vector.h"
#include <stdio.h>
#include <inttypes.h>

static void *__safe_reserve_impl(usize stride, usize cap);
#define safe_reserve(_type, _cap) __safe_reserve_impl(sizeof(_type), (_cap))

static Alc_Vector(Alc_String) to_string(const Alc_Ast *ast);

static const char *func_kind_to_string(Alc_Ast_Function_Kind kind)
{
  switch (kind) {
  case ALC_AST_FUNCTION_KIND_DEFAULT:
    return "DEFAULT";
  case ALC_AST_FUNCTION_KIND_EXPLICIT:
    return "EXPLICIT";
  case ALC_AST_FUNCTION_KIND_EXPORTED:
    return "EXPORTED";
  }

  ALC_NOREACH();
}

static const char *struct_kind_to_string(Alc_Ast_Struct_Kind kind)
{
  switch (kind) {
  case ALC_AST_STRUCT_KIND_DEFAULT:
    return "DEFAULT";
  case ALC_AST_STRUCT_KIND_PARTIAL:
    return "PARTIAL";
  }

  ALC_NOREACH();
}

static Alc_Vector(Alc_String)
  build_tree(Alc_String header, Alc_Vector(Alc_Vector(Alc_String)) children_vs_v)
{
  usize children_vs_v_n = alc_vector_get_length(children_vs_v);
  Alc_Vector(Alc_String) out_strs_v = alc_vector_reserve(Alc_String, children_vs_v_n + 1);
  alc_vector_push(out_strs_v, header);

  for (usize i = 0; i < children_vs_v_n; i++) {
    Alc_Vector(Alc_String) children_v = children_vs_v[i];
    usize children_v_n = alc_vector_get_length(children_v);
    for (usize j = 0; j < children_v_n; j++) {
      const char *prefix = j == 0                  ? (i == children_vs_v_n - 1 ? "╰─" : "├─") :
                           i < children_vs_v_n - 1 ? "│ " :
                                                     "  ";
      Alc_String str = alc_string_create_from(prefix);
      alc_string_append(&str, &children_v[j]);
      alc_vector_push(out_strs_v, str);

      alc_string_destroy(&children_v[j]);
    }

    alc_vector_destroy(children_v);
  }

  alc_vector_destroy(children_vs_v);

  return out_strs_v;
}

static void array_to_strings(Alc_Vector(Alc_Vector(Alc_String)) children_vs_v,
                             struct __Alc_Ast **asts, usize n)
{
  for (usize i = 0; i < n; i++) {
    alc_vector_push(children_vs_v, to_string(asts[i]));
  }
}

static void add_to_strings_opt(Alc_Vector(Alc_Vector(Alc_String)) children_vs_v, const Alc_Ast *ast)
{
  if (ast == nullptr)
    return;

  alc_vector_push(children_vs_v, to_string(ast));
}

void alc_ast_print(const Alc_Ast *ast)
{
  ALC_ASSERT(ast != nullptr);

  Alc_Vector(Alc_String) strs = to_string(ast);
  usize strs_n = alc_vector_get_length(strs);
  for (usize i = 0; i < strs_n; i++) {
    printf("%s\n", strs[i].c_str);
    alc_string_destroy(&strs[i]);
  }
  alc_vector_destroy(strs);
}

static Alc_Vector(Alc_String) to_string(const Alc_Ast *ast)
{
  switch (ast->kind) {
  case ALC_AST_KIND_ROOT: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = safe_reserve(Alc_Vector(Alc_String), ast->data.ROOT.toplevel_statements_num);
    array_to_strings(children_vs_v, ast->data.ROOT.toplevel_statements,
                     ast->data.ROOT.toplevel_statements_num);
    return build_tree(alc_string_create_from("ROOT"), children_vs_v);
  }

  case ALC_AST_KIND_EXPR: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 3);
    alc_vector_push(children_vs_v, to_string(ast->data.EXPR.lhs));
    alc_vector_push(children_vs_v, to_string(ast->data.EXPR.rhs));
    alc_vector_push(children_vs_v, to_string(ast->data.EXPR.operator));
    return build_tree(alc_string_create_from("EXPR"), children_vs_v);
  }

  case ALC_AST_KIND_PREFIX_EXPR: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 2);
    alc_vector_push(children_vs_v, to_string(ast->data.PREFIX_EXPR.operand));
    alc_vector_push(children_vs_v, to_string(ast->data.PREFIX_EXPR.operator));
    return build_tree(alc_string_create_from("PREFIX_EXPR"), children_vs_v);
  }

  case ALC_AST_KIND_MODULE: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 1);
    Alc_String header = alc_string_create_from("MODULE { name: \"");
    alc_string_append_cstr(&header, ast->data.MODULE.name);
    alc_string_append_cstr(&header, "\" }");
    add_to_strings_opt(children_vs_v, ast->data.MODULE.submodule);
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_IMPORT: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 1);
    alc_vector_push(children_vs_v, to_string(ast->data.IMPORT.module));
    return build_tree(alc_string_create_from("IMPORT"), children_vs_v);
  }

  case ALC_AST_KIND_TYPEDEF: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 3);
    add_to_strings_opt(children_vs_v, ast->data.TYPEDEF.generic_placeholder_type_list);
    alc_vector_push(children_vs_v, to_string(ast->data.TYPEDEF.aliased_type));
    add_to_strings_opt(children_vs_v, ast->data.TYPEDEF.attribute_list);
    Alc_String header = alc_string_create_from("TYPEDEF { name: \"");
    alc_string_append_cstr(&header, ast->data.TYPEDEF.name);
    alc_string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_EXTERN_FUNC: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 2);
    alc_vector_push(children_vs_v, to_string(ast->data.EXTERN_FUNC.argument_list));
    add_to_strings_opt(children_vs_v, ast->data.EXTERN_FUNC.return_type);
    Alc_String header = alc_string_create_from("EXTERN_FUNC { name: \"");
    alc_string_append_cstr(&header, ast->data.EXTERN_FUNC.name);
    alc_string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_EXTERN_VARDECL: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 1);
    alc_vector_push(children_vs_v, to_string(ast->data.EXTERN_VARDECL.type));
    Alc_String header = alc_string_create_from("EXTERN_VARDECL { name: \"");
    alc_string_append_cstr(&header, ast->data.EXTERN_VARDECL.name);
    alc_string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_QUALIFIER: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 1);
    alc_vector_push(children_vs_v, to_string(ast->data.QUALIFIER.qualified));
    Alc_String header = alc_string_create_from("QUALIFIER { name: \"");
    alc_string_append_cstr(&header, ast->data.QUALIFIER.name);
    alc_string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_NONE: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("NONE"));
    return out_v;
  }

  case ALC_AST_KIND_VARIADIC: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("VARIADIC"));
    return out_v;
  }

  case ALC_AST_KIND_STRUCT: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), ast->data.STRUCT.children_num + 1);
    add_to_strings_opt(children_vs_v, ast->data.STRUCT.attribute_list);
    array_to_strings(children_vs_v, ast->data.STRUCT.children, ast->data.STRUCT.children_num);
    Alc_String header = alc_string_create_from("STRUCT { name: \"");
    alc_string_append_cstr(&header, ast->data.STRUCT.name);
    alc_string_append_cstr(&header, "\", kind: ");
    alc_string_append_cstr(&header, struct_kind_to_string(ast->data.STRUCT.kind));
    alc_string_append_cstr(&header, " }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_UNION: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), ast->data.UNION.children_num + 1);
    add_to_strings_opt(children_vs_v, ast->data.UNION.attribute_list);
    array_to_strings(children_vs_v, ast->data.UNION.children, ast->data.UNION.children_num);
    Alc_String header = alc_string_create_from("UNION { name: \"");
    alc_string_append_cstr(&header, ast->data.UNION.name);
    alc_string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_ENUM: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), ast->data.ENUM.elements_num + 1);
    add_to_strings_opt(children_vs_v, ast->data.ENUM.attribute_list);
    array_to_strings(children_vs_v, ast->data.ENUM.elements, ast->data.ENUM.elements_num);
    Alc_String header = alc_string_create_from("ENUM { name: \"");
    alc_string_append_cstr(&header, ast->data.ENUM.name);
    alc_string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_ENUM_ELEMENT: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 1);
    add_to_strings_opt(children_vs_v, ast->data.ENUM_ELEMENT.expression);
    Alc_String header = alc_string_create_from("ENUM_ELEMENT { name: \"");
    alc_string_append_cstr(&header, ast->data.ENUM_ELEMENT.name);
    alc_string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_FUNC: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 4);
    add_to_strings_opt(children_vs_v, ast->data.FUNC.attribute_list);
    alc_vector_push(children_vs_v, to_string(ast->data.FUNC.argument_list));
    add_to_strings_opt(children_vs_v, ast->data.FUNC.return_type);
    alc_vector_push(children_vs_v, to_string(ast->data.FUNC.body));
    Alc_String header = alc_string_create_from("FUNC { name: \"");
    alc_string_append_cstr(&header, ast->data.FUNC.name);
    alc_string_append_cstr(&header, "\", kind: ");
    alc_string_append_cstr(&header, func_kind_to_string(ast->data.FUNC.kind));
    alc_string_append_cstr(&header, " }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_ARGUMENT_LIST: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = safe_reserve(Alc_Vector(Alc_String), ast->data.ARGUMENT_LIST.arguments_num);
    array_to_strings(children_vs_v, ast->data.ARGUMENT_LIST.arguments,
                     ast->data.ARGUMENT_LIST.arguments_num);
    return build_tree(alc_string_create_from("ARGUMENT_LIST"), children_vs_v);
  }

  case ALC_AST_KIND_NAMESPACE: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 1);
    alc_vector_push(children_vs_v, to_string(ast->data.NAMESPACE.subobject));
    Alc_String header = alc_string_create_from("NAMESPACE { name: \"");
    alc_string_append_cstr(&header, ast->data.NAMESPACE.name);
    alc_string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_SCOPE: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    Alc_String header = alc_string_create_from("SCOPE { type: \"");
    alc_string_append_cstr(&header, ast->data.SCOPE.type);
    alc_string_append_cstr(&header, "\" }");
    alc_vector_push(out_v, header);
    return out_v;
  }

  case ALC_AST_KIND_CASE_CHAIN: {
    Alc_Vector(Alc_Vector(Alc_String)) children_vs_v =
      alc_vector_reserve(Alc_Vector(Alc_String), ast->data.CASE_CHAIN.cases_num + 1);
    array_to_strings(children_vs_v, ast->data.CASE_CHAIN.cases, ast->data.CASE_CHAIN.cases_num);
    alc_vector_push(children_vs_v, to_string(ast->data.CASE_CHAIN.body));
    return build_tree(alc_string_create_from("CASE_CHAIN"), children_vs_v);
  }

  case ALC_AST_KIND_CASE: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 1);
    alc_vector_push(children_vs_v, to_string(ast->data.CASE.expression));
    return build_tree(alc_string_create_from("CASE"), children_vs_v);
  }

  case ALC_AST_KIND_DEFAULT: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("DEFAULT"));
    return out_v;
  }

  case ALC_AST_KIND_ATTRIBUTE: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = safe_reserve(Alc_Vector(Alc_String), ast->data.ATTRIBUTE.arguments_num);
    array_to_strings(children_vs_v, ast->data.ATTRIBUTE.arguments,
                     ast->data.ATTRIBUTE.arguments_num);
    Alc_String header = alc_string_create_from("ATTRIBUTE { name: \"");
    alc_string_append_cstr(&header, ast->data.ATTRIBUTE.name);
    alc_string_append_cstr(&header, "\", has_arguments: ");
    alc_string_append_cstr(&header, ast->data.ATTRIBUTE.has_arguments ? "true" : "false");
    alc_string_append_cstr(&header, " }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_ATTRIBUTE_LIST: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = safe_reserve(Alc_Vector(Alc_String), ast->data.ATTRIBUTE_LIST.attributes_num);
    array_to_strings(children_vs_v, ast->data.ATTRIBUTE_LIST.attributes,
                     ast->data.ATTRIBUTE_LIST.attributes_num);
    return build_tree(alc_string_create_from("ATTRIBUTE_LIST"), children_vs_v);
  }

  case ALC_AST_KIND_EXPLICIT_CALL_ARGUMENT: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 1);
    alc_vector_push(children_vs_v, to_string(ast->data.EXPLICIT_CALL_ARGUMENT.expression));
    Alc_String header = alc_string_create_from("EXPLICIT_CALL_ARGUMENT { name: \"");
    alc_string_append_cstr(&header, ast->data.EXPLICIT_CALL_ARGUMENT.name);
    alc_string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_LABEL: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    Alc_String header = alc_string_create_from("LABEL { name: \"");
    alc_string_append_cstr(&header, ast->data.LABEL.name);
    alc_string_append_cstr(&header, "\" }");
    alc_vector_push(out_v, header);
    return out_v;
  }

  case ALC_AST_KIND_STMT_BLOCK: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = safe_reserve(Alc_Vector(Alc_String), ast->data.STMT_BLOCK.statements_num);
    array_to_strings(children_vs_v, ast->data.STMT_BLOCK.statements,
                     ast->data.STMT_BLOCK.statements_num);
    return build_tree(alc_string_create_from("STMT_BLOCK"), children_vs_v);
  }

  case ALC_AST_KIND_STMT_RETURN: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 1);
    add_to_strings_opt(children_vs_v, ast->data.STMT_RETURN.expression);
    return build_tree(alc_string_create_from("STMT_RETURN"), children_vs_v);
  }

  case ALC_AST_KIND_STMT_GOTO: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 1);
    alc_vector_push(children_vs_v, to_string(ast->data.STMT_GOTO.label));
    return build_tree(alc_string_create_from("STMT_GOTO"), children_vs_v);
  }

  case ALC_AST_KIND_STMT_LABEL: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 1);
    alc_vector_push(children_vs_v, to_string(ast->data.STMT_LABEL.label));
    return build_tree(alc_string_create_from("STMT_LABEL"), children_vs_v);
  }

  case ALC_AST_KIND_STMT_BREAK: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("STMT_BREAK"));
    return out_v;
  }

  case ALC_AST_KIND_STMT_CONTINUE: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("STMT_CONTINUE"));
    return out_v;
  }

  case ALC_AST_KIND_STMT_FALLTHROUGH: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("STMT_FALLTHROUGH"));
    return out_v;
  }

  case ALC_AST_KIND_STMT_WHILE: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 3);
    add_to_strings_opt(children_vs_v, ast->data.STMT_WHILE.attribute_list);
    alc_vector_push(children_vs_v, to_string(ast->data.STMT_WHILE.condition));
    alc_vector_push(children_vs_v, to_string(ast->data.STMT_WHILE.body));
    return build_tree(alc_string_create_from("STMT_WHILE"), children_vs_v);
  }

  case ALC_AST_KIND_STMT_FOR: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 5);
    add_to_strings_opt(children_vs_v, ast->data.STMT_FOR.attribute_list);
    add_to_strings_opt(children_vs_v, ast->data.STMT_FOR.init_statement);
    add_to_strings_opt(children_vs_v, ast->data.STMT_FOR.condition);
    add_to_strings_opt(children_vs_v, ast->data.STMT_FOR.expression);
    alc_vector_push(children_vs_v, to_string(ast->data.STMT_FOR.body));
    return build_tree(alc_string_create_from("STMT_FOR"), children_vs_v);
  }

  case ALC_AST_KIND_STMT_DO_WHILE: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 3);
    add_to_strings_opt(children_vs_v, ast->data.STMT_DO_WHILE.attribute_list);
    alc_vector_push(children_vs_v, to_string(ast->data.STMT_DO_WHILE.condition));
    alc_vector_push(children_vs_v, to_string(ast->data.STMT_DO_WHILE.body));
    return build_tree(alc_string_create_from("STMT_DO_WHILE"), children_vs_v);
  }

  case ALC_AST_KIND_STMT_LOOP: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 1);
    alc_vector_push(children_vs_v, to_string(ast->data.STMT_LOOP.body));
    return build_tree(alc_string_create_from("STMT_LOOP"), children_vs_v);
  }

  case ALC_AST_KIND_STMT_FOREACH: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 3);
    add_to_strings_opt(children_vs_v, ast->data.STMT_FOREACH.attribute_list);
    alc_vector_push(children_vs_v, to_string(ast->data.STMT_FOREACH.iterator));
    alc_vector_push(children_vs_v, to_string(ast->data.STMT_FOREACH.body));
    Alc_String header = alc_string_create_from("STMT_FOREACH { item_name: \"");
    alc_string_append_cstr(&header, ast->data.STMT_FOREACH.item_name);

    if (ast->data.STMT_FOREACH.i_name == nullptr)
      alc_string_append_cstr(&header, "\", i_name: - }");
    else {
      alc_string_append_cstr(&header, "\", i_name: \"");
      alc_string_append_cstr(&header, ast->data.STMT_FOREACH.i_name);
      alc_string_append_cstr(&header, "\" }");
    }

    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_STMT_EXPR: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 1);
    alc_vector_push(children_vs_v, to_string(ast->data.STMT_EXPR.expression));
    return build_tree(alc_string_create_from("STMT_EXPR"), children_vs_v);
  }

  case ALC_AST_KIND_STMT_SWITCH: {
    Alc_Vector(Alc_Vector(Alc_String)) children_vs_v =
      alc_vector_reserve(Alc_Vector(Alc_String), ast->data.STMT_SWITCH.case_chains_num + 1);
    alc_vector_push(children_vs_v, to_string(ast->data.STMT_SWITCH.expression));
    array_to_strings(children_vs_v, ast->data.STMT_SWITCH.case_chains,
                     ast->data.STMT_SWITCH.case_chains_num);
    return build_tree(alc_string_create_from("STMT_SWITCH"), children_vs_v);
  }

  case ALC_AST_KIND_STMT_DEFER: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 1);
    alc_vector_push(children_vs_v, to_string(ast->data.STMT_DEFER.body));
    return build_tree(alc_string_create_from("STMT_DEFER"), children_vs_v);
  }

  case ALC_AST_KIND_STMT_IF: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 4);
    add_to_strings_opt(children_vs_v, ast->data.STMT_IF.attribute_list);
    alc_vector_push(children_vs_v, to_string(ast->data.STMT_IF.condition));
    alc_vector_push(children_vs_v, to_string(ast->data.STMT_IF.body));
    add_to_strings_opt(children_vs_v, ast->data.STMT_IF.else_statement);
    return build_tree(alc_string_create_from("STMT_IF"), children_vs_v);
  }

  case ALC_AST_KIND_STMT_ELSE: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 1);
    alc_vector_push(children_vs_v, to_string(ast->data.STMT_ELSE.body));
    return build_tree(alc_string_create_from("STMT_ELSE"), children_vs_v);
  }

  case ALC_AST_KIND_TYPE_PLAIN: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    Alc_String header = alc_string_create_from("TYPE_PLAIN { name: \"");
    alc_string_append_cstr(&header, ast->data.TYPE_PLAIN.name);
    alc_string_append_cstr(&header, "\" }");
    alc_vector_push(out_v, header);
    return out_v;
  }

  case ALC_AST_KIND_TYPE_POINTER: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 1);
    alc_vector_push(children_vs_v, to_string(ast->data.TYPE_POINTER.type));
    return build_tree(alc_string_create_from("TYPE_POINTER"), children_vs_v);
  }

  case ALC_AST_KIND_TYPE_ARRAY: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 2);
    alc_vector_push(children_vs_v, to_string(ast->data.TYPE_ARRAY.type));
    add_to_strings_opt(children_vs_v, ast->data.TYPE_ARRAY.size_expression);
    return build_tree(alc_string_create_from("TYPE_ARRAY"), children_vs_v);
  }

  case ALC_AST_KIND_TYPE_FUNCTION_POINTER: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 2);
    alc_vector_push(children_vs_v, to_string(ast->data.TYPE_FUNCTION_POINTER.argument_list));
    add_to_strings_opt(children_vs_v, ast->data.TYPE_FUNCTION_POINTER.return_type);
    return build_tree(alc_string_create_from("TYPE_FUNCTION_POINTER"), children_vs_v);
  }

  case ALC_AST_KIND_TYPE_TYPE_OF: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 1);
    alc_vector_push(children_vs_v, to_string(ast->data.TYPE_TYPE_OF.expression));
    return build_tree(alc_string_create_from("TYPE_TYPE_OF"), children_vs_v);
  }

  case ALC_AST_KIND_VAR_DECL: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 2);
    add_to_strings_opt(children_vs_v, ast->data.VAR_DECL.attribute_list);
    alc_vector_push(children_vs_v, to_string(ast->data.VAR_DECL.type));
    Alc_String header = alc_string_create_from("VAR_DECL { name: \"");
    alc_string_append_cstr(&header, ast->data.VAR_DEF.name);
    alc_string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_VAR_DEF: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 3);
    add_to_strings_opt(children_vs_v, ast->data.VAR_DEF.attribute_list);
    add_to_strings_opt(children_vs_v, ast->data.VAR_DEF.type);
    alc_vector_push(children_vs_v, to_string(ast->data.VAR_DEF.expression));
    Alc_String header = alc_string_create_from("VAR_DEF { name: \"");
    alc_string_append_cstr(&header, ast->data.VAR_DEF.name);
    alc_string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_EXPR_OPERAND_IDENTIFIER: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    Alc_String header = alc_string_create_from("EXPR_OPERAND_IDENTIFIER { name: \"");
    alc_string_append_cstr(&header, ast->data.EXPR_OPERAND_IDENTIFIER.name);
    alc_string_append_cstr(&header, "\" }");
    alc_vector_push(out_v, header);
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERAND_NUMBER: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    char buf[1024] = { 0 };
    if (ast->data.EXPR_OPERAND_NUMBER.typespec == nullptr)
      snprintf(buf, 1024, "EXPR_OPERAND_NUMBER { value: %" PRIu64 ", typespec: - }",
               ast->data.EXPR_OPERAND_NUMBER.value);
    else
      snprintf(buf, 1024, "EXPR_OPERAND_NUMBER { value: %" PRIu64 ", typespec: \"%s\" }",
               ast->data.EXPR_OPERAND_NUMBER.value, ast->data.EXPR_OPERAND_NUMBER.typespec);
    alc_vector_push(out_v, alc_string_create_from(buf));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERAND_NUMBER_FLOAT: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    char buf[1024] = { 0 };
    if (ast->data.EXPR_OPERAND_NUMBER.typespec == nullptr)
      snprintf(buf, 1024, "EXPR_OPERAND_NUMBER_FLOAT { value: %lf, typespec: - }",
               ast->data.EXPR_OPERAND_NUMBER_FLOAT.value);
    else
      snprintf(buf, 1024, "EXPR_OPERAND_NUMBER_FLOAT { value: %lf, typespec: \"%s\" }",
               ast->data.EXPR_OPERAND_NUMBER_FLOAT.value,
               ast->data.EXPR_OPERAND_NUMBER_FLOAT.typespec);
    alc_vector_push(out_v, alc_string_create_from(buf));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERAND_ARRAY_ELEMENT: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 2);
    alc_vector_push(children_vs_v, to_string(ast->data.EXPR_OPERAND_ARRAY_ELEMENT.array));
    alc_vector_push(children_vs_v,
                    to_string(ast->data.EXPR_OPERAND_ARRAY_ELEMENT.index_expression));
    return build_tree(alc_string_create_from("EXPR_OPERAND_ARRAY_ELEMENT"), children_vs_v);
  }

  case ALC_AST_KIND_EXPR_OPERAND_CAST_TO: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 2);
    alc_vector_push(children_vs_v, to_string(ast->data.EXPR_OPERAND_CAST_TO.type));
    alc_vector_push(children_vs_v, to_string(ast->data.EXPR_OPERAND_CAST_TO.expression));
    return build_tree(alc_string_create_from("EXPR_OPERAND_CAST_TO"), children_vs_v);
  }

  case ALC_AST_KIND_EXPR_OPERAND_CALL: {
    Alc_Vector(Alc_Vector(Alc_String)) children_vs_v =
      safe_reserve(Alc_Vector(Alc_String), ast->data.EXPR_OPERAND_CALL.arguments_num);
    array_to_strings(children_vs_v, ast->data.EXPR_OPERAND_CALL.arguments,
                     ast->data.EXPR_OPERAND_CALL.arguments_num);
    Alc_String header = alc_string_create_from("EXPR_OPERAND_CALL { callee_name: \"");
    alc_string_append_cstr(&header, ast->data.EXPR_OPERAND_CALL.callee_name);
    alc_string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_EXPR_OPERAND_GENERIC_CALL: {
    Alc_Vector(Alc_Vector(Alc_String)) children_vs_v = alc_vector_reserve(
      Alc_Vector(Alc_String), ast->data.EXPR_OPERAND_GENERIC_CALL.arguments_num + 1);
    alc_vector_push(children_vs_v,
                    to_string(ast->data.EXPR_OPERAND_GENERIC_CALL.generic_type_list));
    array_to_strings(children_vs_v, ast->data.EXPR_OPERAND_GENERIC_CALL.arguments,
                     ast->data.EXPR_OPERAND_GENERIC_CALL.arguments_num);
    Alc_String header = alc_string_create_from("EXPR_OPERAND_GENERIC_CALL { callee_name: \"");
    alc_string_append_cstr(&header, ast->data.EXPR_OPERAND_GENERIC_CALL.callee_name);
    alc_string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_EXPR_OPERAND_STRING: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    char buf[1024] = { 0 };
    if (ast->data.EXPR_OPERAND_STRING.typespec == nullptr)
      snprintf(buf, 1024, "EXPR_OPERAND_STRING { content: \"%s\", typespec: - }",
               ast->data.EXPR_OPERAND_STRING.content);
    else
      snprintf(buf, 1024, "EXPR_OPERAND_STRING { value: \"%s\", typespec: \"%s\" }",
               ast->data.EXPR_OPERAND_STRING.content, ast->data.EXPR_OPERAND_STRING.typespec);
    alc_vector_push(out_v, alc_string_create_from(buf));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERAND_SYMBOL: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    char buf[1024] = { 0 };
    if (ast->data.EXPR_OPERAND_SYMBOL.typespec == nullptr)
      snprintf(buf, 1024, "EXPR_OPERAND_SYMBOL { content: \"%s\", typespec: - }",
               ast->data.EXPR_OPERAND_SYMBOL.content);
    else
      snprintf(buf, 1024, "EXPR_OPERAND_SYMBOL { value: \"%s\", typespec: \"%s\" }",
               ast->data.EXPR_OPERAND_SYMBOL.content, ast->data.EXPR_OPERAND_SYMBOL.typespec);
    alc_vector_push(out_v, alc_string_create_from(buf));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERAND_ACCESS_MEMBER: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 2);
    alc_vector_push(children_vs_v, to_string(ast->data.EXPR_OPERAND_ACCESS_MEMBER.from));
    alc_vector_push(children_vs_v, to_string(ast->data.EXPR_OPERAND_ACCESS_MEMBER.what));
    return build_tree(alc_string_create_from("EXPR_OPERAND_ACCESS_MEMBER"), children_vs_v);
  }

  case ALC_AST_KIND_EXPR_OPERAND_SIZE_OF: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 1);
    alc_vector_push(children_vs_v, to_string(ast->data.EXPR_OPERAND_SIZE_OF.type));
    return build_tree(alc_string_create_from("EXPR_OPERAND_SIZE_OF"), children_vs_v);
  }

  case ALC_AST_KIND_EXPR_OPERAND_ALIGN_OF: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 1);
    alc_vector_push(children_vs_v, to_string(ast->data.EXPR_OPERAND_ALIGN_OF.expression));
    return build_tree(alc_string_create_from("EXPR_OPERAND_ALIGN_OF"), children_vs_v);
  }

  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_ADD: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_BINARY_ADD"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_SUB: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_BINARY_SUB"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_MUL: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_BINARY_MUL"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_DIV: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_BINARY_DIV"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_MOD: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_BINARY_MOD"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_SHL: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_BINARY_SHL"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_SHR: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_BINARY_SHR"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_AND: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_BINARY_AND"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_OR: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_BINARY_OR"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_XOR: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_BINARY_XOR"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_COMPARE_EQ: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_COMPARE_EQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_COMPARE_NOTEQ: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_COMPARE_NOTEQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_COMPARE_LTHAN: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_COMPARE_LTHAN"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_COMPARE_GTHAN: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_COMPARE_GTHAN"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_COMPARE_LTHANEQ: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_COMPARE_LTHANEQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_COMPARE_GTHANEQ: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_COMPARE_GTHANEQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_BOOLEAN_AND: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_BOOLEAN_AND"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_BOOLEAN_OR: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_BOOLEAN_OR"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_EQ: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_ASSIGN_EQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_ADDEQ: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_ASSIGN_ADDEQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_SUBEQ: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_ASSIGN_SUBEQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_MULEQ: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_ASSIGN_MULEQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_DIVEQ: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_ASSIGN_DIVEQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_MODEQ: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_ASSIGN_MODEQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_SHLEQ: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_ASSIGN_SHLEQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_SHREQ: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_ASSIGN_SHREQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_ANDEQ: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_ASSIGN_ANDEQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_OREQ: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_ASSIGN_OREQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_XOREQ: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_ASSIGN_XOREQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_PREFIX_NOT: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_PREFIX_NOT"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_PREFIX_BOOLEAN_NOT: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_PREFIX_BOOLEAN_NOT"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_PREFIX_NEGATIVE: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_PREFIX_NEGATIVE"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_PREFIX_DEREFERENCE: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_PREFIX_DEREFERENCE"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_PREFIX_ADDRESS: {
    Alc_String *out_v = alc_vector_reserve(Alc_String, 1);
    alc_vector_push(out_v, alc_string_create_from("EXPR_OPERATOR_PREFIX_ADDRESS"));
    return out_v;
  }

  case ALC_AST_KIND_INITLIST: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = safe_reserve(Alc_Vector(Alc_String), ast->data.INITLIST.entries_num);
    array_to_strings(children_vs_v, ast->data.INITLIST.entries, ast->data.INITLIST.entries_num);
    return build_tree(alc_string_create_from("INITLIST"), children_vs_v);
  }

  case ALC_AST_KIND_INITLIST_ENTRY: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 1);
    alc_vector_push(children_vs_v, to_string(ast->data.INITLIST_ENTRY.expression));
    return build_tree(alc_string_create_from("INITLIST_ENTRY"), children_vs_v);
  }

  case ALC_AST_KIND_INITLIST_ENTRY_EXPLICIT: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 1);
    alc_vector_push(children_vs_v, to_string(ast->data.INITLIST_ENTRY_EXPLICIT.expression));
    Alc_String header = alc_string_create_from("INITLIST_ENTRY_EXPLICIT { field_name: \"");
    alc_string_append_cstr(&header, ast->data.INITLIST_ENTRY_EXPLICIT.field_name);
    alc_string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_INITLIST_ENTRY_EXPLICIT_ARRAY_ELEMENT: {
    Alc_Vector(Alc_Vector(Alc_String)) children_vs_v = alc_vector_reserve(
      Alc_String *, ast->data.INITLIST_ENTRY_EXPLICIT_ARRAY_ELEMENT.index_expressions_num + 1);
    array_to_strings(children_vs_v,
                     ast->data.INITLIST_ENTRY_EXPLICIT_ARRAY_ELEMENT.index_expressions,
                     ast->data.INITLIST_ENTRY_EXPLICIT_ARRAY_ELEMENT.index_expressions_num);
    alc_vector_push(children_vs_v,
                    to_string(ast->data.INITLIST_ENTRY_EXPLICIT_ARRAY_ELEMENT.expression));
    return build_tree(alc_string_create_from("INITLIST_ENTRY_EXPLICIT_ARRAY_ELEMENT"),
                      children_vs_v);
  }

  case ALC_AST_KIND_GENERIC_STRUCT: {
    Alc_Vector(Alc_Vector(Alc_String)) children_vs_v =
      alc_vector_reserve(Alc_Vector(Alc_String), ast->data.GENERIC_STRUCT.children_num + 2);
    add_to_strings_opt(children_vs_v, ast->data.GENERIC_STRUCT.attribute_list);
    alc_vector_push(children_vs_v,
                    to_string(ast->data.GENERIC_STRUCT.generic_placeholder_type_list));
    array_to_strings(children_vs_v, ast->data.GENERIC_STRUCT.children,
                     ast->data.GENERIC_STRUCT.children_num);
    Alc_String header = alc_string_create_from("GENERIC_STRUCT { name: \"");
    alc_string_append_cstr(&header, ast->data.GENERIC_STRUCT.name);
    alc_string_append_cstr(&header, "\", kind: ");
    alc_string_append_cstr(&header, struct_kind_to_string(ast->data.GENERIC_STRUCT.kind));
    alc_string_append_cstr(&header, " }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_GENERIC_FUNC: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 5);
    add_to_strings_opt(children_vs_v, ast->data.GENERIC_FUNC.attribute_list);
    alc_vector_push(children_vs_v, to_string(ast->data.GENERIC_FUNC.generic_placeholder_type_list));
    alc_vector_push(children_vs_v, to_string(ast->data.GENERIC_FUNC.argument_list));
    add_to_strings_opt(children_vs_v, ast->data.GENERIC_FUNC.return_type);
    alc_vector_push(children_vs_v, to_string(ast->data.GENERIC_FUNC.body));
    Alc_String header = alc_string_create_from("GENERIC_FUNC { name: \"");
    alc_string_append_cstr(&header, ast->data.GENERIC_FUNC.name);
    alc_string_append_cstr(&header, "\", kind: ");
    alc_string_append_cstr(&header, func_kind_to_string(ast->data.GENERIC_FUNC.kind));
    alc_string_append_cstr(&header, " }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_GENERIC_PLACEHOLDER_TYPE_LIST: {
    Alc_Vector(Alc_Vector(Alc_String)) children_vs_v =
      safe_reserve(Alc_Vector(Alc_String),
                   ast->data.GENERIC_PLACEHOLDER_TYPE_LIST.generic_placeholder_types_num);
    array_to_strings(children_vs_v,
                     ast->data.GENERIC_PLACEHOLDER_TYPE_LIST.generic_placeholder_types,
                     ast->data.GENERIC_PLACEHOLDER_TYPE_LIST.generic_placeholder_types_num);
    return build_tree(alc_string_create_from("GENERIC_PLACEHOLDER_TYPE_LIST"), children_vs_v);
  }

  case ALC_AST_KIND_GENERIC_PLACEHOLDER_TYPE: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 1);
    add_to_strings_opt(children_vs_v, ast->data.GENERIC_PLACEHOLDER_TYPE.default_type);
    Alc_String header = alc_string_create_from("GENERIC_PLACEHOLDER_TYPE { name: \"");
    alc_string_append_cstr(&header, ast->data.GENERIC_PLACEHOLDER_TYPE.name);
    alc_string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_GENERIC_TYPE_LIST: {
    Alc_Vector(Alc_Vector(Alc_String)) children_vs_v =
      safe_reserve(Alc_Vector(Alc_String), ast->data.GENERIC_TYPE_LIST.generic_types_num);
    array_to_strings(children_vs_v, ast->data.GENERIC_TYPE_LIST.generic_types,
                     ast->data.GENERIC_TYPE_LIST.generic_types_num);
    return build_tree(alc_string_create_from("GENERIC_TYPE_LIST"), children_vs_v);
  }

  case ALC_AST_KIND_GENERIC_TYPE: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 1);
    alc_vector_push(children_vs_v, to_string(ast->data.GENERIC_TYPE.generic_type_list));
    Alc_String header = alc_string_create_from("GENERIC_TYPE { name: \"");
    alc_string_append_cstr(&header, ast->data.GENERIC_TYPE.name);
    alc_string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_GENERIC_NAMESPACE: {
    Alc_Vector(Alc_Vector(Alc_String))
      children_vs_v = alc_vector_reserve(Alc_Vector(Alc_String), 2);
    alc_vector_push(children_vs_v, to_string(ast->data.GENERIC_NAMESPACE.generic_type_list));
    alc_vector_push(children_vs_v, to_string(ast->data.GENERIC_NAMESPACE.subobject));
    Alc_String header = alc_string_create_from("GENERIC_NAMESPACE { name: \"");
    alc_string_append_cstr(&header, ast->data.GENERIC_NAMESPACE.name);
    alc_string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }
  }

  ALC_NOREACH();
}

static inline void *__safe_reserve_impl(usize stride, usize cap)
{
  ALC_ASSUME(stride > 0);
  return cap > 0 ? __alc_vector_create_impl(stride, cap) : __alc_vector_create_impl(stride, 1);
}
