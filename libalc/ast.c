#include "alc/ast.h"
#include "alc/defs.h"
#include "containers/string.h"
#include "containers/vector.h"
#include <stdio.h>
#include <inttypes.h>

static void *__safe_reserve_impl(usize stride, usize cap);
#define safe_reserve(_type, _cap) __safe_reserve_impl(sizeof(_type), (_cap))

static string_t *to_string(const alc_ast_t *ast);

static const char *func_kind_to_string(alc_ast_function_kind_t kind)
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

static string_t *build_tree(string_t header, string_t **children_vs_v)
{
  usize children_vs_v_n = vector_get_length(children_vs_v);
  string_t *out_strs_v = vector_reserve(string_t, children_vs_v_n + 1);
  vector_push(out_strs_v, header);

  for (usize i = 0; i < children_vs_v_n; i++) {
    string_t *children_v = children_vs_v[i];
    usize children_v_n = vector_get_length(children_v);
    for (usize j = 0; j < children_v_n; j++) {
      const char *prefix = j == 0                  ? (i == children_vs_v_n - 1 ? "╰─" : "├─") :
                           i < children_vs_v_n - 1 ? "│ " :
                                                     "  ";
      string_t str = string_create_from(prefix);
      string_append(&str, &children_v[j]);
      vector_push(out_strs_v, str);

      string_destroy(&children_v[j]);
    }

    vector_destroy(children_v);
  }

  vector_destroy(children_vs_v);

  return out_strs_v;
}

static void array_to_strings(string_t **children_vs_v, struct __alc_ast_t **asts, usize n)
{
  for (usize i = 0; i < n; i++) {
    vector_push(children_vs_v, to_string(asts[i]));
  }
}

static void add_to_strings_opt(string_t **children_vs_v, const alc_ast_t *ast)
{
  if (ast == nullptr)
    return;
  vector_push(children_vs_v, to_string(ast));
}

void alc_ast_print(const alc_ast_t *ast)
{
  ALC_ASSERT(ast != nullptr);

  string_t *strs = to_string(ast);
  usize strs_n = vector_get_length(strs);
  for (usize i = 0; i < strs_n; i++) {
    printf("%s\n", strs[i].c_str);
    string_destroy(&strs[i]);
  }
  vector_destroy(strs);
}

static string_t *to_string(const alc_ast_t *ast)
{
  switch (ast->kind) {
  case ALC_AST_KIND_ROOT: {
    string_t **children_vs_v = safe_reserve(string_t *, ast->data.ROOT.toplevel_statements_num);
    array_to_strings(children_vs_v, ast->data.ROOT.toplevel_statements,
                     ast->data.ROOT.toplevel_statements_num);
    return build_tree(string_create_from("ROOT"), children_vs_v);
  }

  case ALC_AST_KIND_EXPR: {
    string_t **children_vs_v = vector_reserve(string_t *, 3);
    vector_push(children_vs_v, to_string(ast->data.EXPR.lhs));
    vector_push(children_vs_v, to_string(ast->data.EXPR.rhs));
    vector_push(children_vs_v, to_string(ast->data.EXPR.operator));
    return build_tree(string_create_from("EXPR"), children_vs_v);
  }

  case ALC_AST_KIND_PREFIX_EXPR: {
    string_t **children_vs_v = vector_reserve(string_t *, 2);
    vector_push(children_vs_v, to_string(ast->data.PREFIX_EXPR.operand));
    vector_push(children_vs_v, to_string(ast->data.PREFIX_EXPR.operator));
    return build_tree(string_create_from("PREFIX_EXPR"), children_vs_v);
  }

  case ALC_AST_KIND_MODULE: {
    string_t **children_vs_v = vector_reserve(string_t *, 1);
    string_t header = string_create_from("MODULE { name: \"");
    string_append_cstr(&header, ast->data.MODULE.name);
    string_append_cstr(&header, "\" }");
    add_to_strings_opt(children_vs_v, ast->data.MODULE.submodule);
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_IMPORT: {
    string_t **children_vs_v = vector_reserve(string_t *, 1);
    vector_push(children_vs_v, to_string(ast->data.IMPORT.module));
    return build_tree(string_create_from("IMPORT"), children_vs_v);
  }

  case ALC_AST_KIND_TYPEDEF: {
    string_t **children_vs_v = vector_reserve(string_t *, 3);
    add_to_strings_opt(children_vs_v, ast->data.TYPEDEF.generic_placeholder_type_list);
    vector_push(children_vs_v, to_string(ast->data.TYPEDEF.aliased_type));
    add_to_strings_opt(children_vs_v, ast->data.TYPEDEF.attribute_list);
    string_t header = string_create_from("TYPEDEF { name: \"");
    string_append_cstr(&header, ast->data.TYPEDEF.name);
    string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_EXTERN_FUNC: {
    string_t **children_vs_v = vector_reserve(string_t *, 2);
    vector_push(children_vs_v, to_string(ast->data.EXTERN_FUNC.argument_list));
    add_to_strings_opt(children_vs_v, ast->data.EXTERN_FUNC.argument_list);
    string_t header = string_create_from("EXTERN_FUNC { name: \"");
    string_append_cstr(&header, ast->data.EXTERN_FUNC.name);
    string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_EXTERN_VARDECL: {
    string_t **children_vs_v = vector_reserve(string_t *, 1);
    vector_push(children_vs_v, to_string(ast->data.EXTERN_VARDECL.type));
    string_t header = string_create_from("EXTERN_VARDECL { name: \"");
    string_append_cstr(&header, ast->data.EXTERN_VARDECL.name);
    string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_QUALIFIER: {
    string_t **children_vs_v = vector_reserve(string_t *, 1);
    vector_push(children_vs_v, to_string(ast->data.QUALIFIER.qualified));
    string_t header = string_create_from("QUALIFIER { name: \"");
    string_append_cstr(&header, ast->data.QUALIFIER.name);
    string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_NONE: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("NONE"));
    return out_v;
  }

  case ALC_AST_KIND_VARIADIC: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("VARIADIC"));
    return out_v;
  }

  case ALC_AST_KIND_STRUCT: {
    string_t **children_vs_v = vector_reserve(string_t *, ast->data.STRUCT.children_num + 1);
    add_to_strings_opt(children_vs_v, ast->data.STRUCT.attribute_list);
    array_to_strings(children_vs_v, ast->data.STRUCT.children, ast->data.STRUCT.children_num);
    string_t header = string_create_from("STRUCT { name: \"");
    string_append_cstr(&header, ast->data.STRUCT.name);
    string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_UNION: {
    string_t **children_vs_v = vector_reserve(string_t *, ast->data.UNION.children_num + 1);
    add_to_strings_opt(children_vs_v, ast->data.UNION.attribute_list);
    array_to_strings(children_vs_v, ast->data.UNION.children, ast->data.UNION.children_num);
    string_t header = string_create_from("UNION { name: \"");
    string_append_cstr(&header, ast->data.UNION.name);
    string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_ENUM: {
    string_t **children_vs_v = vector_reserve(string_t *, ast->data.ENUM.elements_num + 1);
    add_to_strings_opt(children_vs_v, ast->data.ENUM.attribute_list);
    array_to_strings(children_vs_v, ast->data.ENUM.elements, ast->data.ENUM.elements_num);
    string_t header = string_create_from("ENUM { name: \"");
    string_append_cstr(&header, ast->data.ENUM.name);
    string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_ENUM_ELEMENT: {
    string_t **children_vs_v = vector_reserve(string_t *, 1);
    add_to_strings_opt(children_vs_v, ast->data.ENUM_ELEMENT.expression);
    string_t header = string_create_from("ENUM { name: \"");
    string_append_cstr(&header, ast->data.ENUM_ELEMENT.name);
    string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_FUNC: {
    string_t **children_vs_v = vector_reserve(string_t *, 4);
    add_to_strings_opt(children_vs_v, ast->data.FUNC.attribute_list);
    vector_push(children_vs_v, to_string(ast->data.FUNC.argument_list));
    add_to_strings_opt(children_vs_v, ast->data.FUNC.return_type);
    vector_push(children_vs_v, to_string(ast->data.FUNC.body));
    string_t header = string_create_from("FUNC { name: \"");
    string_append_cstr(&header, ast->data.FUNC.name);
    string_append_cstr(&header, "\", kind: ");
    string_append_cstr(&header, func_kind_to_string(ast->data.FUNC.kind));
    string_append_cstr(&header, " }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_ARGUMENT_LIST: {
    string_t **children_vs_v = safe_reserve(string_t *, ast->data.ARGUMENT_LIST.arguments_num);
    array_to_strings(children_vs_v, ast->data.ARGUMENT_LIST.arguments,
                     ast->data.ARGUMENT_LIST.arguments_num);
    return build_tree(string_create_from("ARGUMENT_LIST"), children_vs_v);
  }

  case ALC_AST_KIND_NAMESPACE: {
    string_t **children_vs_v = vector_reserve(string_t *, 1);
    vector_push(children_vs_v, to_string(ast->data.NAMESPACE.subobject));
    string_t header = string_create_from("NAMESPACE { name: \"");
    string_append_cstr(&header, ast->data.NAMESPACE.name);
    string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_CASE_CHAIN: {
    string_t **children_vs_v = vector_reserve(string_t *, ast->data.CASE_CHAIN.cases_num + 1);
    array_to_strings(children_vs_v, ast->data.CASE_CHAIN.cases, ast->data.CASE_CHAIN.cases_num);
    vector_push(children_vs_v, ast->data.CASE_CHAIN.body);
    return build_tree(string_create_from("CASE_CHAIN"), children_vs_v);
  }

  case ALC_AST_KIND_CASE: {
    string_t **children_vs_v = vector_reserve(string_t *, 1);
    vector_push(children_vs_v, to_string(ast->data.CASE.expression));
    return build_tree(string_create_from("CASE"), children_vs_v);
  }

  case ALC_AST_KIND_DEFAULT: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("DEFAULT"));
    return out_v;
  }

  case ALC_AST_KIND_ATTRIBUTE: {
    string_t **children_vs_v = safe_reserve(string_t *, ast->data.ATTRIBUTE.arguments_num);
    array_to_strings(children_vs_v, ast->data.ATTRIBUTE.arguments,
                     ast->data.ATTRIBUTE.arguments_num);
    string_t header = string_create_from("ATTRIBUTE { name: \"");
    string_append_cstr(&header, ast->data.ATTRIBUTE.name);
    string_append_cstr(&header, "\", has_arguments: ");
    string_append_cstr(&header, ast->data.ATTRIBUTE.has_arguments ? "true" : "false");
    string_append_cstr(&header, " }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_ATTRIBUTE_LIST: {
    string_t **children_vs_v = safe_reserve(string_t *, ast->data.ATTRIBUTE_LIST.attributes_num);
    array_to_strings(children_vs_v, ast->data.ATTRIBUTE_LIST.attributes,
                     ast->data.ATTRIBUTE_LIST.attributes_num);
    return build_tree(string_create_from("ATTRIBUTE_LIST"), children_vs_v);
  }

  case ALC_AST_KIND_STMT_BLOCK: {
    string_t **children_vs_v = safe_reserve(string_t *, ast->data.STMT_BLOCK.statements_num);
    array_to_strings(children_vs_v, ast->data.STMT_BLOCK.statements,
                     ast->data.STMT_BLOCK.statements_num);
    return build_tree(string_create_from("STMT_BLOCK"), children_vs_v);
  }

  case ALC_AST_KIND_STMT_RETURN: {
    string_t **children_vs_v = vector_reserve(string_t *, 1);
    add_to_strings_opt(children_vs_v, ast->data.STMT_RETURN.expression);
    return build_tree(string_create_from("STMT_RETURN"), children_vs_v);
  }

  case ALC_AST_KIND_STMT_GOTO: {
    string_t *out_v = vector_reserve(string_t, 1);
    string_t header = string_create_from("STMT_GOTO { label_name: \"");
    string_append_cstr(&header, ast->data.STMT_GOTO.label_name);
    string_append_cstr(&header, "\" }");
    vector_push(out_v, header);
    return out_v;
  }

  case ALC_AST_KIND_STMT_LABEL: {
    string_t *out_v = vector_reserve(string_t, 1);
    string_t header = string_create_from("STMT_LABEL { name: \"");
    string_append_cstr(&header, ast->data.STMT_LABEL.name);
    string_append_cstr(&header, "\" }");
    vector_push(out_v, header);
    return out_v;
  }

  case ALC_AST_KIND_STMT_BREAK: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("STMT_BREAK"));
    return out_v;
  }

  case ALC_AST_KIND_STMT_CONTINUE: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("STMT_CONTINUE"));
    return out_v;
  }

  case ALC_AST_KIND_STMT_FALLTHROUGH: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("STMT_FALLTHROUGH"));
    return out_v;
  }

  case ALC_AST_KIND_STMT_WHILE: {
    string_t **children_vs_v = vector_reserve(string_t *, 3);
    add_to_strings_opt(children_vs_v, ast->data.STMT_WHILE.attribute_list);
    vector_push(children_vs_v, to_string(ast->data.STMT_WHILE.condition));
    vector_push(children_vs_v, to_string(ast->data.STMT_WHILE.body));
    return build_tree(string_create_from("STMT_WHILE"), children_vs_v);
  }

  case ALC_AST_KIND_STMT_FOR: {
    string_t **children_vs_v = vector_reserve(string_t *, 5);
    add_to_strings_opt(children_vs_v, ast->data.STMT_FOR.attribute_list);
    add_to_strings_opt(children_vs_v, ast->data.STMT_FOR.init_statement);
    add_to_strings_opt(children_vs_v, ast->data.STMT_FOR.condition);
    add_to_strings_opt(children_vs_v, ast->data.STMT_FOR.expression);
    vector_push(children_vs_v, to_string(ast->data.STMT_FOR.body));
    return build_tree(string_create_from("STMT_FOR"), children_vs_v);
  }

  case ALC_AST_KIND_STMT_DO_WHILE: {
    string_t **children_vs_v = vector_reserve(string_t *, 3);
    add_to_strings_opt(children_vs_v, ast->data.STMT_DO_WHILE.attribute_list);
    vector_push(children_vs_v, to_string(ast->data.STMT_DO_WHILE.condition));
    vector_push(children_vs_v, to_string(ast->data.STMT_DO_WHILE.body));
    return build_tree(string_create_from("STMT_DO_WHILE"), children_vs_v);
  }

  case ALC_AST_KIND_STMT_LOOP: {
    string_t **children_vs_v = vector_reserve(string_t *, 1);
    vector_push(children_vs_v, to_string(ast->data.STMT_LOOP.body));
    return build_tree(string_create_from("STMT_LOOP"), children_vs_v);
  }

  case ALC_AST_KIND_STMT_FOREACH: {
    string_t **children_vs_v = vector_reserve(string_t *, 3);
    add_to_strings_opt(children_vs_v, ast->data.STMT_FOREACH.attribute_list);
    vector_push(children_vs_v, to_string(ast->data.STMT_FOREACH.iterator));
    vector_push(children_vs_v, to_string(ast->data.STMT_FOREACH.body));
    string_t header = string_create_from("STMT_FOREACH { item_name: \"");
    string_append_cstr(&header, ast->data.STMT_FOREACH.item_name);
    string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_STMT_EXPR: {
    string_t **children_vs_v = vector_reserve(string_t *, 1);
    vector_push(children_vs_v, to_string(ast->data.STMT_EXPR.expression));
    return build_tree(string_create_from("STMT_EXPR"), children_vs_v);
  }

  case ALC_AST_KIND_STMT_SWITCH: {
    string_t **children_vs_v =
      vector_reserve(string_t *, ast->data.STMT_SWITCH.case_chains_num + 1);
    vector_push(children_vs_v, to_string(ast->data.STMT_SWITCH.expression));
    array_to_strings(children_vs_v, ast->data.STMT_SWITCH.case_chains,
                     ast->data.STMT_SWITCH.case_chains_num);
    return build_tree(string_create_from("STMT_SWITCH"), children_vs_v);
  }

  case ALC_AST_KIND_STMT_DEFER: {
    string_t **children_vs_v = vector_reserve(string_t *, 1);
    vector_push(children_vs_v, to_string(ast->data.STMT_DEFER.body));
    return build_tree(string_create_from("STMT_DEFER"), children_vs_v);
  }

  case ALC_AST_KIND_STMT_IF: {
    string_t **children_vs_v = vector_reserve(string_t *, 4);
    add_to_strings_opt(children_vs_v, ast->data.STMT_IF.attribute_list);
    vector_push(children_vs_v, to_string(ast->data.STMT_IF.condition));
    vector_push(children_vs_v, to_string(ast->data.STMT_IF.body));
    add_to_strings_opt(children_vs_v, ast->data.STMT_IF.else_statement);
    return build_tree(string_create_from("STMT_IF"), children_vs_v);
  }

  case ALC_AST_KIND_STMT_ELSE: {
    string_t **children_vs_v = vector_reserve(string_t *, 1);
    vector_push(children_vs_v, to_string(ast->data.STMT_ELSE.body));
    return build_tree(string_create_from("STMT_ELSE"), children_vs_v);
  }

  case ALC_AST_KIND_TYPE_PLAIN: {
    string_t *out_v = vector_reserve(string_t, 1);
    string_t header = string_create_from("TYPE_PLAIN { name: \"");
    string_append_cstr(&header, ast->data.TYPE_PLAIN.name);
    string_append_cstr(&header, "\" }");
    vector_push(out_v, header);
    return out_v;
  }

  case ALC_AST_KIND_TYPE_POINTER: {
    string_t **children_vs_v = vector_reserve(string_t *, 1);
    vector_push(children_vs_v, to_string(ast->data.TYPE_POINTER.type));
    return build_tree(string_create_from("TYPE_POINTER"), children_vs_v);
  }

  case ALC_AST_KIND_TYPE_ARRAY: {
    string_t **children_vs_v = vector_reserve(string_t *, 2);
    vector_push(children_vs_v, to_string(ast->data.TYPE_ARRAY.type));
    add_to_strings_opt(children_vs_v, ast->data.TYPE_ARRAY.size_expression);
    return build_tree(string_create_from("TYPE_ARRAY"), children_vs_v);
  }

  case ALC_AST_KIND_TYPE_FUNCTION_POINTER: {
    string_t **children_vs_v = vector_reserve(string_t *, 2);
    vector_push(children_vs_v, to_string(ast->data.TYPE_FUNCTION_POINTER.argument_list));
    add_to_strings_opt(children_vs_v, ast->data.TYPE_FUNCTION_POINTER.return_type);
    return build_tree(string_create_from("TYPE_FUNCTION_POINTER"), children_vs_v);
  }

  case ALC_AST_KIND_TYPE_TYPE_OF: {
    string_t **children_vs_v = vector_reserve(string_t *, 1);
    vector_push(children_vs_v, to_string(ast->data.TYPE_TYPE_OF.expression));
    return build_tree(string_create_from("TYPE_TYPE_OF"), children_vs_v);
  }

  case ALC_AST_KIND_VAR_DECL: {
    string_t **children_vs_v = vector_reserve(string_t *, 2);
    add_to_strings_opt(children_vs_v, ast->data.VAR_DECL.attribute_list);
    vector_push(children_vs_v, to_string(ast->data.VAR_DECL.type));
    string_t header = string_create_from("VAR_DECL { name: \"");
    string_append_cstr(&header, ast->data.VAR_DEF.name);
    string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_VAR_DEF: {
    string_t **children_vs_v = vector_reserve(string_t *, 3);
    add_to_strings_opt(children_vs_v, ast->data.VAR_DEF.attribute_list);
    add_to_strings_opt(children_vs_v, ast->data.VAR_DEF.type);
    vector_push(children_vs_v, to_string(ast->data.VAR_DEF.expression));
    string_t header = string_create_from("VAR_DEF { name: \"");
    string_append_cstr(&header, ast->data.VAR_DEF.name);
    string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_EXPR_OPERAND_IDENTIFIER: {
    string_t *out_v = vector_reserve(string_t, 1);
    string_t header = string_create_from("EXPR_OPERAND_IDENTIFIER { name: \"");
    string_append_cstr(&header, ast->data.EXPR_OPERAND_IDENTIFIER.name);
    string_append_cstr(&header, "\" }");
    vector_push(out_v, header);
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERAND_NUMBER: {
    string_t *out_v = vector_reserve(string_t, 1);
    char buf[1024] = { 0 };
    if (ast->data.EXPR_OPERAND_NUMBER.typespec == nullptr)
      snprintf(buf, 1024, "EXPR_OPERAND_NUMBER { value: %" PRIu64 ", typespec: - }",
               ast->data.EXPR_OPERAND_NUMBER.value);
    else
      snprintf(buf, 1024, "EXPR_OPERAND_NUMBER { value: %" PRIu64 ", typespec: \"%s\" }",
               ast->data.EXPR_OPERAND_NUMBER.value, ast->data.EXPR_OPERAND_NUMBER.typespec);
    vector_push(out_v, string_create_from(buf));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERAND_NUMBER_FLOAT: {
    string_t *out_v = vector_reserve(string_t, 1);
    char buf[1024] = { 0 };
    if (ast->data.EXPR_OPERAND_NUMBER.typespec == nullptr)
      snprintf(buf, 1024, "EXPR_OPERAND_NUMBER_FLOAT { value: %lf, typespec: - }",
               ast->data.EXPR_OPERAND_NUMBER_FLOAT.value);
    else
      snprintf(buf, 1024, "EXPR_OPERAND_NUMBER_FLOAT { value: %lf, typespec: \"%s\" }",
               ast->data.EXPR_OPERAND_NUMBER_FLOAT.value,
               ast->data.EXPR_OPERAND_NUMBER_FLOAT.typespec);
    vector_push(out_v, string_create_from(buf));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERAND_ARRAY_ELEMENT: {
    string_t **children_vs_v = vector_reserve(string_t *, 2);
    vector_push(children_vs_v, to_string(ast->data.EXPR_OPERAND_ARRAY_ELEMENT.array));
    vector_push(children_vs_v, to_string(ast->data.EXPR_OPERAND_ARRAY_ELEMENT.index_expression));
    return build_tree(string_create_from("EXPR_OPERAND_ARRAY_ELEMENT"), children_vs_v);
  }

  case ALC_AST_KIND_EXPR_OPERAND_CAST_TO: {
    string_t **children_vs_v = vector_reserve(string_t *, 2);
    vector_push(children_vs_v, to_string(ast->data.EXPR_OPERAND_CAST_TO.type));
    vector_push(children_vs_v, to_string(ast->data.EXPR_OPERAND_CAST_TO.expression));
    return build_tree(string_create_from("EXPR_OPERAND_CAST_TO"), children_vs_v);
  }

  case ALC_AST_KIND_EXPR_OPERAND_CALL: {
    string_t **children_vs_v = safe_reserve(string_t *, ast->data.EXPR_OPERAND_CALL.arguments_num);
    array_to_strings(children_vs_v, ast->data.EXPR_OPERAND_CALL.arguments,
                     ast->data.EXPR_OPERAND_CALL.arguments_num);
    string_t header = string_create_from("EXPR_OPERAND_CALL { callee_name: \"");
    string_append_cstr(&header, ast->data.EXPR_OPERAND_CALL.callee_name);
    string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_EXPR_OPERAND_GENERIC_CALL: {
    string_t **children_vs_v =
      vector_reserve(string_t *, ast->data.EXPR_OPERAND_GENERIC_CALL.arguments_num + 1);
    vector_push(children_vs_v, to_string(ast->data.EXPR_OPERAND_GENERIC_CALL.generic_type_list));
    array_to_strings(children_vs_v, ast->data.EXPR_OPERAND_GENERIC_CALL.arguments,
                     ast->data.EXPR_OPERAND_GENERIC_CALL.arguments_num);
    string_t header = string_create_from("EXPR_OPERAND_GENERIC_CALL { callee_name: \"");
    string_append_cstr(&header, ast->data.EXPR_OPERAND_GENERIC_CALL.callee_name);
    string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_EXPR_OPERAND_STRING: {
    string_t *out_v = vector_reserve(string_t, 1);
    char buf[1024] = { 0 };
    if (ast->data.EXPR_OPERAND_STRING.typespec == nullptr)
      snprintf(buf, 1024, "EXPR_OPERAND_STRING { content: \"%s\", typespec: - }",
               ast->data.EXPR_OPERAND_STRING.content);
    else
      snprintf(buf, 1024, "EXPR_OPERAND_STRING { value: \"%s\", typespec: \"%s\" }",
               ast->data.EXPR_OPERAND_STRING.content, ast->data.EXPR_OPERAND_STRING.typespec);
    vector_push(out_v, string_create_from(buf));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERAND_SYMBOL: {
    string_t *out_v = vector_reserve(string_t, 1);
    char buf[1024] = { 0 };
    if (ast->data.EXPR_OPERAND_SYMBOL.typespec == nullptr)
      snprintf(buf, 1024, "EXPR_OPERAND_SYMBOL { content: \"%s\", typespec: - }",
               ast->data.EXPR_OPERAND_SYMBOL.content);
    else
      snprintf(buf, 1024, "EXPR_OPERAND_SYMBOL { value: \"%s\", typespec: \"%s\" }",
               ast->data.EXPR_OPERAND_SYMBOL.content, ast->data.EXPR_OPERAND_SYMBOL.typespec);
    vector_push(out_v, string_create_from(buf));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERAND_ACCESS_MEMBER: {
    string_t **children_vs_v = vector_reserve(string_t *, 2);
    vector_push(children_vs_v, to_string(ast->data.EXPR_OPERAND_ACCESS_MEMBER.from));
    vector_push(children_vs_v, to_string(ast->data.EXPR_OPERAND_ACCESS_MEMBER.what));
    return build_tree(string_create_from("EXPR_OPERAND_ACCESS_MEMBER"), children_vs_v);
  }

  case ALC_AST_KIND_EXPR_OPERAND_SIZE_OF: {
    string_t **children_vs_v = vector_reserve(string_t *, 1);
    vector_push(children_vs_v, to_string(ast->data.EXPR_OPERAND_SIZE_OF.type));
    return build_tree(string_create_from("EXPR_OPERAND_SIZE_OF"), children_vs_v);
  }

  case ALC_AST_KIND_EXPR_OPERAND_ALIGN_OF: {
    string_t **children_vs_v = vector_reserve(string_t *, 1);
    vector_push(children_vs_v, to_string(ast->data.EXPR_OPERAND_ALIGN_OF.expression));
    return build_tree(string_create_from("EXPR_OPERAND_ALIGN_OF"), children_vs_v);
  }

  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_ADD: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_BINARY_ADD"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_SUB: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_BINARY_SUB"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_MUL: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_BINARY_MUL"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_DIV: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_BINARY_DIV"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_MOD: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_BINARY_MOD"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_SHL: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_BINARY_SHL"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_SHR: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_BINARY_SHR"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_AND: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_BINARY_AND"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_OR: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_BINARY_OR"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_XOR: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_BINARY_XOR"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_COMPARE_EQ: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_COMPARE_EQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_COMPARE_NOTEQ: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_COMPARE_NOTEQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_COMPARE_LTHAN: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_COMPARE_LTHAN"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_COMPARE_GTHAN: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_COMPARE_GTHAN"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_COMPARE_LTHANEQ: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_COMPARE_LTHANEQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_COMPARE_GTHANEQ: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_COMPARE_GTHANEQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_BOOLEAN_AND: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_BOOLEAN_AND"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_BOOLEAN_OR: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_BOOLEAN_OR"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_EQ: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_ASSIGN_EQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_ADDEQ: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_ASSIGN_ADDEQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_SUBEQ: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_ASSIGN_SUBEQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_MULEQ: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_ASSIGN_MULEQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_DIVEQ: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_ASSIGN_DIVEQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_MODEQ: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_ASSIGN_MODEQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_SHLEQ: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_ASSIGN_SHLEQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_SHREQ: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_ASSIGN_SHREQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_ANDEQ: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_ASSIGN_ANDEQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_OREQ: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_ASSIGN_OREQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_XOREQ: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_ASSIGN_XOREQ"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_PREFIX_NOT: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_PREFIX_NOT"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_PREFIX_BOOLEAN_NOT: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_PREFIX_BOOLEAN_NOT"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_PREFIX_NEGATIVE: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_PREFIX_NEGATIVE"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_PREFIX_DEREFERENCE: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_PREFIX_DEREFERENCE"));
    return out_v;
  }

  case ALC_AST_KIND_EXPR_OPERATOR_PREFIX_ADDRESS: {
    string_t *out_v = vector_reserve(string_t, 1);
    vector_push(out_v, string_create_from("EXPR_OPERATOR_PREFIX_ADDRESS"));
    return out_v;
  }

  case ALC_AST_KIND_INITLIST: {
    string_t **children_vs_v = safe_reserve(string_t *, ast->data.INITLIST.entries_num);
    array_to_strings(children_vs_v, ast->data.INITLIST.entries, ast->data.INITLIST.entries_num);
    return build_tree(string_create_from("INITLIST"), children_vs_v);
  }

  case ALC_AST_KIND_INITLIST_ENTRY: {
    string_t **children_vs_v = vector_reserve(string_t *, 1);
    vector_push(children_vs_v, to_string(ast->data.INITLIST_ENTRY.expression));
    return build_tree(string_create_from("INITLIST_ENTRY"), children_vs_v);
  }

  case ALC_AST_KIND_INITLIST_ENTRY_EXPLICIT: {
    string_t **children_vs_v = vector_reserve(string_t *, 1);
    vector_push(children_vs_v, to_string(ast->data.INITLIST_ENTRY_EXPLICIT.expression));
    string_t header = string_create_from("INITLIST_ENTRY_EXPLICIT { field_name: \"");
    string_append_cstr(&header, ast->data.INITLIST_ENTRY_EXPLICIT.field_name);
    string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_INITLIST_ENTRY_EXPLICIT_ARRAY_ELEMENT: {
    string_t **children_vs_v = vector_reserve(string_t *, 2);
    vector_push(children_vs_v,
                to_string(ast->data.INITLIST_ENTRY_EXPLICIT_ARRAY_ELEMENT.index_expression));
    vector_push(children_vs_v,
                to_string(ast->data.INITLIST_ENTRY_EXPLICIT_ARRAY_ELEMENT.expression));
    return build_tree(string_create_from("INITLIST_ENTRY_EXPLICIT_ARRAY_ELEMENT"), children_vs_v);
  }

  case ALC_AST_KIND_GENERIC_STRUCT: {
    string_t **children_vs_v =
      vector_reserve(string_t *, ast->data.GENERIC_STRUCT.children_num + 2);
    add_to_strings_opt(children_vs_v, ast->data.GENERIC_STRUCT.attribute_list);
    vector_push(children_vs_v, to_string(ast->data.GENERIC_STRUCT.generic_placeholder_type_list));
    array_to_strings(children_vs_v, ast->data.GENERIC_STRUCT.children,
                     ast->data.GENERIC_STRUCT.children_num);
    string_t header = string_create_from("GENERIC_STRUCT { name: \"");
    string_append_cstr(&header, ast->data.GENERIC_STRUCT.name);
    string_append_cstr(&header, "\" }");
    return build_tree(string_create_from("INITLIST_ENTRY_EXPLICIT_ARRAY_ELEMENT"), children_vs_v);
  }

  case ALC_AST_KIND_GENERIC_FUNC: {
    string_t **children_vs_v = vector_reserve(string_t *, 5);
    add_to_strings_opt(children_vs_v, ast->data.GENERIC_FUNC.attribute_list);
    vector_push(children_vs_v, to_string(ast->data.GENERIC_FUNC.generic_placeholder_type_list));
    vector_push(children_vs_v, to_string(ast->data.GENERIC_FUNC.argument_list));
    add_to_strings_opt(children_vs_v, ast->data.GENERIC_FUNC.return_type);
    vector_push(children_vs_v, to_string(ast->data.GENERIC_FUNC.body));
    string_t header = string_create_from("GENERIC_FUNC { name: \"");
    string_append_cstr(&header, ast->data.GENERIC_FUNC.name);
    string_append_cstr(&header, "\", kind: ");
    string_append_cstr(&header, func_kind_to_string(ast->data.GENERIC_FUNC.kind));
    string_append_cstr(&header, " }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_GENERIC_PLACEHOLDER_TYPE_LIST: {
    string_t **children_vs_v = safe_reserve(
      string_t *, ast->data.GENERIC_PLACEHOLDER_TYPE_LIST.generic_placeholder_types_num);
    array_to_strings(children_vs_v,
                     ast->data.GENERIC_PLACEHOLDER_TYPE_LIST.generic_placeholder_types,
                     ast->data.GENERIC_PLACEHOLDER_TYPE_LIST.generic_placeholder_types_num);
    return build_tree(string_create_from("GENERIC_PLACEHOLDER_TYPE_LIST"), children_vs_v);
  }

  case ALC_AST_KIND_GENERIC_PLACEHOLDER_TYPE: {
    string_t **children_vs_v = vector_reserve(string_t *, 1);
    add_to_strings_opt(children_vs_v, ast->data.GENERIC_PLACEHOLDER_TYPE.default_type);
    string_t header = string_create_from("GENERIC_PLACEHOLDER_TYPE { name: \"");
    string_append_cstr(&header, ast->data.GENERIC_PLACEHOLDER_TYPE.name);
    string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_GENERIC_TYPE_LIST: {
    string_t **children_vs_v =
      safe_reserve(string_t *, ast->data.GENERIC_TYPE_LIST.generic_types_num);
    array_to_strings(children_vs_v, ast->data.GENERIC_TYPE_LIST.generic_types,
                     ast->data.GENERIC_TYPE_LIST.generic_types_num);
    return build_tree(string_create_from("GENERIC_TYPE_LIST"), children_vs_v);
  }

  case ALC_AST_KIND_GENERIC_TYPE: {
    string_t **children_vs_v = vector_reserve(string_t *, 1);
    vector_push(children_vs_v, to_string(ast->data.GENERIC_TYPE.generic_type_list));
    string_t header = string_create_from("GENERIC_TYPE { name: \"");
    string_append_cstr(&header, ast->data.GENERIC_TYPE.name);
    string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }

  case ALC_AST_KIND_GENERIC_NAMESPACE: {
    string_t **children_vs_v = vector_reserve(string_t *, 2);
    vector_push(children_vs_v, to_string(ast->data.GENERIC_NAMESPACE.generic_type_list));
    vector_push(children_vs_v, to_string(ast->data.GENERIC_NAMESPACE.subobject));
    string_t header = string_create_from("GENERIC_NAMESPACE { name: \"");
    string_append_cstr(&header, ast->data.GENERIC_NAMESPACE.name);
    string_append_cstr(&header, "\" }");
    return build_tree(header, children_vs_v);
  }
  }

  ALC_NOREACH();
}

static inline void *__safe_reserve_impl(usize stride, usize cap)
{
  ALC_ASSUME(stride > 0);
  return cap > 0 ? __vector_create_impl(stride, cap) : __vector_create_impl(stride, 1);
}
