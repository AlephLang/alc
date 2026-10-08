#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "alc/vector.h"
#include "alc/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

static Alc_Ast *parse_function_pointer(Alc_Parser *p);
static Alc_Ast *parse_tuple(Alc_Parser *p);
static Alc_Ast *parse_typeof(Alc_Parser *p);
static Alc_Ast *parse_nonnull(Alc_Parser *p);
static Alc_Ast *parse_package_or_type(Alc_Parser *p);
static Alc_Ast *parse_id(Alc_Parser *p);

Alc_Ast *parse_type_raw(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);

  switch (p->tokens[p->pos].type) {
  case ALC_TOKEN_TYPE_LPAREN:
    return parse_function_pointer(p);

  case ALC_TOKEN_TYPE_PIPE:
    return parse_tuple(p);

  case ALC_TOKEN_TYPE_ID: {
    const char *value = p->tokens[p->pos].value;
    if (strcmp(value, "typeof") == 0)
      return parse_typeof(p);
    return parse_package_or_type(p);
  }

  default: {
    Alc_Vector(Alc_Token_Type) expected_v = alc_vector_reserve(Alc_Token_Type, 3);
    alc_vector_push(expected_v, ALC_TOKEN_TYPE_ID);
    alc_vector_push(expected_v, ALC_TOKEN_TYPE_PIPE);
    alc_vector_push(expected_v, ALC_TOKEN_TYPE_LPAREN);
    add_error_unexpected_token_v(p, p->pos, expected_v);
    return nullptr;
  }
  }
}

static Alc_Ast *parse_package_or_type(Alc_Parser *p)
{
  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  if (p->pos + 2 < p->tokens_num && p->tokens[p->pos + 1].type == ALC_TOKEN_TYPE_COLON &&
      p->tokens[p->pos + 2].type == ALC_TOKEN_TYPE_COLON) {
    const char *package_name = p->tokens[p->pos].value;
    usize package_name_len = strlen(package_name) + 1;

    usize pos = p->pos++;

    _VERIFY_POS(p, p->pos);
    _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COLON);
    _VERIFY_NO_WS(p, p->pos, ALC_TOKEN_TYPE_COLON);

    p->pos++;

    _VERIFY_POS(p, p->pos);
    _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COLON);

    p->pos++;

    Alc_Ast *id_type_ast = parse_id(p);
    _VERIFY_AST(id_type_ast);

    Alc_Ast *package_ast =
      alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + (sizeof(char) * package_name_len));
    package_ast->TYPE_PACKAGE.name = (char *)package_ast + sizeof(Alc_Ast);
    package_ast->TYPE_PACKAGE.symbol = id_type_ast;
    package_ast->pos = pos;
    package_ast->kind = ALC_AST_KIND_TYPE_PACKAGE;
    memcpy(package_ast->TYPE_PACKAGE.name, package_name, sizeof(char) * package_name_len);

    return package_ast;
  }

  return parse_id(p);
}

static Alc_Ast *parse_id(Alc_Parser *p)
{
  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  Alc_Vector(Alc_Ast *) module_list = alc_vector_create(Alc_Ast *);
  while (p->pos < p->tokens_num) {
    _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID, { alc_vector_destroy(module_list); });

    if (p->pos + 1 >= p->tokens_num || p->tokens[p->pos + 1].type != ALC_TOKEN_TYPE_PERIOD)
      break;

    const char *module_name = p->tokens[p->pos].value;
    usize module_name_len = strlen(module_name) + 1;
    usize pos = p->pos;
    p->pos += 2;

    Alc_Ast *module_ast =
      alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + (sizeof(char) * module_name_len));
    module_ast->TYPE_MODULE.name = (char *)module_ast + sizeof(Alc_Ast);
    module_ast->TYPE_MODULE.symbol = nullptr;
    module_ast->pos = pos;
    module_ast->kind = ALC_AST_KIND_TYPE_MODULE;
    memcpy(module_ast->TYPE_MODULE.name, module_name, sizeof(char) * module_name_len);

    alc_vector_push(module_list, module_ast);
  }

  Alc_Ast *start_module = alc_vector_is_empty(module_list) ? nullptr : module_list[0];
  Alc_Ast *last_module = nullptr;
  for (usize i = 0, module_list_len = alc_vector_get_length(module_list); i < module_list_len;
       i++) {
    last_module = module_list[i];

    if (i + 1 == module_list_len)
      break;

    Alc_Ast *next_module = module_list[i + 1];
    last_module->TYPE_MODULE.symbol = next_module;
  }
  alc_vector_destroy(module_list);

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;
  usize pos = p->pos++;

  Alc_Ast *type_ast;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_EXCLMARK) {
    Alc_Ast *generic_type_list = parse_generic_type_list(p);
    _VERIFY_AST(generic_type_list);

    type_ast = alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + name_len);
    type_ast->GENERIC_TYPE.name = (char *)type_ast + sizeof(Alc_Ast);
    type_ast->GENERIC_TYPE.generic_type_list = generic_type_list;
    type_ast->pos = pos;
    type_ast->kind = ALC_AST_KIND_GENERIC_TYPE;
    memcpy(type_ast->GENERIC_TYPE.name, name, name_len);
  } else {
    type_ast = alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + name_len);
    type_ast->TYPE_PLAIN.name = (char *)type_ast + sizeof(Alc_Ast);
    type_ast->pos = pos;
    type_ast->kind = ALC_AST_KIND_TYPE_PLAIN;
    memcpy(type_ast->TYPE_PLAIN.name, name, name_len);
  }

  if (last_module != nullptr)
    last_module->TYPE_MODULE.symbol = type_ast;

  return start_module != nullptr ? start_module : type_ast;
}

Alc_Ast *parse_type(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);

  if (p->tokens[p->pos].type == ALC_TOKEN_TYPE_ID &&
      strcmp(p->tokens[p->pos].value, "nonnull") == 0) {
    return parse_nonnull(p);
  }

  struct Array_Ast_And_Pos {
    Alc_Ast *size_expression;
    usize pos;
  };
  Alc_Vector(struct Array_Ast_And_Pos) arrays_v = alc_vector_create(struct Array_Ast_And_Pos);
  while (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_LBRACK) {
    usize pos = p->pos++;

    Alc_Ast *size_expression = nullptr;
    if (p->pos < p->tokens_num && p->tokens[p->pos].type != ALC_TOKEN_TYPE_RBRACK) {
      size_expression = parse_expr(p, false);
      _VERIFY_AST(size_expression, { alc_vector_destroy(arrays_v); });
    }

    _VERIFY_POS(p, p->pos, { alc_vector_destroy(arrays_v); });
    _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RBRACK, { alc_vector_destroy(arrays_v); });

    p->pos++;

    struct Array_Ast_And_Pos array = {
      .size_expression = size_expression,
      .pos = pos,
    };
    alc_vector_push(arrays_v, array);
  }

  usize ptr_start_pos = p->pos;

  usize ptr_num = 0;
  for (; p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_ASTERISK;
       ptr_num++, p->pos++)
    ;

  Alc_Ast *type_raw = parse_type_raw(p);
  _VERIFY_AST(type_raw, { alc_vector_destroy(arrays_v); });

  Alc_Ast *cur_type = type_raw;

  for (; ptr_num; ptr_num--) {
    Alc_Ast *ptr_type = alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
    ptr_type->TYPE_POINTER.type = cur_type;
    ptr_type->pos = ptr_start_pos + ptr_num - 1;
    ptr_type->kind = ALC_AST_KIND_TYPE_POINTER;
    cur_type = ptr_type;
  }

  for (usize i = 0, arrays_v_len = alc_vector_get_length(arrays_v); i < arrays_v_len; i++) {
    struct Array_Ast_And_Pos *array = &arrays_v[arrays_v_len - i - 1];

    Alc_Ast *array_type = alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
    array_type->TYPE_ARRAY.type = cur_type;
    array_type->TYPE_ARRAY.size_expression = array->size_expression;
    array_type->pos = array->pos;
    array_type->kind = ALC_AST_KIND_TYPE_ARRAY;
    cur_type = array_type;
  }

  alc_vector_destroy(arrays_v);

  return cur_type;
}

static Alc_Ast *parse_function_pointer(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);

  usize pos = p->pos;

  Alc_Ast *arguments = parse_function_arguments(p);
  _VERIFY_AST(arguments);

  Alc_Ast *return_type = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_MINUS) {
    _VERIFY_NO_WS(p, p->pos, ALC_TOKEN_TYPE_RARROW);

    p->pos++;

    _VERIFY_POS(p, p->pos);
    _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RARROW);

    p->pos++;

    return_type = parse_type(p);
    _VERIFY_AST(return_type);
  }

  Alc_Ast *function_pointer_ast = alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  function_pointer_ast->TYPE_FUNCTION_POINTER.argument_list = arguments;
  function_pointer_ast->TYPE_FUNCTION_POINTER.return_type = return_type;
  function_pointer_ast->pos = pos;
  function_pointer_ast->kind = ALC_AST_KIND_TYPE_FUNCTION_POINTER;
  return function_pointer_ast;
}

static Alc_Ast *parse_tuple(Alc_Parser *p)
{
  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_PIPE);

  usize pos = p->pos++;
  Alc_Vector(Alc_Ast *) types_v = alc_vector_create(Alc_Ast *);

  b8 first = true;
  while (p->pos < p->tokens_num && (p->tokens[p->pos].type != ALC_TOKEN_TYPE_PIPE || first)) {
    if ALC_LIKELY (!first) {
      _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COMMA, { alc_vector_destroy(types_v); });

      p->pos++;
      _VERIFY_POS(p, p->pos, { alc_vector_destroy(types_v); });
    }

    Alc_Ast *type_ast = parse_type(p);
    _VERIFY_AST(type_ast, { alc_vector_destroy(types_v); });

    alc_vector_push(types_v, type_ast);

    first = false;
  }

  _VERIFY_POS(p, p->pos, { alc_vector_destroy(types_v); });
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_PIPE, { alc_vector_destroy(types_v); });
  p->pos++;

  Alc_Ast *tuple_ast = alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  tuple_ast->TYPE_TUPLE.types = alc_vector_to_array(types_v, &tuple_ast->TYPE_TUPLE.types_num);
  tuple_ast->pos = pos;
  tuple_ast->kind = ALC_AST_KIND_TYPE_TUPLE;
  alc_vector_destroy(types_v);
  return tuple_ast;
}

static Alc_Ast *parse_typeof(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);
  _VERIFY_VALUE(p, p->pos, "typeof");

  usize pos = p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_LPAREN);

  p->pos++;

  Alc_Ast *expr = parse_expr(p, false);
  _VERIFY_AST(expr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RPAREN);

  p->pos++;

  Alc_Ast *typeof_ast = alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  typeof_ast->TYPE_TYPE_OF.expression = expr;
  typeof_ast->pos = pos;
  typeof_ast->kind = ALC_AST_KIND_TYPE_TYPE_OF;
  return typeof_ast;
}

static Alc_Ast *parse_nonnull(Alc_Parser *p)
{
  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);
  _VERIFY_VALUE(p, p->pos, "nonnull");

  usize pos = p->pos++;

  Alc_Ast *type = parse_type(p);
  _VERIFY_AST(type);

  Alc_Ast *nonnull_ast = alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  nonnull_ast->TYPE_NONNULL.type = type;
  nonnull_ast->pos = pos;
  nonnull_ast->kind = ALC_AST_KIND_TYPE_NONNULL;

  return nonnull_ast;
}
