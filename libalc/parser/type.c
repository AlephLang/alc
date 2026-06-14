#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

static alc_ast_t *parse_function_pointer(alc_parser_t *p);
static alc_ast_t *parse_typeof(alc_parser_t *p);
static alc_ast_t *parse_generic_type_or_namespace(alc_parser_t *p);
static alc_ast_t *parse_namespace(alc_parser_t *p);
static alc_ast_t *parse_id(alc_parser_t *p);

alc_ast_t *parse_type_raw(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if (p->tokens[p->pos].type == ALC_TOKEN_TYPE_LPAREN) {
    return parse_function_pointer(p);
  }

  return parse_id(p);
}

static alc_ast_t *parse_id(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_ID) {
    add_error_unexpected_token(p, p->pos++, 2, ALC_TOKEN_TYPE_ID, ALC_TOKEN_TYPE_LPAREN);
    return nullptr;
  }

  if (strcmp(p->tokens[p->pos].value, "typeof") == 0)
    return parse_typeof(p);

  alc_token_t *peeked = peek(p, 1);
  if (peeked != nullptr) {
    if (peeked->type == ALC_TOKEN_TYPE_EXCLMARK)
      return parse_generic_type_or_namespace(p);
    else if (peeked->type == ALC_TOKEN_TYPE_COLON)
      return parse_namespace(p);
  }

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;
  usize pos = p->pos++;

  alc_ast_t *type_plain_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + name_len);
  type_plain_ast->data.TYPE_PLAIN.name = (char *)type_plain_ast + sizeof(alc_ast_t);
  type_plain_ast->pos = pos;
  type_plain_ast->kind = ALC_AST_KIND_TYPE_PLAIN;
  memcpy(type_plain_ast->data.TYPE_PLAIN.name, name, name_len);
  return type_plain_ast;
}

alc_ast_t *parse_type(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  usize start_pos = p->pos;

  usize ptr_num = 0;
  for (; p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_ASTERISK;
       ptr_num++, p->pos++)
    ;

  alc_ast_t *type_raw = parse_type_raw(p);
  if ALC_UNLIKELY (type_raw == nullptr)
    return nullptr;

  alc_ast_t *cur_type = type_raw;

  for (; ptr_num; ptr_num--) {
    alc_ast_t *ptr_type = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
    ptr_type->data.TYPE_POINTER.type = cur_type;
    ptr_type->pos = start_pos + ptr_num - 1;
    ptr_type->kind = ALC_AST_KIND_TYPE_POINTER;
    cur_type = ptr_type;
  }

  while (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_LBRACK) {
    usize array_pos = p->pos++;

    if ALC_UNLIKELY (p->pos >= p->tokens_num) {
      add_error_unexpected_eof(p, p->pos);
      return nullptr;
    }

    alc_ast_t *size_expression = nullptr;
    if (p->tokens[p->pos].type != ALC_TOKEN_TYPE_RBRACK) {
      ALC_TODO("Parse array with size expression");
    }

    if (p->tokens[p->pos].type != ALC_TOKEN_TYPE_RBRACK) {
      add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_RBRACK);
      return nullptr;
    }

    p->pos++;

    alc_ast_t *array_type_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
    array_type_ast->data.TYPE_ARRAY.type = cur_type;
    array_type_ast->data.TYPE_ARRAY.size_expression = size_expression;
    array_type_ast->pos = array_pos;
    array_type_ast->kind = ALC_AST_KIND_TYPE_ARRAY;
    cur_type = array_type_ast;
  }

  return cur_type;
}

static alc_ast_t *parse_function_pointer(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos;

  alc_ast_t *arguments = parse_function_arguments(p);
  if ALC_UNLIKELY (arguments == nullptr)
    return nullptr;

  alc_ast_t *return_type = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_MINUS) {
    if ALC_UNLIKELY (p->tokens[p->pos].has_whitespace_after) {
      add_error_unexpected_whitespace(p, p->pos++, ALC_TOKEN_TYPE_RARROW);
      return nullptr;
    }

    p->pos++;

    if ALC_UNLIKELY (p->pos >= p->tokens_num) {
      add_error_unexpected_eof(p, p->pos);
      return nullptr;
    }

    if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_RARROW) {
      add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_RARROW);
      return nullptr;
    }

    p->pos++;

    return_type = parse_type(p);
    if ALC_UNLIKELY (return_type == nullptr)
      return nullptr;
  }

  alc_ast_t *function_pointer_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  function_pointer_ast->data.TYPE_FUNCTION_POINTER.argument_list = arguments;
  function_pointer_ast->data.TYPE_FUNCTION_POINTER.return_type = return_type;
  function_pointer_ast->pos = pos;
  function_pointer_ast->kind = ALC_AST_KIND_TYPE_FUNCTION_POINTER;
  return function_pointer_ast;
}

static alc_ast_t *parse_typeof(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos++;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_LPAREN) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_LPAREN);
    return nullptr;
  }

  p->pos++;

  alc_ast_t *expr = nullptr; // parse_expr(p, false);
  ALC_TODO("Parse expression in typeof");

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_RPAREN) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_RPAREN);
    return nullptr;
  }

  p->pos++;

  alc_ast_t *typeof_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  typeof_ast->data.TYPE_TYPE_OF.expression = expr;
  typeof_ast->pos = pos;
  typeof_ast->kind = ALC_AST_KIND_TYPE_TYPE_OF;
  return typeof_ast;
}

static alc_ast_t *parse_generic_type_or_namespace(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos;

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  b8 has_ws = p->tokens[p->pos].has_whitespace_after;

  if ALC_UNLIKELY (has_ws) {
    add_error_unexpected_whitespace(p, p->pos++, ALC_TOKEN_TYPE_EXCLMARK);
    return nullptr;
  }

  p->pos++;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  alc_ast_t *generic_type_list = parse_generic_type_list(p);
  if ALC_UNLIKELY (generic_type_list == nullptr)
    return nullptr;

  alc_ast_t *generic_type_ast; // Declare it here to make compiler happy and not worry about GOTOs.
  if (p->pos < p->tokens_num) {
    b8 prev_has_ws = p->tokens[p->pos - 1].has_whitespace_after;
    if (prev_has_ws)
      goto __generic_type_ast;

    if (p->tokens[p->pos].type != ALC_TOKEN_TYPE_COLON)
      goto __generic_type_ast;

    if ALC_UNLIKELY (p->tokens[p->pos].has_whitespace_after) {
      add_error_unexpected_whitespace(p, p->pos++, ALC_TOKEN_TYPE_COLON);
      return nullptr;
    }

    p->pos++;

    if ALC_UNLIKELY (p->pos >= p->tokens_num) {
      add_error_unexpected_eof(p, p->pos);
      return nullptr;
    }

    if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_COLON) {
      add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_COLON);
      return nullptr;
    }

    if ALC_UNLIKELY (p->tokens[p->pos].has_whitespace_after) {
      add_error_unexpected_whitespace(p, p->pos++, ALC_TOKEN_TYPE_ID);
      return nullptr;
    }

    p->pos++;

    alc_ast_t *subobject = parse_id(p);
    if ALC_UNLIKELY (subobject == nullptr)
      return nullptr;

    alc_ast_t *generic_namespace_ast =
      alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + name_len);
    generic_namespace_ast->data.GENERIC_NAMESPACE.name =
      (char *)generic_namespace_ast + sizeof(alc_ast_t);
    generic_namespace_ast->data.GENERIC_NAMESPACE.generic_type_list = generic_type_list;
    generic_namespace_ast->data.GENERIC_NAMESPACE.subobject = subobject;
    generic_namespace_ast->pos = pos;
    generic_namespace_ast->kind = ALC_AST_KIND_GENERIC_NAMESPACE;
    memcpy(generic_namespace_ast->data.GENERIC_NAMESPACE.name, name, name_len);
    return generic_namespace_ast;
  }

__generic_type_ast:
  generic_type_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + name_len);
  generic_type_ast->data.GENERIC_TYPE.name = (char *)generic_type_ast + sizeof(alc_ast_t);
  generic_type_ast->data.GENERIC_TYPE.generic_type_list = generic_type_list;
  generic_type_ast->pos = pos;
  generic_type_ast->kind = ALC_AST_KIND_GENERIC_TYPE;
  memcpy(generic_type_ast->data.GENERIC_TYPE.name, name, name_len);
  return generic_type_ast;
}

static alc_ast_t *parse_namespace(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos;

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;
  b8 has_ws = p->tokens[p->pos].has_whitespace_after;

  if ALC_UNLIKELY (has_ws) {
    add_error_unexpected_whitespace(p, p->pos++, ALC_TOKEN_TYPE_COLON);
    return nullptr;
  }

  p->pos++;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_COLON) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_COLON);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].has_whitespace_after) {
    add_error_unexpected_whitespace(p, p->pos++, ALC_TOKEN_TYPE_COLON);
    return nullptr;
  }

  p->pos++;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_COLON) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_COLON);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].has_whitespace_after) {
    add_error_unexpected_whitespace(p, p->pos++, ALC_TOKEN_TYPE_ID);
    return nullptr;
  }

  p->pos++;

  alc_ast_t *subobject = parse_id(p);
  if ALC_UNLIKELY (subobject == nullptr)
    return nullptr;

  alc_ast_t *namespace_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + name_len);
  namespace_ast->data.NAMESPACE.name = (char *)namespace_ast + sizeof(alc_ast_t);
  namespace_ast->data.NAMESPACE.subobject = subobject;
  namespace_ast->pos = pos;
  namespace_ast->kind = ALC_AST_KIND_NAMESPACE;
  memcpy(namespace_ast->data.NAMESPACE.name, name, name_len);
  return namespace_ast;
}
