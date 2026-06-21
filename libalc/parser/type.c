#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

static Alc_Ast *parse_function_pointer(Alc_Parser *p);
static Alc_Ast *parse_typeof(Alc_Parser *p);
static Alc_Ast *parse_generic_type_or_namespace(Alc_Parser *p);
static Alc_Ast *parse_namespace(Alc_Parser *p);
static Alc_Ast *parse_id(Alc_Parser *p);

Alc_Ast *parse_type_raw(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);

  return p->tokens[p->pos].type == ALC_TOKEN_TYPE_LPAREN ? parse_function_pointer(p) : parse_id(p);
}

static Alc_Ast *parse_id(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  if (strcmp(p->tokens[p->pos].value, "typeof") == 0)
    return parse_typeof(p);

  if (p->pos + 1 < p->tokens_num) {
    if (p->tokens[p->pos + 1].type == ALC_TOKEN_TYPE_EXCLMARK)
      return parse_generic_type_or_namespace(p);
    else if (p->tokens[p->pos + 1].type == ALC_TOKEN_TYPE_COLON)
      return parse_namespace(p);
  }

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;
  usize pos = p->pos++;

  Alc_Ast *type_plain_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + name_len);
  type_plain_ast->data.TYPE_PLAIN.name = (char *)type_plain_ast + sizeof(Alc_Ast);
  type_plain_ast->pos = pos;
  type_plain_ast->kind = ALC_AST_KIND_TYPE_PLAIN;
  memcpy(type_plain_ast->data.TYPE_PLAIN.name, name, name_len);
  return type_plain_ast;
}

Alc_Ast *parse_type(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);

  usize start_pos = p->pos;

  usize ptr_num = 0;
  for (; p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_ASTERISK;
       ptr_num++, p->pos++)
    ;

  Alc_Ast *type_raw = parse_type_raw(p);
  if ALC_UNLIKELY (type_raw == nullptr)
    return nullptr;

  Alc_Ast *cur_type = type_raw;

  for (; ptr_num; ptr_num--) {
    Alc_Ast *ptr_type = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
    ptr_type->data.TYPE_POINTER.type = cur_type;
    ptr_type->pos = start_pos + ptr_num - 1;
    ptr_type->kind = ALC_AST_KIND_TYPE_POINTER;
    cur_type = ptr_type;
  }

  while (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_LBRACK) {
    usize array_pos = p->pos++;

    _VERIFY_POS(p, p->pos);

    Alc_Ast *size_expression = nullptr;
    if (p->tokens[p->pos].type != ALC_TOKEN_TYPE_RBRACK) {
      size_expression = parse_expr(p, false);
      _VERIFY_AST(size_expression);
      _VERIFY_POS(p, p->pos);
    }

    _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RBRACK);

    p->pos++;

    Alc_Ast *array_type_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
    array_type_ast->data.TYPE_ARRAY.type = cur_type;
    array_type_ast->data.TYPE_ARRAY.size_expression = size_expression;
    array_type_ast->pos = array_pos;
    array_type_ast->kind = ALC_AST_KIND_TYPE_ARRAY;
    cur_type = array_type_ast;
  }

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

  Alc_Ast *function_pointer_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  function_pointer_ast->data.TYPE_FUNCTION_POINTER.argument_list = arguments;
  function_pointer_ast->data.TYPE_FUNCTION_POINTER.return_type = return_type;
  function_pointer_ast->pos = pos;
  function_pointer_ast->kind = ALC_AST_KIND_TYPE_FUNCTION_POINTER;
  return function_pointer_ast;
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

  Alc_Ast *typeof_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  typeof_ast->data.TYPE_TYPE_OF.expression = expr;
  typeof_ast->pos = pos;
  typeof_ast->kind = ALC_AST_KIND_TYPE_TYPE_OF;
  return typeof_ast;
}

static Alc_Ast *parse_generic_type_or_namespace(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  usize pos = p->pos;

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  p->pos++;

  Alc_Ast *generic_type_list = parse_generic_type_list(p);
  _VERIFY_AST(generic_type_list);

  Alc_Ast *generic_type_ast; // Declare it here to make compiler happy and not worry about GOTOs.
  if (p->pos < p->tokens_num) {
    b8 prev_has_ws = p->tokens[p->pos - 1].has_whitespace_after;
    if (prev_has_ws)
      goto __generic_type_ast;

    if (p->tokens[p->pos].type != ALC_TOKEN_TYPE_COLON)
      goto __generic_type_ast;

    _VERIFY_NO_WS(p, p->pos, ALC_TOKEN_TYPE_COLON);

    p->pos++;

    _VERIFY_POS(p, p->pos);
    _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COLON);
    _VERIFY_NO_WS(p, p->pos, ALC_TOKEN_TYPE_ID);

    p->pos++;

    Alc_Ast *subobject = parse_id(p);
    _VERIFY_AST(subobject);

    Alc_Ast *generic_namespace_ast =
      alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + name_len);
    generic_namespace_ast->data.GENERIC_NAMESPACE.name =
      (char *)generic_namespace_ast + sizeof(Alc_Ast);
    generic_namespace_ast->data.GENERIC_NAMESPACE.generic_type_list = generic_type_list;
    generic_namespace_ast->data.GENERIC_NAMESPACE.subobject = subobject;
    generic_namespace_ast->pos = pos;
    generic_namespace_ast->kind = ALC_AST_KIND_GENERIC_NAMESPACE;
    memcpy(generic_namespace_ast->data.GENERIC_NAMESPACE.name, name, name_len);
    return generic_namespace_ast;
  }

__generic_type_ast:
  generic_type_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + name_len);
  generic_type_ast->data.GENERIC_TYPE.name = (char *)generic_type_ast + sizeof(Alc_Ast);
  generic_type_ast->data.GENERIC_TYPE.generic_type_list = generic_type_list;
  generic_type_ast->pos = pos;
  generic_type_ast->kind = ALC_AST_KIND_GENERIC_TYPE;
  memcpy(generic_type_ast->data.GENERIC_TYPE.name, name, name_len);
  return generic_type_ast;
}

static Alc_Ast *parse_namespace(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);
  _VERIFY_NO_WS(p, p->pos, ALC_TOKEN_TYPE_COLON);

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  usize pos = p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COLON);
  _VERIFY_NO_WS(p, p->pos, ALC_TOKEN_TYPE_COLON);

  p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COLON);
  _VERIFY_NO_WS(p, p->pos, ALC_TOKEN_TYPE_ID);

  p->pos++;

  Alc_Ast *subobject = parse_id(p);
  _VERIFY_AST(subobject);

  Alc_Ast *namespace_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + name_len);
  namespace_ast->data.NAMESPACE.name = (char *)namespace_ast + sizeof(Alc_Ast);
  namespace_ast->data.NAMESPACE.subobject = subobject;
  namespace_ast->pos = pos;
  namespace_ast->kind = ALC_AST_KIND_NAMESPACE;
  memcpy(namespace_ast->data.NAMESPACE.name, name, name_len);
  return namespace_ast;
}
