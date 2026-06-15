#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

alc_ast_t *parse_typedef(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos++;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  alc_ast_t *attribute_list = nullptr;
  if (p->tokens[p->pos].type == ALC_TOKEN_TYPE_LBRACK) {
    attribute_list = parse_attribute_list(p);
    if ALC_UNLIKELY (attribute_list == nullptr)
      return nullptr;

    if ALC_UNLIKELY (p->pos >= p->tokens_num) {
      add_error_unexpected_eof(p, p->pos);
      return nullptr;
    }
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_ID) {
    add_error_unexpected_token(p, p->pos, 1, ALC_TOKEN_TYPE_ID);
    return nullptr;
  }

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;
  p->pos++;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  alc_ast_t *generic_placeholder_type_list = nullptr;
  if (p->tokens[p->pos].type == ALC_TOKEN_TYPE_LARROW) {
    generic_placeholder_type_list = parse_generic_placeholder_type_list(p);
    if ALC_UNLIKELY (generic_placeholder_type_list == nullptr)
      return nullptr;

    if ALC_UNLIKELY (p->pos >= p->tokens_num) {
      add_error_unexpected_eof(p, p->pos);
      return nullptr;
    }
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_EQ) {
    add_error_unexpected_token(p, p->pos, 1, ALC_TOKEN_TYPE_EQ);
    return nullptr;
  }

  p->pos++;
  alc_ast_t *aliased_type = parse_type(p);
  if ALC_UNLIKELY (aliased_type == nullptr)
    return nullptr;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_SEMICOLON) {
    add_error_unexpected_token(p, p->pos, 1, ALC_TOKEN_TYPE_SEMICOLON);
    return nullptr;
  }

  p->pos++;

  alc_ast_t *typedef_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + name_len);
  typedef_ast->data.TYPEDEF.name = (char *)typedef_ast + sizeof(alc_ast_t);
  typedef_ast->data.TYPEDEF.aliased_type = aliased_type;
  typedef_ast->data.TYPEDEF.generic_placeholder_type_list = generic_placeholder_type_list;
  typedef_ast->data.TYPEDEF.attribute_list = attribute_list;
  typedef_ast->pos = pos;
  typedef_ast->kind = ALC_AST_KIND_TYPEDEF;
  memcpy(typedef_ast->data.TYPEDEF.name, name, name_len);
  return typedef_ast;
}
