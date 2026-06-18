#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "containers/vector.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

static alc_ast_t *parse_enum_element(alc_parser_t *p);

alc_ast_t *parse_enum(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  p->pos++;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_ID) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_ID);
    return nullptr;
  }

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  usize pos = p->pos++;

  alc_ast_t *attribute_list = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_LBRACK) {
    attribute_list = parse_attribute_list(p);
    if ALC_UNLIKELY (attribute_list == nullptr)
      return nullptr;
  }

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_LCBRACK) {
    add_error_unexpected_token(p, p->pos, 2, ALC_TOKEN_TYPE_LCBRACK, ALC_TOKEN_TYPE_LBRACK);
    return nullptr;
  }

  p->pos++;

  alc_ast_t **elements = vector_create(alc_ast_t *);
  while (p->pos < p->tokens_num && p->tokens[p->pos].type != ALC_TOKEN_TYPE_RCBRACK) {
    alc_ast_t *element = parse_enum_element(p);
    if ALC_UNLIKELY (element == nullptr) {
      vector_destroy(elements);
      return nullptr;
    }

    if ALC_UNLIKELY (p->pos >= p->tokens_num) {
      add_error_unexpected_eof(p, p->pos);
      vector_destroy(elements);
      return nullptr;
    }

    vector_push(elements, element);

    if (p->tokens[p->pos].type != ALC_TOKEN_TYPE_COMMA)
      break;

    p->pos++;
  }

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    vector_destroy(elements);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_RCBRACK) {
    add_error_unexpected_token(p, p->pos++, 2, ALC_TOKEN_TYPE_RCBRACK, ALC_TOKEN_TYPE_COMMA);
    vector_destroy(elements);
    return nullptr;
  }

  p->pos++;

  alc_ast_t *enum_ast =
    alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + sizeof(char) * name_len);
  enum_ast->data.ENUM.name = (char *)enum_ast + sizeof(alc_ast_t);
  enum_ast->data.ENUM.elements = vector_to_array(elements, &enum_ast->data.ENUM.elements_num);
  enum_ast->data.ENUM.attribute_list = attribute_list;
  enum_ast->pos = pos;
  enum_ast->kind = ALC_AST_KIND_ENUM;
  memcpy(enum_ast->data.ENUM.name, name, name_len);
  return enum_ast;
}

static alc_ast_t *parse_enum_element(alc_parser_t *p)
{
  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_ID) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_ID);
    return nullptr;
  }

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  usize pos = p->pos++;

  alc_ast_t *expr = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_EQ) {
    p->pos++;

    if ALC_UNLIKELY (p->pos >= p->tokens_num) {
      add_error_unexpected_eof(p, p->pos);
      return nullptr;
    }

    expr = parse_expr(p, false);
    if ALC_UNLIKELY (expr == nullptr)
      return nullptr;
  }

  alc_ast_t *enum_element =
    alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + sizeof(char) * name_len);
  enum_element->data.ENUM_ELEMENT.name = (char *)enum_element + sizeof(alc_ast_t);
  enum_element->data.ENUM_ELEMENT.expression = expr;
  enum_element->pos = pos;
  enum_element->kind = ALC_AST_KIND_ENUM_ELEMENT;
  memcpy(enum_element->data.ENUM_ELEMENT.name, name, name_len);
  return enum_element;
}
