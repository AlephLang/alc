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

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  usize pos = p->pos++;

  alc_ast_t *attribute_list = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_LBRACK) {
    attribute_list = parse_attribute_list(p);
    _VERIFY_AST(attribute_list);
  }

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_LCBRACK);

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
    alc_token_type_t *expected_v = vector_reserve(alc_token_type_t, 2);
    vector_push(expected_v, ALC_TOKEN_TYPE_RCBRACK);
    vector_push(expected_v, ALC_TOKEN_TYPE_COMMA);
    add_error_unexpected_token_v(p, p->pos++, expected_v);
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
  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  usize pos = p->pos++;

  alc_ast_t *expr = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_EQ) {
    p->pos++;

    expr = parse_expr(p, false);
    _VERIFY_AST(expr);
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
