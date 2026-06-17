#include "alc/ast.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "containers/vector.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

static alc_ast_t *parse_entry(alc_parser_t *p);
static alc_ast_t *parse_entry_default(alc_parser_t *p);
static alc_ast_t *parse_entry_explicit(alc_parser_t *p);
static alc_ast_t *parse_entry_explicit_array_elem(alc_parser_t *p);

alc_ast_t *parse_initlist(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos++;

  alc_ast_t **entries = vector_create(alc_ast_t *);
  while (p->pos < p->tokens_num) {
    if (p->tokens[p->pos].type == ALC_TOKEN_TYPE_RCBRACK)
      break;

    alc_ast_t *entry = parse_entry(p);
    if ALC_UNLIKELY (entry == nullptr) {
      vector_destroy(entries);
      return nullptr;
    }

    if ALC_UNLIKELY (p->pos >= p->tokens_num) {
      add_error_unexpected_eof(p, p->pos);
      vector_destroy(entries);
      return nullptr;
    }

    vector_push(entries, entry);

    if (p->tokens[p->pos].type != ALC_TOKEN_TYPE_COMMA)
      break;

    p->pos++;
  }

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    vector_destroy(entries);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_RCBRACK) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_RCBRACK);
    vector_destroy(entries);
    return nullptr;
  }

  p->pos++;

  alc_ast_t *initlist_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  initlist_ast->data.INITLIST.entries =
    vector_to_array(entries, &initlist_ast->data.INITLIST.entries_num);
  initlist_ast->pos = pos;
  initlist_ast->kind = ALC_AST_KIND_INITLIST;
  vector_destroy(entries);
  return initlist_ast;
}

static alc_ast_t *parse_entry(alc_parser_t *p)
{
  switch (p->tokens[p->pos].type) {
  case ALC_TOKEN_TYPE_PERIOD:
    return parse_entry_explicit(p);
  case ALC_TOKEN_TYPE_LBRACK:
    return parse_entry_explicit_array_elem(p);
  default:
    return parse_entry_default(p);
  }
}

static alc_ast_t *parse_entry_default(alc_parser_t *p)
{
  alc_ast_t *expr = p->tokens[p->pos].type == ALC_TOKEN_TYPE_LCBRACK ? parse_initlist(p) :
                                                                       parse_expr(p, false);
  if ALC_UNLIKELY (expr == nullptr)
    return nullptr;

  alc_ast_t *entry_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  entry_ast->data.INITLIST_ENTRY.expression = expr;
  entry_ast->pos = expr->pos;
  entry_ast->kind = ALC_AST_KIND_INITLIST_ENTRY;
  return entry_ast;
}

static alc_ast_t *parse_entry_explicit(alc_parser_t *p)
{
  if ALC_UNLIKELY (p->tokens[p->pos].has_whitespace_after) {
    add_error_unexpected_whitespace(p, p->pos++, ALC_TOKEN_TYPE_ID);
    return nullptr;
  }

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

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_EQ) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_EQ);
    return nullptr;
  }

  p->pos++;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  alc_ast_t *expr = p->tokens[p->pos].type == ALC_TOKEN_TYPE_LCBRACK ? parse_initlist(p) :
                                                                       parse_expr(p, false);
  if ALC_UNLIKELY (expr == nullptr)
    return nullptr;

  alc_ast_t *entry_explicit =
    alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + sizeof(char) * name_len);
  entry_explicit->data.INITLIST_ENTRY_EXPLICIT.field_name =
    (char *)entry_explicit + sizeof(alc_ast_t);
  entry_explicit->data.INITLIST_ENTRY_EXPLICIT.expression = expr;
  entry_explicit->pos = pos;
  entry_explicit->kind = ALC_AST_KIND_INITLIST_ENTRY_EXPLICIT;
  memcpy(entry_explicit->data.INITLIST_ENTRY_EXPLICIT.field_name, name, sizeof(char) * name_len);
  return entry_explicit;
}

static alc_ast_t *parse_entry_explicit_array_elem(alc_parser_t *p)
{
  usize pos = p->pos;

  alc_ast_t **index_expressions = vector_create(alc_ast_t *);
  while (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_LBRACK) {
    p->pos++;

    if ALC_UNLIKELY (p->pos >= p->tokens_num) {
      add_error_unexpected_eof(p, p->pos);
      vector_destroy(index_expressions);
      return nullptr;
    }

    alc_ast_t *expr = parse_expr(p, false);
    if ALC_UNLIKELY (expr == nullptr) {
      vector_destroy(index_expressions);
      return nullptr;
    }

    if ALC_UNLIKELY (p->pos >= p->tokens_num) {
      add_error_unexpected_eof(p, p->pos);
      vector_destroy(index_expressions);
      return nullptr;
    }

    if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_RBRACK) {
      add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_RBRACK);
      vector_destroy(index_expressions);
      return nullptr;
    }

    vector_push(index_expressions, expr);

    p->pos++;
  }

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    vector_destroy(index_expressions);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_EQ) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_EQ);
    vector_destroy(index_expressions);
    return nullptr;
  }

  p->pos++;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    vector_destroy(index_expressions);
    return nullptr;
  }

  alc_ast_t *expr = p->tokens[p->pos].type == ALC_TOKEN_TYPE_LCBRACK ? parse_initlist(p) :
                                                                       parse_expr(p, false);
  if ALC_UNLIKELY (expr == nullptr) {
    vector_destroy(index_expressions);
    return nullptr;
  }

  alc_ast_t *entry_explicit_array_element = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  entry_explicit_array_element->data.INITLIST_ENTRY_EXPLICIT_ARRAY_ELEMENT.index_expressions =
    vector_to_array(index_expressions,
                    &entry_explicit_array_element->data.INITLIST_ENTRY_EXPLICIT_ARRAY_ELEMENT
                       .index_expressions_num);
  entry_explicit_array_element->data.INITLIST_ENTRY_EXPLICIT_ARRAY_ELEMENT.expression = expr;
  entry_explicit_array_element->pos = pos;
  entry_explicit_array_element->kind = ALC_AST_KIND_INITLIST_ENTRY_EXPLICIT_ARRAY_ELEMENT;
  vector_destroy(index_expressions);
  return entry_explicit_array_element;
}
