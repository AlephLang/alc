#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "containers/vector.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

static alc_ast_t *parse_generic_placeholder_type(alc_parser_t *p);

alc_ast_t *parse_generic_placeholder_type_list(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  usize pos = p->pos;

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_LARROW) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_LARROW);
    return nullptr;
  }

  p->pos++;

  b8 first = true;
  alc_ast_t **placeholder_types = vector_create(alc_ast_t *);
  while (p->pos < p->tokens_num) {
    if (p->tokens[p->pos].type == ALC_TOKEN_TYPE_RARROW)
      break;

    if (!first) {
      if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_COMMA) {
        add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_COMMA);
        vector_destroy(placeholder_types);
        return nullptr;
      }

      p->pos++;

      if ALC_UNLIKELY (p->pos >= p->tokens_num) {
        add_error_unexpected_eof(p, p->pos);
        vector_destroy(placeholder_types);
        return nullptr;
      }
    }

    alc_ast_t *placeholder_type = parse_generic_placeholder_type(p);
    if ALC_UNLIKELY (placeholder_types == nullptr) {
      vector_destroy(placeholder_types);
      return nullptr;
    }

    vector_push(placeholder_types, placeholder_type);

    first = false;
  }

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    vector_destroy(placeholder_types);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_RARROW) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_RARROW);
    vector_destroy(placeholder_types);
    return nullptr;
  }

  p->pos++;

  alc_ast_t *generic_placeholder_type_list_ast =
    alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  generic_placeholder_type_list_ast->data.GENERIC_PLACEHOLDER_TYPE_LIST
    .generic_placeholder_types_num = 0;
  generic_placeholder_type_list_ast->data.GENERIC_PLACEHOLDER_TYPE_LIST.generic_placeholder_types =
    vector_get_length(placeholder_types) > 0 ?
      vector_to_array(placeholder_types,
                      &generic_placeholder_type_list_ast->data.GENERIC_PLACEHOLDER_TYPE_LIST
                         .generic_placeholder_types_num) :
      nullptr;
  generic_placeholder_type_list_ast->pos = pos;
  generic_placeholder_type_list_ast->kind = ALC_AST_KIND_GENERIC_PLACEHOLDER_TYPE_LIST;
  vector_destroy(placeholder_types);
  return generic_placeholder_type_list_ast;
}

alc_ast_t *parse_generic_type_list(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  usize pos = p->pos;

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_EXCLMARK) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_EXCLMARK);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].has_whitespace_after) {
    add_error_unexpected_whitespace(p, p->pos++, ALC_TOKEN_TYPE_LARROW);
    return nullptr;
  }

  p->pos++;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_LARROW) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_LARROW);
    return nullptr;
  }

  p->pos++;

  alc_ast_t **types_in_type_list = vector_create(alc_ast_t *);
  b8 first = true;
  while (p->pos < p->tokens_num) {
    if (p->tokens[p->pos].type == ALC_TOKEN_TYPE_RARROW)
      break;

    if (!first) {
      if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_COMMA) {
        add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_COMMA);
        vector_destroy(types_in_type_list);
        return nullptr;
      }

      p->pos++;

      if ALC_UNLIKELY (p->pos >= p->tokens_num) {
        add_error_unexpected_eof(p, p->pos);
        vector_destroy(types_in_type_list);
        return nullptr;
      }
    }

    alc_ast_t *type_in_type_list = parse_type(p);
    if ALC_UNLIKELY (type_in_type_list == nullptr) {
      vector_destroy(types_in_type_list);
      return nullptr;
    }

    vector_push(types_in_type_list, type_in_type_list);

    first = false;
  }

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    vector_destroy(types_in_type_list);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_RARROW) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_RARROW);
    vector_destroy(types_in_type_list);
    return nullptr;
  }

  p->pos++;

  alc_ast_t *generic_type_list = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  generic_type_list->data.GENERIC_TYPE_LIST.generic_types_num = 0;
  generic_type_list->data.GENERIC_TYPE_LIST.generic_types =
    vector_get_length(types_in_type_list) > 0 ?
      vector_to_array(types_in_type_list,
                      &generic_type_list->data.GENERIC_TYPE_LIST.generic_types_num) :
      nullptr;
  generic_type_list->pos = pos;
  generic_type_list->kind = ALC_AST_KIND_GENERIC_TYPE_LIST;
  vector_destroy(types_in_type_list);
  return generic_type_list;
}

static alc_ast_t *parse_generic_placeholder_type(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_ID) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_ID);
    return nullptr;
  }

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  usize pos = p->pos++;

  alc_ast_t *default_type = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_EQ) {
    p->pos++;

    default_type = parse_type(p);
    if ALC_UNLIKELY (default_type == nullptr)
      return nullptr;
  }

  alc_ast_t *generic_placeholder_type =
    alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + name_len);
  generic_placeholder_type->data.GENERIC_PLACEHOLDER_TYPE.name =
    (char *)generic_placeholder_type + sizeof(alc_ast_t);
  generic_placeholder_type->data.GENERIC_PLACEHOLDER_TYPE.default_type = default_type;
  generic_placeholder_type->pos = pos;
  generic_placeholder_type->kind = ALC_AST_KIND_GENERIC_PLACEHOLDER_TYPE;
  memcpy(generic_placeholder_type->data.GENERIC_PLACEHOLDER_TYPE.name, name, name_len);
  return generic_placeholder_type;
}
