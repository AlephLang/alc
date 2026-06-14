#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "containers/vector.h"
#include "global.h"
#include "parser/parser_private.h"

alc_ast_t *parse_generic_placeholder_type_list(alc_parser_t *p)
{
  ALC_UNUSED_DEBUG(p);
  ALC_TODO("Parse generic placeholder type list");
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
