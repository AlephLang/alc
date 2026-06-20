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

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_LARROW);

  usize pos = p->pos++;

  b8 first = true;
  alc_ast_t **placeholder_types = vector_create(alc_ast_t *);
  while (p->pos < p->tokens_num && p->tokens[p->pos].type != ALC_TOKEN_TYPE_RARROW) {
    if (!first) {
      _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COMMA, { vector_destroy(placeholder_types); });
      p->pos++;
    }

    alc_ast_t *placeholder_type = parse_generic_placeholder_type(p);
    _VERIFY_AST(placeholder_type, { vector_destroy(placeholder_types); });

    vector_push(placeholder_types, placeholder_type);

    first = false;
  }

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RARROW);

  p->pos++;

  alc_ast_t *generic_placeholder_type_list_ast =
    alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  generic_placeholder_type_list_ast->data.GENERIC_PLACEHOLDER_TYPE_LIST.generic_placeholder_types =
    vector_to_array(placeholder_types,
                    &generic_placeholder_type_list_ast->data.GENERIC_PLACEHOLDER_TYPE_LIST
                       .generic_placeholder_types_num);
  generic_placeholder_type_list_ast->pos = pos;
  generic_placeholder_type_list_ast->kind = ALC_AST_KIND_GENERIC_PLACEHOLDER_TYPE_LIST;
  vector_destroy(placeholder_types);
  return generic_placeholder_type_list_ast;
}

alc_ast_t *parse_generic_type_list(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_EXCLMARK);
  _VERIFY_NO_WS(p, p->pos, ALC_TOKEN_TYPE_LARROW);

  usize pos = p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_LARROW);

  p->pos++;

  alc_ast_t **types_in_type_list = vector_create(alc_ast_t *);
  b8 first = true;
  while (p->pos < p->tokens_num && p->tokens[p->pos].type != ALC_TOKEN_TYPE_RARROW) {
    if (!first) {
      _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COMMA, { vector_destroy(types_in_type_list); });
      p->pos++;
    }

    alc_ast_t *type_in_type_list = parse_type(p);
    _VERIFY_AST(type_in_type_list, { vector_destroy(types_in_type_list); });

    vector_push(types_in_type_list, type_in_type_list);

    first = false;
  }

  _VERIFY_POS(p, p->pos, { vector_destroy(types_in_type_list); });
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RARROW, { vector_destroy(types_in_type_list); });

  p->pos++;

  alc_ast_t *generic_type_list = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  generic_type_list->data.GENERIC_TYPE_LIST.generic_types = vector_to_array(
    types_in_type_list, &generic_type_list->data.GENERIC_TYPE_LIST.generic_types_num);
  generic_type_list->pos = pos;
  generic_type_list->kind = ALC_AST_KIND_GENERIC_TYPE_LIST;
  vector_destroy(types_in_type_list);
  return generic_type_list;
}

static alc_ast_t *parse_generic_placeholder_type(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  usize pos = p->pos++;

  alc_ast_t *default_type = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_EQ) {
    p->pos++;

    default_type = parse_type(p);
    _VERIFY_AST(default_type);
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
