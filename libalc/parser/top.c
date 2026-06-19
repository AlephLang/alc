#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"

alc_ast_t *parse_top(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  switch (p->tokens[p->pos].type) {
  case ALC_TOKEN_TYPE_SEMICOLON: {
    alc_ast_t *none_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
    none_ast->pos = p->pos++;
    none_ast->kind = ALC_AST_KIND_NONE;
    return none_ast;
  }

  case ALC_TOKEN_TYPE_ID: {
    alc_ast_t *ids = parse_ids(p);
    return ids == (void *)-1 ? parse_decldef(p, nullptr) : ids;
  }

  case ALC_TOKEN_TYPE_LBRACK: {
    alc_ast_t *attribute_list = parse_attribute_list(p);
    if ALC_UNLIKELY (attribute_list == nullptr)
      return nullptr;

    if ALC_UNLIKELY (p->pos >= p->tokens_num) {
      add_error_unexpected_eof(p, p->pos);
      return nullptr;
    }

    return parse_decldef(p, attribute_list);
  }

  default:
    add_error_unexpected_token(p, p->pos++, 0);
    return nullptr;
  }
}
