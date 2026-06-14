#include "alc/ast.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

alc_ast_t *parse_module(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_ID) {
    add_error_unexpected_token(p, p->pos, 1, ALC_TOKEN_TYPE_ID);
    return nullptr;
  }

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  usize pos = p->pos++;

  alc_ast_t *submodule = nullptr;

  alc_token_t *peeked = peek(p, 0);
  if (peeked != nullptr && peeked->type == ALC_TOKEN_TYPE_COLON) {
    if ALC_UNLIKELY (peeked->has_whitespace_after) {
      add_error_unexpected_whitespace(p, p->pos, ALC_TOKEN_TYPE_COLON);
      return nullptr;
    }

    peeked = peek(p, 1);
    if ALC_UNLIKELY (peeked == nullptr) {
      add_error_unexpected_eof(p, p->pos + 1);
      return nullptr;
    }

    if ALC_UNLIKELY (peeked->type != ALC_TOKEN_TYPE_COLON) {
      add_error_unexpected_token(p, p->pos + 1, 1, ALC_TOKEN_TYPE_COLON);
      return nullptr;
    }

    if ALC_UNLIKELY (peeked->has_whitespace_after) {
      add_error_unexpected_whitespace(p, p->pos + 1, ALC_TOKEN_TYPE_ID);
      return nullptr;
    }

    p->pos += 2;
    submodule = parse_module(p);

    if ALC_UNLIKELY (submodule == nullptr)
      return nullptr;
  }

  alc_ast_t *module_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + name_len);
  module_ast->data.MODULE.name = (char *)module_ast + sizeof(alc_ast_t);
  module_ast->data.MODULE.submodule = submodule;
  module_ast->pos = pos;
  module_ast->kind = ALC_AST_KIND_MODULE;
  memcpy(module_ast->data.MODULE.name, name, name_len);

  return module_ast;
}
