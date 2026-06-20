#include "alc/ast.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

alc_ast_t *parse_module(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;
  b8 has_ws = p->tokens[p->pos].has_whitespace_after;

  usize pos = p->pos++;

  alc_ast_t *submodule = nullptr;

  if (!has_ws && p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_COLON) {
    _VERIFY_NO_WS(p, p->pos, ALC_TOKEN_TYPE_COLON);
    p->pos++;

    _VERIFY_POS(p, p->pos);
    _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COLON);
    _VERIFY_NO_WS(p, p->pos, ALC_TOKEN_TYPE_ID);

    p->pos++;
    submodule = parse_module(p);
    _VERIFY_AST(submodule);
  }

  alc_ast_t *module_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + name_len);
  module_ast->data.MODULE.name = (char *)module_ast + sizeof(alc_ast_t);
  module_ast->data.MODULE.submodule = submodule;
  module_ast->pos = pos;
  module_ast->kind = ALC_AST_KIND_MODULE;
  memcpy(module_ast->data.MODULE.name, name, name_len);

  return module_ast;
}
