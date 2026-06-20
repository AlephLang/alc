#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

alc_ast_t *parse_scope(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  usize pos = p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_SEMICOLON);

  p->pos++;

  alc_ast_t *scope_ast =
    alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + sizeof(char) * name_len);
  scope_ast->data.SCOPE.type = (char *)scope_ast + sizeof(alc_ast_t);
  scope_ast->pos = pos;
  scope_ast->kind = ALC_AST_KIND_SCOPE;
  memcpy(scope_ast->data.SCOPE.type, name, sizeof(char) * name_len);
  return scope_ast;
}
