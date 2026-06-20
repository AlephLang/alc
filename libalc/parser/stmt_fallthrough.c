#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"

alc_ast_t *parse_stmt_fallthrough(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_SEMICOLON);

  p->pos++;

  alc_ast_t *stmt_fallthrough_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  stmt_fallthrough_ast->pos = pos;
  stmt_fallthrough_ast->kind = ALC_AST_KIND_STMT_FALLTHROUGH;
  return stmt_fallthrough_ast;
}
