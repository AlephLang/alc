#include "alc/ast.h"
#include "alc/defs.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"

alc_ast_t *parse_stmt_defer(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos++;

  alc_ast_t *body = parse_stmt(p);
  _VERIFY_AST(body);

  alc_ast_t *defer_stmt_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  defer_stmt_ast->data.STMT_DEFER.body = body;
  defer_stmt_ast->pos = pos;
  defer_stmt_ast->kind = ALC_AST_KIND_STMT_DEFER;
  return defer_stmt_ast;
}
