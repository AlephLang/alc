#include "alc/ast.h"
#include "alc/defs.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"

Alc_Ast *parse_stmt_loop(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos++;

  Alc_Ast *body = parse_stmt(p);
  _VERIFY_AST(body);

  Alc_Ast *stmt_loop_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  stmt_loop_ast->data.STMT_LOOP.body = body;
  stmt_loop_ast->pos = pos;
  stmt_loop_ast->kind = ALC_AST_KIND_STMT_LOOP;
  return stmt_loop_ast;
}
