#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"

Alc_Ast *parse_stmt_continue(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_SEMICOLON);

  p->pos++;

  Alc_Ast *stmt_continue_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  stmt_continue_ast->pos = pos;
  stmt_continue_ast->kind = ALC_AST_KIND_STMT_CONTINUE;
  return stmt_continue_ast;
}
