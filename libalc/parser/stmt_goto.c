#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"

Alc_Ast *parse_stmt_goto(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos++;

  Alc_Ast *label = parse_label(p);
  _VERIFY_AST(label);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_SEMICOLON);

  p->pos++;

  Alc_Ast *stmt_goto_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  stmt_goto_ast->data.STMT_GOTO.label = label;
  stmt_goto_ast->pos = pos;
  stmt_goto_ast->kind = ALC_AST_KIND_STMT_GOTO;
  return stmt_goto_ast;
}
