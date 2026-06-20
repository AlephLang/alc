#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"

alc_ast_t *parse_stmt_goto(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos++;

  alc_ast_t *label = parse_label(p);
  _VERIFY_AST(label);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_SEMICOLON);

  p->pos++;

  alc_ast_t *stmt_goto_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  stmt_goto_ast->data.STMT_GOTO.label = label;
  stmt_goto_ast->pos = pos;
  stmt_goto_ast->kind = ALC_AST_KIND_STMT_GOTO;
  return stmt_goto_ast;
}
