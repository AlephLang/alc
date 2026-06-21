#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"

Alc_Ast *parse_stmt_return(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos++;

  Alc_Ast *expr = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type != ALC_TOKEN_TYPE_SEMICOLON) {
    expr = p->tokens[p->pos].type == ALC_TOKEN_TYPE_LCBRACK ? parse_initlist(p) :
                                                              parse_expr(p, false);
    _VERIFY_AST(expr);
  }

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_SEMICOLON);

  p->pos++;

  Alc_Ast *return_stmt_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  return_stmt_ast->data.STMT_RETURN.expression = expr;
  return_stmt_ast->pos = pos;
  return_stmt_ast->kind = ALC_AST_KIND_STMT_RETURN;
  return return_stmt_ast;
}
