#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"

alc_ast_t *parse_stmt_while(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos++;

  alc_ast_t *attribute_list = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_LBRACK) {
    attribute_list = parse_attribute_list(p);
    _VERIFY_AST(attribute_list);
  }

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_LPAREN);

  p->pos++;

  alc_ast_t *cond_expr = parse_expr(p, false);
  _VERIFY_AST(cond_expr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RPAREN);

  p->pos++;

  alc_ast_t *body = parse_stmt(p);
  _VERIFY_AST(body);

  alc_ast_t *stmt_while_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  stmt_while_ast->data.STMT_WHILE.condition = cond_expr;
  stmt_while_ast->data.STMT_WHILE.body = body;
  stmt_while_ast->data.STMT_WHILE.attribute_list = attribute_list;
  stmt_while_ast->pos = pos;
  stmt_while_ast->kind = ALC_AST_KIND_STMT_WHILE;
  return stmt_while_ast;
}
