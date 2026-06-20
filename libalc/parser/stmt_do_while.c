#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"

alc_ast_t *parse_stmt_do_while(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos++;

  alc_ast_t *body = parse_stmt(p);
  _VERIFY_AST(body);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);
  _VERIFY_VALUE(p, p->pos, "while");

  p->pos++;

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

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_SEMICOLON);

  p->pos++;

  alc_ast_t *stmt_do_while_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  stmt_do_while_ast->data.STMT_DO_WHILE.condition = cond_expr;
  stmt_do_while_ast->data.STMT_DO_WHILE.body = body;
  stmt_do_while_ast->data.STMT_DO_WHILE.attribute_list = attribute_list;
  stmt_do_while_ast->pos = pos;
  stmt_do_while_ast->kind = ALC_AST_KIND_STMT_DO_WHILE;
  return stmt_do_while_ast;
}
