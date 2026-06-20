#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

alc_ast_t *parse_stmt_if(alc_parser_t *p)
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

  alc_ast_t *stmt_else = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_ID &&
      strcmp(p->tokens[p->pos].value, "else") == 0) {
    stmt_else = parse_stmt_else(p);
    _VERIFY_AST(stmt_else);
  }

  alc_ast_t *stmt_if_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  stmt_if_ast->data.STMT_IF.condition = cond_expr;
  stmt_if_ast->data.STMT_IF.body = body;
  stmt_if_ast->data.STMT_IF.else_statement = stmt_else;
  stmt_if_ast->data.STMT_IF.attribute_list = attribute_list;
  stmt_if_ast->pos = pos;
  stmt_if_ast->kind = ALC_AST_KIND_STMT_IF;
  return stmt_if_ast;
}
