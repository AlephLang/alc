#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"

alc_ast_t *parse_stmt_for(alc_parser_t *p)
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

  alc_ast_t *init_statement = parse_stmt(p);
  _VERIFY_AST(init_statement);

  alc_ast_t *condition = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type != ALC_TOKEN_TYPE_SEMICOLON) {
    condition = parse_expr(p, false);
    _VERIFY_AST(condition);
  }

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_SEMICOLON);

  p->pos++;

  alc_ast_t *expression = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type != ALC_TOKEN_TYPE_RPAREN) {
    expression = parse_expr(p, true);
    _VERIFY_AST(expression);
  }

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RPAREN);

  p->pos++;

  alc_ast_t *body = parse_stmt(p);
  _VERIFY_AST(body);

  alc_ast_t *stmt_for_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  stmt_for_ast->data.STMT_FOR.init_statement = init_statement;
  stmt_for_ast->data.STMT_FOR.condition = condition;
  stmt_for_ast->data.STMT_FOR.expression = expression;
  stmt_for_ast->data.STMT_FOR.body = body;
  stmt_for_ast->data.STMT_FOR.attribute_list = attribute_list;
  stmt_for_ast->pos = pos;
  stmt_for_ast->kind = ALC_AST_KIND_STMT_FOR;
  return stmt_for_ast;
}
