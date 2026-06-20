#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

alc_ast_t *parse_stmt_foreach(alc_parser_t *p)
{
  usize pos = p->pos++;

  alc_ast_t *attribute_list = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_LBRACK) {
    attribute_list = parse_attribute_list(p);
    _VERIFY_AST(attribute_list);
  }

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_LPAREN);

  p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  const char *item_name = p->tokens[p->pos].value;
  usize item_name_len = strlen(item_name) + 1;

  p->pos++;

  const char *i_name = nullptr;
  usize i_name_len = 0;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_COMMA) {
    p->pos++;

    _VERIFY_POS(p, p->pos);
    _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

    i_name = p->tokens[p->pos].value;
    i_name_len = strlen(i_name) + 1;

    p->pos++;
  }

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COLON);

  p->pos++;

  alc_ast_t *expr = parse_expr(p, false);
  _VERIFY_AST(expr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RPAREN);

  p->pos++;

  alc_ast_t *body = parse_stmt(p);
  _VERIFY_AST(body);

  alc_ast_t *stmt_foreach_ast = alloc_arena_allocate(
    &ctx()->arena, sizeof(alc_ast_t) + sizeof(char) * (item_name_len + i_name_len));
  stmt_foreach_ast->data.STMT_FOREACH.item_name = (char *)stmt_foreach_ast + sizeof(alc_ast_t);
  stmt_foreach_ast->data.STMT_FOREACH.iterator = expr;
  stmt_foreach_ast->data.STMT_FOREACH.body = body;
  stmt_foreach_ast->data.STMT_FOREACH.attribute_list = attribute_list;
  stmt_foreach_ast->data.STMT_FOREACH.i_name =
    i_name != nullptr ?
      (char *)stmt_foreach_ast + sizeof(alc_ast_t) + sizeof(char) * item_name_len :
      nullptr;
  stmt_foreach_ast->pos = pos;
  stmt_foreach_ast->kind = ALC_AST_KIND_STMT_FOREACH;
  memcpy(stmt_foreach_ast->data.STMT_FOREACH.item_name, item_name, sizeof(char) * item_name_len);
  if (i_name != nullptr)
    memcpy(stmt_foreach_ast->data.STMT_FOREACH.i_name, i_name, sizeof(char) * i_name_len);
  return stmt_foreach_ast;
}
