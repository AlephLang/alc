#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"

alc_ast_t *parse_expr(alc_parser_t *p, b8 is_toplevel)
{
  ALC_UNUSED_DEBUG(p);
  ALC_UNUSED_DEBUG(is_toplevel);
  ALC_TODO("Parse expression");
}

alc_ast_t *parse_stmt_expr(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  alc_ast_t *expr = parse_expr(p, true);
  if ALC_UNLIKELY (expr == nullptr)
    return nullptr;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_SEMICOLON) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_SEMICOLON);
    return nullptr;
  }

  p->pos++;

  alc_ast_t *stmt_expr = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  stmt_expr->data.STMT_EXPR.expression = expr;
  stmt_expr->pos = expr->pos;
  stmt_expr->kind = ALC_AST_KIND_STMT_EXPR;
  return stmt_expr;
}
