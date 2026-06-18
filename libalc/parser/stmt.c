#include "alc/ast.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "containers/vector.h"
#include "global.h"
#include "parser/parser_private.h"

alc_ast_t *parse_stmt_block(alc_parser_t *p)
{
  usize pos = p->pos++;

  alc_ast_t **statements = vector_create(alc_ast_t *);
  while (p->pos < p->tokens_num && p->tokens[p->pos].type != ALC_TOKEN_TYPE_RCBRACK) {
    alc_ast_t *statement = parse_stmt(p);
    if ALC_UNLIKELY (statement == nullptr) {
      vector_destroy(statements);
      return nullptr;
    }
  }

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    vector_destroy(statements);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_RCBRACK) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_RCBRACK);
    vector_destroy(statements);
    return nullptr;
  }

  p->pos++;

  alc_ast_t *stmt_block = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  stmt_block->data.STMT_BLOCK.statements =
    vector_to_array(statements, &stmt_block->data.STMT_BLOCK.statements_num);
  stmt_block->pos = pos;
  stmt_block->kind = ALC_AST_KIND_STMT_BLOCK;
  vector_destroy(statements);
  return stmt_block;
}

alc_ast_t *parse_stmt(alc_parser_t *p)
{
  // TODO: More
  return parse_decldef(p, nullptr);
}
