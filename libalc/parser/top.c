#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

alc_ast_t *parse_top(alc_parser_t *p)
{
  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  switch (p->tokens[p->pos].type) {
  case ALC_TOKEN_TYPE_SEMICOLON: {
    alc_ast_t *none_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
    none_ast->pos = p->pos++;
    none_ast->kind = ALC_AST_KIND_NONE;
    return none_ast;
  }

  case ALC_TOKEN_TYPE_ID: {
    if (strcmp(p->tokens[p->pos].value, "import") == 0)
      return parse_import(p);

    ALC_TODO("Parse decldef");
  }

  case ALC_TOKEN_TYPE_LBRACK: {
    ALC_TODO("Parse decldef with attributes");
  }

  default:
    add_error_unexpected_token(p, p->pos, 0);
    return nullptr;
  }
}
