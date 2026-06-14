#include "alc/ast.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"

alc_ast_t *parse_import(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos;

  p->pos++;
  alc_ast_t *module = parse_module(p);

  if ALC_UNLIKELY (module == nullptr)
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

  alc_ast_t *import_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  import_ast->data.IMPORT.module = module;
  import_ast->pos = pos;
  import_ast->kind = ALC_AST_KIND_IMPORT;

  return import_ast;
}
