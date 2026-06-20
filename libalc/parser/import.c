#include "alc/ast.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"

alc_ast_t *parse_import(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos++;

  alc_ast_t *module = parse_module(p);
  _VERIFY_AST(module);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_SEMICOLON);

  p->pos++;

  alc_ast_t *import_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  import_ast->data.IMPORT.module = module;
  import_ast->pos = pos;
  import_ast->kind = ALC_AST_KIND_IMPORT;

  return import_ast;
}
