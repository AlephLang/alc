#include "alc/ast.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"

Alc_Ast *parse_import(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos++;

  Alc_Ast *module = parse_module(p);
  _VERIFY_AST(module);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_SEMICOLON);

  p->pos++;

  Alc_Ast *import_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  import_ast->data.IMPORT.module = module;
  import_ast->pos = pos;
  import_ast->kind = ALC_AST_KIND_IMPORT;

  return import_ast;
}
