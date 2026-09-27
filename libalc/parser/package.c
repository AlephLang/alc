#include "alc/alloc_arena.h"
#include "alc/ast.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

Alc_Ast *parse_package(Alc_Parser *p)
{
  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  usize pos = p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COLON);
  _VERIFY_NO_WS(p, p->pos, ALC_TOKEN_TYPE_COLON);

  p->pos++;
  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COLON);

  p->pos++;
  Alc_Ast *module = parse_module(p);
  _VERIFY_AST(module);

  Alc_Ast *package_ast =
    alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + (sizeof(char) * name_len));
  package_ast->PACKAGE.name = (char *)package_ast + sizeof(Alc_Ast);
  package_ast->PACKAGE.module = module;
  package_ast->pos = pos;
  package_ast->kind = ALC_AST_KIND_PACKAGE;
  memcpy(package_ast->PACKAGE.name, name, sizeof(char) * name_len);

  return package_ast;
}
