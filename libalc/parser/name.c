#include "alc/alloc_arena.h"
#include "alc/ast.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

Alc_Ast *parse_name(Alc_Parser *p)
{
  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;
  usize pos = p->pos++;

  Alc_Ast *name_ast =
    alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + (sizeof(char) * name_len));
  name_ast->NAME.name = (char *)name_ast + sizeof(Alc_Ast);
  name_ast->pos = pos;
  name_ast->kind = ALC_AST_KIND_NAME;
  memcpy(name_ast->NAME.name, name, sizeof(char) * name_len);

  return name_ast;
}
