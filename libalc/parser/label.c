#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

Alc_Ast *parse_label(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_AT);
  _VERIFY_NO_WS(p, p->pos, ALC_TOKEN_TYPE_ID);

  p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  usize pos = p->pos++;

  Alc_Ast *label_ast =
    alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + sizeof(char) * name_len);
  label_ast->data.LABEL.name = (char *)label_ast + sizeof(Alc_Ast);
  label_ast->pos = pos;
  label_ast->kind = ALC_AST_KIND_LABEL;
  memcpy(label_ast->data.LABEL.name, name, sizeof(char) * name_len);
  return label_ast;
}
