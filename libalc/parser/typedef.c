#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

Alc_Ast *parse_typedef(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos++;

  Alc_Ast *attribute_list = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_LBRACK) {
    attribute_list = parse_attribute_list(p);
    _VERIFY_AST(attribute_list);
  }

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;
  p->pos++;

  Alc_Ast *generic_placeholder_type_list = nullptr;
  if (p->tokens[p->pos].type == ALC_TOKEN_TYPE_LARROW) {
    generic_placeholder_type_list = parse_generic_placeholder_type_list(p);
    _VERIFY_AST(generic_placeholder_type_list);
  }

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_EQ);

  p->pos++;

  Alc_Ast *aliased_type = parse_type(p);
  _VERIFY_AST(aliased_type);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_SEMICOLON);

  p->pos++;

  Alc_Ast *typedef_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + name_len);
  typedef_ast->data.TYPEDEF.name = (char *)typedef_ast + sizeof(Alc_Ast);
  typedef_ast->data.TYPEDEF.aliased_type = aliased_type;
  typedef_ast->data.TYPEDEF.generic_placeholder_type_list = generic_placeholder_type_list;
  typedef_ast->data.TYPEDEF.attribute_list = attribute_list;
  typedef_ast->pos = pos;
  typedef_ast->kind = ALC_AST_KIND_TYPEDEF;
  memcpy(typedef_ast->data.TYPEDEF.name, name, name_len);
  return typedef_ast;
}
