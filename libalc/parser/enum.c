#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "containers/vector.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

static Alc_Ast *parse_enum_element(Alc_Parser *p);

Alc_Ast *parse_enum(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  usize pos = p->pos++;

  Alc_Ast *attribute_list = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_LBRACK) {
    attribute_list = parse_attribute_list(p);
    _VERIFY_AST(attribute_list);
  }

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_LCBRACK);

  p->pos++;

  Alc_Ast **elements = vector_create(Alc_Ast *);
  while (p->pos < p->tokens_num && p->tokens[p->pos].type != ALC_TOKEN_TYPE_RCBRACK) {
    Alc_Ast *element = parse_enum_element(p);
    _VERIFY_AST(element, { vector_destroy(elements); });
    _VERIFY_POS(p, p->pos, { vector_destroy(elements); });

    vector_push(elements, element);

    if (p->tokens[p->pos].type != ALC_TOKEN_TYPE_COMMA)
      break;

    p->pos++;
  }

  _VERIFY_POS(p, p->pos, { vector_destroy(elements); });

  {
    Alc_Token_Type *expected_v = vector_reserve(Alc_Token_Type, 2);
    vector_push(expected_v, ALC_TOKEN_TYPE_RCBRACK);
    vector_push(expected_v, ALC_TOKEN_TYPE_COMMA);
    _VERIFY_TOKEN_V(p, p->pos, expected_v, { vector_destroy(elements); });
    vector_destroy(expected_v);
  }

  p->pos++;

  Alc_Ast *enum_ast =
    alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + sizeof(char) * name_len);
  enum_ast->data.ENUM.name = (char *)enum_ast + sizeof(Alc_Ast);
  enum_ast->data.ENUM.elements = vector_to_array(elements, &enum_ast->data.ENUM.elements_num);
  enum_ast->data.ENUM.attribute_list = attribute_list;
  enum_ast->pos = pos;
  enum_ast->kind = ALC_AST_KIND_ENUM;
  memcpy(enum_ast->data.ENUM.name, name, name_len);
  return enum_ast;
}

static Alc_Ast *parse_enum_element(Alc_Parser *p)
{
  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  usize pos = p->pos++;

  Alc_Ast *expr = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_EQ) {
    p->pos++;

    expr = parse_expr(p, false);
    _VERIFY_AST(expr);
  }

  Alc_Ast *enum_element =
    alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + sizeof(char) * name_len);
  enum_element->data.ENUM_ELEMENT.name = (char *)enum_element + sizeof(Alc_Ast);
  enum_element->data.ENUM_ELEMENT.expression = expr;
  enum_element->pos = pos;
  enum_element->kind = ALC_AST_KIND_ENUM_ELEMENT;
  memcpy(enum_element->data.ENUM_ELEMENT.name, name, name_len);
  return enum_element;
}
