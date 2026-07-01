#include "alc/ast.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "alc/vector.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

static Alc_Ast *parse_attribute(Alc_Parser *p);

Alc_Ast *parse_attribute_list(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_LBRACK);

  usize pos = p->pos++;

  Alc_Vector(Alc_Ast *) attrs = alc_vector_create(Alc_Ast *);
  b8 first = true;

  while (p->pos < p->tokens_num) {
    if (p->tokens[p->pos].type == ALC_TOKEN_TYPE_RBRACK)
      break;

    if (!first) {
      _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COMMA, { alc_vector_destroy(attrs); });
      p->pos++;
    }

    Alc_Ast *attr = parse_attribute(p);
    _VERIFY_AST(attr, { alc_vector_destroy(attrs); });

    alc_vector_push(attrs, attr);

    first = false;
  }

  _VERIFY_POS(p, p->pos, { alc_vector_destroy(attrs); });
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RBRACK, { alc_vector_destroy(attrs); });

  p->pos++;

  Alc_Ast *attribute_list = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  attribute_list->data.ATTRIBUTE_LIST.attributes =
    alc_vector_to_array(attrs, &attribute_list->data.ATTRIBUTE_LIST.attributes_num);
  attribute_list->pos = pos;
  attribute_list->kind = ALC_AST_KIND_ATTRIBUTE_LIST;
  alc_vector_destroy(attrs);
  return attribute_list;
}

static Alc_Ast *parse_attribute(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  usize pos = p->pos++;

  b8 has_arguments = false;
  Alc_Vector(Alc_Ast *) arguments = alc_vector_create(Alc_Ast *);
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_LPAREN) {
    has_arguments = true;

    p->pos++;

    b8 first = true;
    while (p->pos < p->tokens_num && p->tokens[p->pos].type != ALC_TOKEN_TYPE_RPAREN) {
      if (!first) {
        _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COMMA, { alc_vector_destroy(arguments); });
        p->pos++;
      }

      Alc_Ast *expr = parse_expr(p, false);
      _VERIFY_AST(expr, { alc_vector_destroy(arguments); });

      alc_vector_push(arguments, expr);

      first = false;
    }

    _VERIFY_POS(p, p->pos, { alc_vector_destroy(arguments); });
    _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RPAREN, { alc_vector_destroy(arguments); });

    p->pos++;
  }

  Alc_Ast *attribute_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + name_len);
  attribute_ast->data.ATTRIBUTE.name = (char *)attribute_ast + sizeof(Alc_Ast);
  attribute_ast->data.ATTRIBUTE.arguments =
    alc_vector_to_array(arguments, &attribute_ast->data.ATTRIBUTE.arguments_num);
  attribute_ast->data.ATTRIBUTE.has_arguments = has_arguments;
  attribute_ast->pos = pos;
  attribute_ast->kind = ALC_AST_KIND_ATTRIBUTE;
  memcpy(attribute_ast->data.ATTRIBUTE.name, name, name_len);
  alc_vector_destroy(arguments);
  return attribute_ast;
}
