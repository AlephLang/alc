#include "alc/ast.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "containers/vector.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

static alc_ast_t *parse_attribute(alc_parser_t *p);

alc_ast_t *parse_attribute_list(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_LBRACK);

  usize pos = p->pos++;

  alc_ast_t **attrs = vector_create(alc_ast_t *);
  b8 first = true;

  while (p->pos < p->tokens_num) {
    if (p->tokens[p->pos].type == ALC_TOKEN_TYPE_RBRACK)
      break;

    if (!first) {
      _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COMMA, { vector_destroy(attrs); });
      p->pos++;
    }

    alc_ast_t *attr = parse_attribute(p);
    _VERIFY_AST(attr, { vector_destroy(attrs); });

    vector_push(attrs, attr);

    first = false;
  }

  _VERIFY_POS(p, p->pos, { vector_destroy(attrs); });
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RBRACK, { vector_destroy(attrs); });

  p->pos++;

  alc_ast_t *attribute_list = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  attribute_list->data.ATTRIBUTE_LIST.attributes =
    vector_to_array(attrs, &attribute_list->data.ATTRIBUTE_LIST.attributes_num);
  attribute_list->pos = pos;
  attribute_list->kind = ALC_AST_KIND_ATTRIBUTE_LIST;
  vector_destroy(attrs);
  return attribute_list;
}

static alc_ast_t *parse_attribute(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  usize pos = p->pos++;

  b8 has_arguments = false;
  alc_ast_t **arguments = vector_create(alc_ast_t *);
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_LPAREN) {
    has_arguments = true;

    p->pos++;

    b8 first = true;
    while (p->pos < p->tokens_num && p->tokens[p->pos].type != ALC_TOKEN_TYPE_RPAREN) {
      if (!first) {
        _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COMMA, { vector_destroy(arguments); });
        p->pos++;
      }

      alc_ast_t *expr = parse_expr(p, false);
      _VERIFY_AST(expr, { vector_destroy(arguments); });

      vector_push(arguments, expr);

      first = false;
    }

    _VERIFY_POS(p, p->pos, { vector_destroy(arguments); });
    _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RPAREN, { vector_destroy(arguments); });

    p->pos++;
  }

  alc_ast_t *attribute_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + name_len);
  attribute_ast->data.ATTRIBUTE.name = (char *)attribute_ast + sizeof(alc_ast_t);
  attribute_ast->data.ATTRIBUTE.arguments =
    vector_to_array(arguments, &attribute_ast->data.ATTRIBUTE.arguments_num);
  attribute_ast->data.ATTRIBUTE.has_arguments = has_arguments;
  attribute_ast->pos = pos;
  attribute_ast->kind = ALC_AST_KIND_ATTRIBUTE;
  memcpy(attribute_ast->data.ATTRIBUTE.name, name, name_len);
  vector_destroy(arguments);
  return attribute_ast;
}
