#include "alc/ast.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "containers/vector.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

alc_ast_t *parse_union(alc_parser_t *p)
{
  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);
  _VERIFY_VALUE(p, p->pos, "union");

  p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  usize pos = p->pos++;

  alc_ast_t *attribute_list = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_LBRACK) {
    attribute_list = parse_attribute_list(p);
    _VERIFY_AST(attribute_list);
  }

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_LCBRACK);

  p->pos++;

  alc_ast_t **children = vector_create(alc_ast_t *);

  while (p->pos < p->tokens_num) {
    if (p->tokens[p->pos].type == ALC_TOKEN_TYPE_RCBRACK)
      break;

    alc_ast_t *attribs = nullptr;
    if (p->tokens[p->pos].type == ALC_TOKEN_TYPE_LBRACK) {
      attribs = parse_attribute_list(p);
      _VERIFY_AST(attribs, { vector_destroy(children); });
      _VERIFY_POS(p, p->pos, { vector_destroy(children); });
    }

    alc_ast_t *child = nullptr;

    if (attribs == nullptr && p->tokens[p->pos].type == ALC_TOKEN_TYPE_ID) {
      child = parse_ids(p);
      if (child == (void *)-1) {
        child = nullptr;
      }

      _VERIFY_AST(child, { vector_destroy(children); });
    }

    if (child == nullptr) {
      if ALC_UNLIKELY (p->tokens[p->pos].type == ALC_TOKEN_TYPE_SEMICOLON) {
        p->pos++;
        child = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
        child->pos = pos;
        child->kind = ALC_AST_KIND_NONE;
      } else {
        child = parse_decldef(p, attribs);
        _VERIFY_AST(child, { vector_destroy(children); });
      }
    }

    vector_push(children, child);
  }

  _VERIFY_POS(p, p->pos, { vector_destroy(children); });
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RCBRACK, { vector_destroy(children); });

  p->pos++;

  alc_ast_t *union_ast =
    alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + sizeof(char) * name_len);
  union_ast->data.UNION.name = (char *)union_ast + sizeof(alc_ast_t);
  union_ast->data.UNION.attribute_list = attribute_list;
  union_ast->data.UNION.children = vector_to_array(children, &union_ast->data.UNION.children_num);
  union_ast->pos = pos;
  union_ast->kind = ALC_AST_KIND_STRUCT;
  memcpy(union_ast->data.UNION.name, name, name_len);
  return union_ast;
}
