#include "alc/ast.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "containers/vector.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

Alc_Ast *parse_union(Alc_Parser *p)
{
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

  Alc_Ast **children = vector_create(Alc_Ast *);

  while (p->pos < p->tokens_num && p->tokens[p->pos].type != ALC_TOKEN_TYPE_RCBRACK) {
    Alc_Ast *attribs = nullptr;
    if (p->tokens[p->pos].type == ALC_TOKEN_TYPE_LBRACK) {
      attribs = parse_attribute_list(p);
      _VERIFY_AST(attribs, { vector_destroy(children); });
      _VERIFY_POS(p, p->pos, { vector_destroy(children); });
    }

    Alc_Ast *child = nullptr;

    if (attribs == nullptr && p->tokens[p->pos].type == ALC_TOKEN_TYPE_ID) {
      child = parse_ids(p);
      if (child == (void *)-1) {
        child = nullptr;
      } else {
        _VERIFY_AST(child, { vector_destroy(children); });
      }
    }

    if (child == nullptr) {
      if ALC_UNLIKELY (p->tokens[p->pos].type == ALC_TOKEN_TYPE_SEMICOLON) {
        child = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
        child->pos = p->pos;
        child->kind = ALC_AST_KIND_NONE;
        p->pos++;
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

  Alc_Ast *union_ast =
    alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + sizeof(char) * name_len);
  union_ast->data.UNION.name = (char *)union_ast + sizeof(Alc_Ast);
  union_ast->data.UNION.attribute_list = attribute_list;
  union_ast->data.UNION.children = vector_to_array(children, &union_ast->data.UNION.children_num);
  union_ast->pos = pos;
  union_ast->kind = ALC_AST_KIND_UNION;
  memcpy(union_ast->data.UNION.name, name, name_len);
  return union_ast;
}
