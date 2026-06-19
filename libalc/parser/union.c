#include "alc/ast.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "containers/vector.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

alc_ast_t *parse_union(alc_parser_t *p)
{
  p->pos++;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_ID) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_ID);
    return nullptr;
  }

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  usize pos = p->pos++;

  alc_ast_t *attribute_list = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_LBRACK) {
    attribute_list = parse_attribute_list(p);
    if ALC_UNLIKELY (attribute_list == nullptr)
      return nullptr;
  }

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_LCBRACK) {
    add_error_unexpected_token(p, p->pos++, 2, ALC_TOKEN_TYPE_LCBRACK, ALC_TOKEN_TYPE_LARROW);
    return nullptr;
  }

  p->pos++;

  alc_ast_t **children = vector_create(alc_ast_t *);

  while (p->pos < p->tokens_num) {
    if (p->tokens[p->pos].type == ALC_TOKEN_TYPE_RCBRACK)
      break;

    alc_ast_t *attribs = nullptr;
    if (p->tokens[p->pos].type == ALC_TOKEN_TYPE_LBRACK) {
      attribs = parse_attribute_list(p);
      if ALC_UNLIKELY (attribs == nullptr) {
        vector_destroy(children);
        return nullptr;
      }

      if ALC_UNLIKELY (p->pos >= p->tokens_num) {
        add_error_unexpected_eof(p, p->pos);
        vector_destroy(children);
        return nullptr;
      }
    }

    alc_ast_t *child = nullptr;

    if (attribs == nullptr && p->tokens[p->pos].type == ALC_TOKEN_TYPE_ID) {
      child = parse_ids(p);
      if (child == (void *)-1) {
        child = nullptr;
      } else if ALC_UNLIKELY (child == nullptr) {
        vector_destroy(children);
        return nullptr;
      }
    }

    if (child == nullptr) {
      if ALC_UNLIKELY (p->tokens[p->pos].type == ALC_TOKEN_TYPE_SEMICOLON) {
        p->pos++;
        child = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
        child->pos = pos;
        child->kind = ALC_AST_KIND_NONE;
      } else {
        child = parse_decldef(p, attribs);
        if ALC_UNLIKELY (child == nullptr) {
          vector_destroy(children);
          return nullptr;
        }
      }
    }

    vector_push(children, child);
  }

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    vector_destroy(children);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_RCBRACK) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_RCBRACK);
    vector_destroy(children);
    return nullptr;
  }

  p->pos++;

  alc_ast_t *union_ast =
    alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + sizeof(char) * name_len);
  union_ast->data.UNION.name = (char *)union_ast + sizeof(alc_ast_t);
  union_ast->data.UNION.attribute_list = attribute_list;
  union_ast->data.UNION.children = vector_to_array(children, &union_ast->data.UNION.children_num);
  union_ast->pos = pos;
  union_ast->kind = ALC_AST_KIND_UNION;
  memcpy(union_ast->data.UNION.name, name, name_len);
  return union_ast;
}
