#include "alc/ast.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "containers/vector.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

alc_ast_t *parse_struct(alc_parser_t *p, alc_ast_struct_kind_t kind)
{
  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);
  _VERIFY_VALUE(p, p->pos, "struct");

  p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  usize pos = p->pos++;

  alc_ast_t *generic_placeholder_type_list = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_LARROW) {
    generic_placeholder_type_list = parse_generic_placeholder_type_list(p);
    _VERIFY_AST(generic_placeholder_type_list);
  }

  alc_ast_t *attribute_list = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_LBRACK) {
    attribute_list = parse_attribute_list(p);
    _VERIFY_AST(attribute_list);
  }

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_LCBRACK);

  p->pos++;

  alc_ast_t **children = vector_create(alc_ast_t *);

  while (p->pos < p->tokens_num && p->tokens[p->pos].type != ALC_TOKEN_TYPE_RCBRACK) {
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
      } else {
        _VERIFY_AST(child, { vector_destroy(children); });
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
        _VERIFY_AST(child, { vector_destroy(children); });
      }
    }

    vector_push(children, child);
  }

  _VERIFY_POS(p, p->pos, { vector_destroy(children); });
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RCBRACK, { vector_destroy(children); });

  p->pos++;

  if (generic_placeholder_type_list != nullptr) {
    alc_ast_t *generic_struct_ast =
      alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + sizeof(char) * name_len);
    generic_struct_ast->data.GENERIC_STRUCT.name = (char *)generic_struct_ast + sizeof(alc_ast_t);
    generic_struct_ast->data.GENERIC_STRUCT.generic_placeholder_type_list =
      generic_placeholder_type_list;
    generic_struct_ast->data.GENERIC_STRUCT.attribute_list = attribute_list;
    generic_struct_ast->data.GENERIC_STRUCT.children =
      vector_to_array(children, &generic_struct_ast->data.GENERIC_STRUCT.children_num);
    generic_struct_ast->data.GENERIC_STRUCT.kind = kind;
    generic_struct_ast->pos = pos;
    generic_struct_ast->kind = ALC_AST_KIND_GENERIC_STRUCT;
    memcpy(generic_struct_ast->data.GENERIC_STRUCT.name, name, name_len);
    vector_destroy(children);
    return generic_struct_ast;
  } else {
    alc_ast_t *struct_ast =
      alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + sizeof(char) * name_len);
    struct_ast->data.STRUCT.name = (char *)struct_ast + sizeof(alc_ast_t);
    struct_ast->data.STRUCT.attribute_list = attribute_list;
    struct_ast->data.STRUCT.children =
      vector_to_array(children, &struct_ast->data.STRUCT.children_num);
    struct_ast->data.STRUCT.kind = kind;
    struct_ast->pos = pos;
    struct_ast->kind = ALC_AST_KIND_STRUCT;
    memcpy(struct_ast->data.STRUCT.name, name, name_len);
    vector_destroy(children);
    return struct_ast;
  }
}

alc_ast_t *parse_partial_struct(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);
  _VERIFY_VALUE(p, p->pos, "partial");

  p->pos++;

  return parse_struct(p, ALC_AST_STRUCT_KIND_PARTIAL);
}
