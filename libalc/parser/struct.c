#include "alc/ast.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "alc/vector.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

Alc_Ast *parse_struct(Alc_Parser *p)
{
  p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  usize pos = p->pos++;

  Alc_Ast *generic_placeholder_type_list = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_LARROW) {
    generic_placeholder_type_list = parse_generic_placeholder_type_list(p);
    _VERIFY_AST(generic_placeholder_type_list);
  }

  Alc_Ast *attribute_list = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_LBRACK) {
    attribute_list = parse_attribute_list(p);
    _VERIFY_AST(attribute_list);
  }

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_LCBRACK);

  p->pos++;

  Alc_Vector(Alc_Ast *) children = alc_vector_create(Alc_Ast *);

  while (p->pos < p->tokens_num && p->tokens[p->pos].type != ALC_TOKEN_TYPE_RCBRACK) {
    Alc_Ast *attribs = nullptr;
    if (p->tokens[p->pos].type == ALC_TOKEN_TYPE_LBRACK) {
      attribs = parse_attribute_list(p);
      _VERIFY_AST(attribs, { alc_vector_destroy(children); });
      _VERIFY_POS(p, p->pos, { alc_vector_destroy(children); });
    }

    Alc_Ast *child = nullptr;

    if (attribs == nullptr && p->tokens[p->pos].type == ALC_TOKEN_TYPE_ID) {
      child = parse_ids(p);
      if (child == (void *)-1) {
        child = nullptr;
      } else {
        _VERIFY_AST(child, { alc_vector_destroy(children); });
      }
    }

    if (child == nullptr) {
      if ALC_UNLIKELY (p->tokens[p->pos].type == ALC_TOKEN_TYPE_SEMICOLON) {
        p->pos++;
        child = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
        child->pos = pos;
        child->kind = ALC_AST_KIND_NONE;
      } else {
        child = parse_decldef(p, attribs);
        _VERIFY_AST(child, { alc_vector_destroy(children); });
      }
    }

    alc_vector_push(children, child);
  }

  _VERIFY_POS(p, p->pos, { alc_vector_destroy(children); });
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RCBRACK, { alc_vector_destroy(children); });

  p->pos++;

  if (generic_placeholder_type_list != nullptr) {
    Alc_Ast *generic_struct_ast =
      alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + sizeof(char) * name_len);
    generic_struct_ast->GENERIC_STRUCT.name = (char *)generic_struct_ast + sizeof(Alc_Ast);
    generic_struct_ast->GENERIC_STRUCT.generic_placeholder_type_list =
      generic_placeholder_type_list;
    generic_struct_ast->GENERIC_STRUCT.attribute_list = attribute_list;
    generic_struct_ast->GENERIC_STRUCT.children =
      alc_vector_to_array(children, &generic_struct_ast->GENERIC_STRUCT.children_num);
    generic_struct_ast->pos = pos;
    generic_struct_ast->kind = ALC_AST_KIND_GENERIC_STRUCT;
    memcpy(generic_struct_ast->GENERIC_STRUCT.name, name, name_len);
    alc_vector_destroy(children);
    return generic_struct_ast;
  } else {
    Alc_Ast *struct_ast =
      alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + sizeof(char) * name_len);
    struct_ast->STRUCT.name = (char *)struct_ast + sizeof(Alc_Ast);
    struct_ast->STRUCT.attribute_list = attribute_list;
    struct_ast->STRUCT.children = alc_vector_to_array(children, &struct_ast->STRUCT.children_num);
    struct_ast->pos = pos;
    struct_ast->kind = ALC_AST_KIND_STRUCT;
    memcpy(struct_ast->STRUCT.name, name, name_len);
    alc_vector_destroy(children);
    return struct_ast;
  }
}
