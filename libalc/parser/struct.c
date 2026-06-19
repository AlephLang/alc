#include "alc/ast.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "containers/vector.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

alc_ast_t *parse_struct(alc_parser_t *p, alc_ast_struct_kind_t kind)
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

  alc_ast_t *generic_placeholder_type_list = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_LARROW) {
    generic_placeholder_type_list = parse_generic_placeholder_type_list(p);
    if ALC_UNLIKELY (generic_placeholder_type_list == nullptr)
      return nullptr;
  }

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

#define PARSE_AND_VERIFY(_child, _func, _parser, ...) \
  {                                                   \
    _child = _func(_parser, ##__VA_ARGS__);           \
    if ALC_UNLIKELY ((_child) == nullptr) {           \
      vector_destroy(children);                       \
      return nullptr;                                 \
    }                                                 \
  }

    if (attribs == nullptr && p->tokens[p->pos].type == ALC_TOKEN_TYPE_ID) {
      if (strcmp(p->tokens[p->pos].value, "struct") == 0) {
        PARSE_AND_VERIFY(child, parse_struct, p, ALC_AST_STRUCT_KIND_DEFAULT);
      } else if (strcmp(p->tokens[p->pos].value, "partial") == 0) {
        PARSE_AND_VERIFY(child, parse_partial_struct, p);
      } else if (strcmp(p->tokens[p->pos].value, "enum") == 0) {
        PARSE_AND_VERIFY(child, parse_enum, p);
      } else if (strcmp(p->tokens[p->pos].value, "union") == 0) {
        PARSE_AND_VERIFY(child, parse_union, p);
      } else if (strcmp(p->tokens[p->pos].value, "using") == 0) {
        PARSE_AND_VERIFY(child, parse_typedef, p);
      } else if (strcmp(p->tokens[p->pos].value, "scope") == 0) {
        PARSE_AND_VERIFY(child, parse_scope, p);
      } else if (strcmp(p->tokens[p->pos].value, "extern") == 0) {
        PARSE_AND_VERIFY(child, parse_extern, p);
      } else if (strcmp(p->tokens[p->pos].value, "export") == 0) {
        p->pos++;

        if ALC_UNLIKELY (p->pos >= p->tokens_num) {
          add_error_unexpected_eof(p, p->pos);
          vector_destroy(children);
          return nullptr;
        }

        if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_ID) {
          add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_ID);
          vector_destroy(children);
          return nullptr;
        }

        PARSE_AND_VERIFY(child, parse_function, p, nullptr, ALC_AST_FUNCTION_KIND_EXPORTED);
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
    return struct_ast;
  }
}

alc_ast_t *parse_partial_struct(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  p->pos++;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_ID) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_ID);
    return nullptr;
  }

  if ALC_UNLIKELY (strcmp(p->tokens[p->pos].value, "struct")) {
    add_error_unexpected_value(p, p->pos++, 1, "struct");
    return nullptr;
  }

  return parse_struct(p, ALC_AST_STRUCT_KIND_PARTIAL);
}
