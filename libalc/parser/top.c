#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

alc_ast_t *parse_top(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  switch (p->tokens[p->pos].type) {
  case ALC_TOKEN_TYPE_SEMICOLON: {
    alc_ast_t *none_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
    none_ast->pos = p->pos++;
    none_ast->kind = ALC_AST_KIND_NONE;
    return none_ast;
  }

  case ALC_TOKEN_TYPE_ID: {
    if (strcmp(p->tokens[p->pos].value, "import") == 0)
      return parse_import(p);
    else if (strcmp(p->tokens[p->pos].value, "using") == 0)
      return parse_typedef(p);
    else if (strcmp(p->tokens[p->pos].value, "struct") == 0)
      return parse_struct(p, ALC_AST_STRUCT_KIND_DEFAULT);
    else if (strcmp(p->tokens[p->pos].value, "partial") == 0)
      return parse_partial_struct(p);
    else if (strcmp(p->tokens[p->pos].value, "enum") == 0)
      return parse_enum(p);
    else if (strcmp(p->tokens[p->pos].value, "union") == 0)
      return parse_union(p);
    else if (strcmp(p->tokens[p->pos].value, "scope") == 0)
      return parse_scope(p);
    else if (strcmp(p->tokens[p->pos].value, "export") == 0) {
      p->pos++;

      if ALC_UNLIKELY (p->pos >= p->tokens_num) {
        add_error_unexpected_eof(p, p->pos);
        return nullptr;
      }

      if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_ID) {
        add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_ID);
        return nullptr;
      }

      return parse_function(p, nullptr, ALC_AST_FUNCTION_KIND_EXPORTED);
    }

    return parse_decldef(p, nullptr);
  }

  case ALC_TOKEN_TYPE_LBRACK: {
    alc_ast_t *attribute_list = parse_attribute_list(p);
    if ALC_UNLIKELY (attribute_list == nullptr)
      return nullptr;

    if ALC_UNLIKELY (p->pos >= p->tokens_num) {
      add_error_unexpected_eof(p, p->pos);
      return nullptr;
    }

    return parse_decldef(p, attribute_list);
  }

  default:
    add_error_unexpected_token(p, p->pos++, 0);
    return nullptr;
  }
}
