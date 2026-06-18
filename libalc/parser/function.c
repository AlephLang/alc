#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "containers/vector.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

static alc_ast_t *parse_variadic_args(alc_parser_t *p);

alc_ast_t *parse_function(alc_parser_t *p, alc_ast_t *attribute_list, alc_ast_function_kind_t kind)
{
  ALC_ASSUME(p != nullptr);

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  usize pos = p->pos++;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_COLON) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_COLON);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].has_whitespace_after) {
    add_error_unexpected_whitespace(p, p->pos++, ALC_TOKEN_TYPE_COLON);
    return nullptr;
  }

  p->pos++;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_COLON) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_COLON);
    return nullptr;
  }

  p->pos++;

  alc_ast_t *generic_placeholder_type_list = nullptr;
  if (kind != ALC_AST_FUNCTION_KIND_EXPORTED && p->pos < p->tokens_num &&
      p->tokens[p->pos].type == ALC_TOKEN_TYPE_LARROW) {
    generic_placeholder_type_list = parse_generic_placeholder_type_list(p);
    if ALC_UNLIKELY (generic_placeholder_type_list == nullptr)
      return nullptr;
  }

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_LPAREN) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_LPAREN);
    return nullptr;
  }

  alc_ast_t *argument_list = parse_function_arguments(p);
  if ALC_UNLIKELY (argument_list == nullptr)
    return nullptr;

  alc_ast_t *return_type = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_MINUS) {
    if ALC_UNLIKELY (p->tokens[p->pos].has_whitespace_after) {
      add_error_unexpected_whitespace(p, p->pos++, ALC_TOKEN_TYPE_RARROW);
      return nullptr;
    }

    p->pos++;

    if ALC_UNLIKELY (p->pos >= p->tokens_num) {
      add_error_unexpected_eof(p, p->pos);
      return nullptr;
    }

    if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_RARROW) {
      add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_RARROW);
      return nullptr;
    }

    p->pos++;

    if ALC_UNLIKELY (p->pos >= p->tokens_num) {
      add_error_unexpected_eof(p, p->pos);
      return nullptr;
    }

    return_type = parse_type(p);
    if ALC_UNLIKELY (return_type == nullptr)
      return nullptr;
  }

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  alc_ast_t *body;
  if (p->tokens[p->pos].type == ALC_TOKEN_TYPE_ID && strcmp(p->tokens[p->pos].value, "$") == 0) {
    p->pos++;

    if ALC_UNLIKELY (p->pos >= p->tokens_num) {
      add_error_unexpected_eof(p, p->pos);
      return nullptr;
    }

    body = parse_expr(p, false);
    if ALC_UNLIKELY (body == nullptr)
      return nullptr;

    if ALC_UNLIKELY (p->pos >= p->tokens_num) {
      add_error_unexpected_eof(p, p->pos);
      return nullptr;
    }

    if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_SEMICOLON) {
      add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_SEMICOLON);
      return nullptr;
    }

    p->pos++;
  } else if (p->tokens[p->pos].type == ALC_TOKEN_TYPE_LCBRACK) {
    body = parse_stmt_block(p);
    if ALC_UNLIKELY (body == nullptr)
      return nullptr;
  } else {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_LCBRACK);
    return nullptr;
  }

  if (generic_placeholder_type_list == nullptr) {
    alc_ast_t *function_ast =
      alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + sizeof(char) * name_len);
    function_ast->data.FUNC.name = (char *)function_ast + sizeof(alc_ast_t);
    function_ast->data.FUNC.argument_list = argument_list;
    function_ast->data.FUNC.return_type = return_type;
    function_ast->data.FUNC.body = body;
    function_ast->data.FUNC.attribute_list = attribute_list;
    function_ast->data.FUNC.kind = kind;
    function_ast->pos = pos;
    function_ast->kind = ALC_AST_KIND_FUNC;
    memcpy(function_ast->data.FUNC.name, name, name_len);
    return function_ast;
  } else {
    alc_ast_t *generic_function_ast =
      alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + sizeof(char) * name_len);
    generic_function_ast->data.GENERIC_FUNC.name = (char *)generic_function_ast + sizeof(alc_ast_t);
    generic_function_ast->data.GENERIC_FUNC.generic_placeholder_type_list =
      generic_placeholder_type_list;
    generic_function_ast->data.GENERIC_FUNC.argument_list = argument_list;
    generic_function_ast->data.GENERIC_FUNC.return_type = return_type;
    generic_function_ast->data.GENERIC_FUNC.body = body;
    generic_function_ast->data.GENERIC_FUNC.attribute_list = attribute_list;
    generic_function_ast->data.GENERIC_FUNC.kind = kind;
    generic_function_ast->pos = pos;
    generic_function_ast->kind = ALC_AST_KIND_GENERIC_FUNC;
    memcpy(generic_function_ast->data.GENERIC_FUNC.name, name, name_len);
    return generic_function_ast;
  }
}

alc_ast_t *parse_function_arguments(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos++;

  alc_ast_t **args = vector_create(alc_ast_t *);
  b8 first = true;
  while (p->pos < p->tokens_num) {
    if (p->tokens[p->pos].type == ALC_TOKEN_TYPE_RPAREN)
      break;

    if (!first) {
      if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_COMMA) {
        add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_COMMA);
        vector_destroy(args);
        return nullptr;
      }

      p->pos++;

      if ALC_UNLIKELY (p->pos >= p->tokens_num) {
        add_error_unexpected_eof(p, p->pos);
        vector_destroy(args);
        return nullptr;
      }
    }

    alc_ast_t *arg = p->tokens[p->pos].type == ALC_TOKEN_TYPE_PERIOD ?
                       parse_variadic_args(p) :
                       parse_decldef_var(p, nullptr);
    if ALC_UNLIKELY (arg == nullptr) {
      vector_destroy(args);
      return nullptr;
    }

    vector_push(args, arg);

    first = false;
  }

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    vector_destroy(args);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_RPAREN) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_RPAREN);
    vector_destroy(args);
    return nullptr;
  }

  p->pos++;

  alc_ast_t *argument_list = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  argument_list->data.ARGUMENT_LIST.arguments =
    vector_to_array(args, &argument_list->data.ARGUMENT_LIST.arguments_num);
  argument_list->pos = pos;
  argument_list->kind = ALC_AST_KIND_ARGUMENT_LIST;
  vector_destroy(args);
  return argument_list;
}

static alc_ast_t *parse_variadic_args(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos;

  if ALC_UNLIKELY (p->tokens[p->pos].has_whitespace_after) {
    add_error_unexpected_whitespace(p, p->pos++, ALC_TOKEN_TYPE_PERIOD);
    return nullptr;
  }

  p->pos++;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_PERIOD) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_PERIOD);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].has_whitespace_after) {
    add_error_unexpected_whitespace(p, p->pos++, ALC_TOKEN_TYPE_PERIOD);
    return nullptr;
  }

  p->pos++;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_PERIOD) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_PERIOD);
    return nullptr;
  }

  p->pos++;

  alc_ast_t *variadic_arg_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  variadic_arg_ast->pos = pos;
  variadic_arg_ast->kind = ALC_AST_KIND_VARIADIC;
  return variadic_arg_ast;
}
