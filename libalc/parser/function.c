#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "containers/vector.h"
#include "global.h"
#include "parser/parser_private.h"

static alc_ast_t *parse_variadic_args(alc_parser_t *p);

alc_ast_t *parse_function(alc_parser_t *p, alc_ast_t *attribute_list, alc_ast_function_kind_t kind)
{
  ALC_UNUSED_DEBUG(p);
  ALC_UNUSED_DEBUG(attribute_list);
  ALC_UNUSED_DEBUG(kind);
  ALC_TODO("Parse function");
}

alc_ast_t *parse_generic_function(alc_parser_t *p, alc_ast_t *attribute_list,
                                  alc_ast_function_kind_t kind)
{
  ALC_UNUSED_DEBUG(p);
  ALC_UNUSED_DEBUG(attribute_list);
  ALC_UNUSED_DEBUG(kind);
  ALC_TODO("Parse generic function");
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
