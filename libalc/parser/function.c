#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "containers/vector.h"
#include "global.h"
#include "parser/parser_private.h"

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

    alc_ast_t *arg = parse_decldef_var(p, nullptr);
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
  argument_list->data.ARGUMENT_LIST.arguments_num = 0;
  argument_list->data.ARGUMENT_LIST.arguments =
    vector_get_length(args) > 0 ?
      vector_to_array(args, &argument_list->data.ARGUMENT_LIST.arguments_num) :
      nullptr;
  argument_list->pos = pos;
  argument_list->kind = ALC_AST_KIND_ARGUMENT_LIST;
  vector_destroy(args);
  return argument_list;
}
