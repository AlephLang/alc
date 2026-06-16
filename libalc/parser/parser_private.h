#ifndef __ALC_PARSER_PRIVATE_H__
#define __ALC_PARSER_PRIVATE_H__

#include "alc/parser.h"
#include "containers/vector.h"

typedef struct __alc_parser_t {
  alc_token_t *tokens;
  usize tokens_num;

  alc_parser_error_t *errors;

  usize pos;
} alc_parser_t;

alc_token_t *peek(const alc_parser_t *p, s32 adv);

alc_ast_t *parse_top(alc_parser_t *p);
alc_ast_t *parse_import(alc_parser_t *p);
alc_ast_t *parse_module(alc_parser_t *p);
alc_ast_t *parse_typedef(alc_parser_t *p);
alc_ast_t *parse_type_raw(alc_parser_t *p);
alc_ast_t *parse_type(alc_parser_t *p);
alc_ast_t *parse_generic_placeholder_type_list(alc_parser_t *p);
alc_ast_t *parse_generic_type_list(alc_parser_t *p);
alc_ast_t *parse_function_arguments(alc_parser_t *p);
alc_ast_t *parse_decldef(alc_parser_t *p);
alc_ast_t *parse_decldef_var(alc_parser_t *p, alc_ast_t *attribute_list);
alc_ast_t *parse_attribute_list(alc_parser_t *p);
alc_ast_t *parse_expr(alc_parser_t *p, b8 is_toplevel);
alc_ast_t *parse_stmt_expr(alc_parser_t *p);

static inline void add_error(alc_parser_t *p, alc_parser_error_t error)
{
  ALC_ASSUME(p != nullptr);
  printf("PARSER ERROR!!!\n"); // TODO: Remove this message when basic error handler is done.
  vector_push(p->errors, error);
}

static inline void add_error_unexpected_eof(alc_parser_t *p, usize pos)
{
  ALC_ASSUME(p != nullptr);
  alc_parser_error_t unexpected_eof_error = {
    .data = { { 0 } },
    .pos = pos,
    .len = 1,
    .type = ALC_PARSER_ERROR_TYPE_UNEXPECTED_EOF,
  };
  add_error(p, unexpected_eof_error);
}

void add_error_unexpected_token(alc_parser_t *p, usize pos, usize expected_token_types_num,
                                /* alc_token_type_t */...);

void add_error_unexpected_value(alc_parser_t *p, usize pos, usize expected_values_num,
                                /* const char * */...);

static inline void add_error_unexpected_whitespace(alc_parser_t *p, usize pos,
                                                   alc_token_type_t expected_token_type)
{
  ALC_ASSUME(p != nullptr);
  alc_parser_error_t unexpected_ws_error = {
    .data = { .UNEXPECTED_WHITESPACE = { .expected_token_type = expected_token_type } },
    .pos = pos,
    .len = 1,
    .type = ALC_PARSER_ERROR_TYPE_UNEXPECTED_WHITESPACE,
  };
  add_error(p, unexpected_ws_error);
}

#endif // __ALC_PARSER_PRIVATE_H__
