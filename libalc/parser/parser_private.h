#ifndef __ALC_PARSER_PRIVATE_H__
#define __ALC_PARSER_PRIVATE_H__

#include "alc/ast.h"
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
alc_ast_t *parse_ids(alc_parser_t *p); // Returns (void*)-1 if nothing was parsed
alc_ast_t *parse_import(alc_parser_t *p);
alc_ast_t *parse_module(alc_parser_t *p);
alc_ast_t *parse_typedef(alc_parser_t *p);
alc_ast_t *parse_type_raw(alc_parser_t *p);
alc_ast_t *parse_type(alc_parser_t *p);
alc_ast_t *parse_function(alc_parser_t *p, alc_ast_t *attribute_list, alc_ast_function_kind_t kind);
alc_ast_t *parse_extern(alc_parser_t *p);
alc_ast_t *parse_function_arguments(alc_parser_t *p);
alc_ast_t *parse_generic_placeholder_type_list(alc_parser_t *p);
alc_ast_t *parse_generic_type_list(alc_parser_t *p);
alc_ast_t *parse_decldef(alc_parser_t *p, alc_ast_t *attribute_list);
alc_ast_t *parse_decldef_var(alc_parser_t *p, alc_ast_t *attribute_list);
alc_ast_t *parse_attribute_list(alc_parser_t *p);
alc_ast_t *parse_expr(alc_parser_t *p, b8 is_toplevel);
alc_ast_t *parse_stmt_expr(alc_parser_t *p);
alc_ast_t *parse_initlist(alc_parser_t *p);
alc_ast_t *parse_struct(alc_parser_t *p, alc_ast_struct_kind_t kind);
alc_ast_t *parse_partial_struct(alc_parser_t *p);
alc_ast_t *parse_union(alc_parser_t *p);
alc_ast_t *parse_enum(alc_parser_t *p);
alc_ast_t *parse_scope(alc_parser_t *p);
alc_ast_t *parse_stmt(alc_parser_t *p);
alc_ast_t *parse_stmt_block(alc_parser_t *p);
alc_ast_t *parse_stmt_return(alc_parser_t *p);
alc_ast_t *parse_stmt_goto(alc_parser_t *p);
alc_ast_t *parse_stmt_label(alc_parser_t *p);
alc_ast_t *parse_stmt_break(alc_parser_t *p);
alc_ast_t *parse_stmt_continue(alc_parser_t *p);
alc_ast_t *parse_stmt_fallthrough(alc_parser_t *p);
alc_ast_t *parse_stmt_if(alc_parser_t *p);
alc_ast_t *parse_stmt_else(alc_parser_t *p);
alc_ast_t *parse_stmt_loop(alc_parser_t *p);
alc_ast_t *parse_stmt_while(alc_parser_t *p);
alc_ast_t *parse_stmt_do_while(alc_parser_t *p);
alc_ast_t *parse_stmt_for(alc_parser_t *p);
alc_ast_t *parse_stmt_foreach(alc_parser_t *p);
alc_ast_t *parse_stmt_switch(alc_parser_t *p);
alc_ast_t *parse_stmt_defer(alc_parser_t *p);

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
