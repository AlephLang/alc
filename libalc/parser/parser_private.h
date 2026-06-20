#ifndef __ALC_PARSER_PRIVATE_H__
#define __ALC_PARSER_PRIVATE_H__

#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "containers/vector.h"
#include "global.h"
#include "string.h"

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
alc_ast_t *parse_label(alc_parser_t *p);
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
  add_error(p, (alc_parser_error_t){
                 .data = { { 0 } },
                 .pos = pos,
                 .len = 1,
                 .type = ALC_PARSER_ERROR_TYPE_UNEXPECTED_EOF,
               });
}

static inline void add_error_unexpected_token(alc_parser_t *p, usize pos, alc_token_type_t expected)
{
  ALC_ASSUME(p != nullptr);
  alc_parser_error_t error = {
    .data = {
      .UNEXPECTED_TOKEN = {
        .expected_token_types = alloc_arena_allocate(&ctx()->arena, sizeof(alc_token_type_t)),
        .expected_token_types_num = 1,
      },
    },
    .pos = pos,
    .len = 1,
    .type = ALC_PARSER_ERROR_TYPE_UNEXPECTED_TOKEN,
  };
  error.data.UNEXPECTED_TOKEN.expected_token_types[0] = expected;
  add_error(p, error);
  p->pos++;
}

static inline void add_error_unexpected_token_v(alc_parser_t *p, usize pos,
                                                alc_token_type_t *expected_v)
{
  ALC_ASSUME(p != nullptr);
  alc_parser_error_t error = {
    .data = { { 0 } },
    .pos = pos,
    .len = 1,
    .type = ALC_PARSER_ERROR_TYPE_UNEXPECTED_TOKEN,
  };
  if ALC_LIKELY (expected_v != nullptr) {
    error.data.UNEXPECTED_TOKEN.expected_token_types =
      vector_to_array(expected_v, &error.data.UNEXPECTED_TOKEN.expected_token_types_num);
    vector_destroy(expected_v);
  }
  add_error(p, error);
  p->pos++;
}

static inline void add_error_unexpected_value(alc_parser_t *p, usize pos, const char *expected)
{
  ALC_ASSUME(p != nullptr);
  alc_parser_error_t error = {
    .data = {
      .UNEXPECTED_VALUE = {
        .expected_values = alloc_arena_allocate(&ctx()->arena, sizeof(const char *)),
        .expected_values_num = 1,
      },
    },
    .pos = pos,
    .len = 1,
    .type = ALC_PARSER_ERROR_TYPE_UNEXPECTED_VALUE,
  };
  error.data.UNEXPECTED_VALUE.expected_values[0] = expected;
  add_error(p, error);
  p->pos++;
}

static inline void add_error_unexpected_value_v(alc_parser_t *p, usize pos, const char **expected_v)
{
  ALC_ASSUME(p != nullptr);
  alc_parser_error_t error = {
    .data = { { 0 } },
    .pos = pos,
    .len = 1,
    .type = ALC_PARSER_ERROR_TYPE_UNEXPECTED_VALUE,
  };
  if ALC_LIKELY (expected_v != nullptr) {
    error.data.UNEXPECTED_VALUE.expected_values =
      vector_to_array(expected_v, &error.data.UNEXPECTED_VALUE.expected_values_num);
    vector_destroy(expected_v);
  }
  add_error(p, error);
  p->pos++;
}

static inline void add_error_unexpected_whitespace(alc_parser_t *p, usize pos,
                                                   alc_token_type_t expected_after)
{
  add_error(p, (alc_parser_error_t){
                 .data = { .UNEXPECTED_WHITESPACE = { .expected_token_type = expected_after } },
                 .pos = pos,
                 .len = 1,
                 .type = ALC_PARSER_ERROR_TYPE_UNEXPECTED_WHITESPACE,
               });
  p->pos++;
}

#define _VERIFY_POS(_p, _pos, ...)                 \
  {                                                \
    if ALC_UNLIKELY ((_pos) >= (_p)->tokens_num) { \
      add_error_unexpected_eof((_p), (_pos));      \
      __VA_ARGS__                                  \
      return nullptr;                              \
    }                                              \
  }

#define _VERIFY_TOKEN(_p, _pos, _expected, ...)                  \
  {                                                              \
    if ALC_UNLIKELY ((_p)->tokens[(_pos)].type != (_expected)) { \
      add_error_unexpected_token((_p), (_pos), (_expected));     \
      __VA_ARGS__                                                \
      return nullptr;                                            \
    }                                                            \
  }

#define _VERIFY_TOKEN_V(_p, _pos, _expected_v, ...)                   \
  {                                                                   \
    if ALC_UNLIKELY ((_p)->tokens[(_pos)].type != (_expected_v)[0]) { \
      add_error_unexpected_token_v((_p), (_pos), (_expected_v));      \
      __VA_ARGS__                                                     \
      return nullptr;                                                 \
    }                                                                 \
  }

#define _VERIFY_VALUE(_p, _pos, _expected, ...)                              \
  {                                                                          \
    if ALC_UNLIKELY (strcmp((_p)->tokens[(_pos)].value, (_expected)) != 0) { \
      add_error_unexpected_value((_p), (_pos), (_expected));                 \
      __VA_ARGS__                                                            \
      return nullptr;                                                        \
    }                                                                        \
  }

#define _VERIFY_VALUE_V(_p, _pos, _expected_v, ...)                             \
  {                                                                             \
    if ALC_UNLIKELY (strcmp((_p)->tokens[(_pos)].value, (_expected)[0]) != 0) { \
      add_error_unexpected_value_v((_p), (_pos), (_expected_v));                \
      __VA_ARGS__                                                               \
      return nullptr;                                                           \
    }                                                                           \
  }

#define _VERIFY_NO_WS(_p, _pos, _expected_token_type, ...)                   \
  {                                                                          \
    if ALC_UNLIKELY ((_p)->tokens[(_pos)].has_whitespace_after) {            \
      add_error_unexpected_whitespace((_p), (_pos), (_expected_token_type)); \
      __VA_ARGS__                                                            \
      return nullptr;                                                        \
    }                                                                        \
  }

#define _VERIFY_AST(_ast, ...)            \
  {                                       \
    if ALC_UNLIKELY ((_ast) == nullptr) { \
      __VA_ARGS__                         \
      return nullptr;                     \
    }                                     \
  }

#endif // __ALC_PARSER_PRIVATE_H__
