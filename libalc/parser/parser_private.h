#ifndef __ALC_PARSER_PRIVATE_H__
#define __ALC_PARSER_PRIVATE_H__

#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "containers/vector.h"
#include "global.h"
#include <string.h>

typedef struct __Alc_Parser {
  Alc_Token *tokens;
  usize tokens_num;

  Alc_Parser_Error *errors;

  usize pos;
} Alc_Parser;

Alc_Token *peek(const Alc_Parser *p, s32 adv);

Alc_Ast *parse_top(Alc_Parser *p);
Alc_Ast *parse_ids(Alc_Parser *p); // Returns (void*)-1 if nothing was parsed
Alc_Ast *parse_import(Alc_Parser *p);
Alc_Ast *parse_module(Alc_Parser *p);
Alc_Ast *parse_typedef(Alc_Parser *p);
Alc_Ast *parse_type_raw(Alc_Parser *p);
Alc_Ast *parse_type(Alc_Parser *p);
Alc_Ast *parse_function(Alc_Parser *p, Alc_Ast *attribute_list, Alc_Ast_Function_Kind kind);
Alc_Ast *parse_extern(Alc_Parser *p);
Alc_Ast *parse_function_arguments(Alc_Parser *p);
Alc_Ast *parse_generic_placeholder_type_list(Alc_Parser *p);
Alc_Ast *parse_generic_type_list(Alc_Parser *p);
Alc_Ast *parse_decldef(Alc_Parser *p, Alc_Ast *attribute_list);
Alc_Ast *parse_decldef_var(Alc_Parser *p, Alc_Ast *attribute_list);
Alc_Ast *parse_attribute_list(Alc_Parser *p);
Alc_Ast *parse_expr(Alc_Parser *p, b8 is_toplevel);
Alc_Ast *parse_stmt_expr(Alc_Parser *p);
Alc_Ast *parse_initlist(Alc_Parser *p);
Alc_Ast *parse_struct(Alc_Parser *p, Alc_Ast_Struct_Kind kind);
Alc_Ast *parse_partial_struct(Alc_Parser *p);
Alc_Ast *parse_union(Alc_Parser *p);
Alc_Ast *parse_enum(Alc_Parser *p);
Alc_Ast *parse_scope(Alc_Parser *p);
Alc_Ast *parse_label(Alc_Parser *p);
Alc_Ast *parse_stmt(Alc_Parser *p);
Alc_Ast *parse_stmt_block(Alc_Parser *p);
Alc_Ast *parse_stmt_return(Alc_Parser *p);
Alc_Ast *parse_stmt_goto(Alc_Parser *p);
Alc_Ast *parse_stmt_label(Alc_Parser *p);
Alc_Ast *parse_stmt_break(Alc_Parser *p);
Alc_Ast *parse_stmt_continue(Alc_Parser *p);
Alc_Ast *parse_stmt_fallthrough(Alc_Parser *p);
Alc_Ast *parse_stmt_if(Alc_Parser *p);
Alc_Ast *parse_stmt_else(Alc_Parser *p);
Alc_Ast *parse_stmt_loop(Alc_Parser *p);
Alc_Ast *parse_stmt_while(Alc_Parser *p);
Alc_Ast *parse_stmt_do_while(Alc_Parser *p);
Alc_Ast *parse_stmt_for(Alc_Parser *p);
Alc_Ast *parse_stmt_foreach(Alc_Parser *p);
Alc_Ast *parse_stmt_switch(Alc_Parser *p);
Alc_Ast *parse_stmt_defer(Alc_Parser *p);

static inline void add_error(Alc_Parser *p, Alc_Parser_Error error)
{
  ALC_ASSUME(p != nullptr);

  vector_push(p->errors, error);
}

static inline void add_error_unexpected_eof(Alc_Parser *p, usize pos)
{
  add_error(p, (Alc_Parser_Error){
                 .data = { { 0 } },
                 .pos = pos,
                 .len = 1,
                 .type = ALC_PARSER_ERROR_TYPE_UNEXPECTED_EOF,
               });
}

static inline void add_error_unexpected_token(Alc_Parser *p, usize pos, Alc_Token_Type expected)
{
  ALC_ASSUME(p != nullptr);
  Alc_Parser_Error error = {
    .data = {
      .UNEXPECTED_TOKEN = {
        .expected_token_types = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Token_Type)),
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

static inline void add_error_unexpected_token_v(Alc_Parser *p, usize pos,
                                                Alc_Token_Type *expected_v)
{
  ALC_ASSUME(p != nullptr);
  Alc_Parser_Error error = {
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

static inline void add_error_unexpected_value(Alc_Parser *p, usize pos, const char *expected)
{
  ALC_ASSUME(p != nullptr);
  Alc_Parser_Error error = {
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

static inline void add_error_unexpected_value_v(Alc_Parser *p, usize pos, const char **expected_v)
{
  ALC_ASSUME(p != nullptr);
  Alc_Parser_Error error = {
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

static inline void add_error_unexpected_whitespace(Alc_Parser *p, usize pos,
                                                   Alc_Token_Type expected_after)
{
  add_error(p, (Alc_Parser_Error){
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

#define _VERIFY_VALUE_V(_p, _pos, _expected_v, ...)                               \
  {                                                                               \
    if ALC_UNLIKELY (strcmp((_p)->tokens[(_pos)].value, (_expected_v)[0]) != 0) { \
      add_error_unexpected_value_v((_p), (_pos), (_expected_v));                  \
      __VA_ARGS__                                                                 \
      return nullptr;                                                             \
    }                                                                             \
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
