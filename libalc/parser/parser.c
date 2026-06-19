#include "alc/parser.h"
#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "containers/vector.h"
#include "global.h"
#include "parser/parser_private.h"
#include <stdarg.h>
#include <string.h>

alc_parser_t *alc_parser_create(alc_token_t *tokens, usize tokens_num)
{
  ALC_ASSERT((tokens_num == 0 && tokens == nullptr) || (tokens_num > 0 && tokens != nullptr));

  alc_parser_t *parser = alloc_arena_allocate(&ctx()->arena, sizeof(alc_parser_t));
  parser->tokens = tokens;
  parser->tokens_num = tokens_num;
  parser->errors = vector_create(alc_parser_error_t);
  parser->pos = 0;
  return parser;
}

void alc_parser_destroy(alc_parser_t *parser)
{
  ALC_ASSERT(parser != nullptr);
  ALC_ASSERT(parser->errors != nullptr);
  vector_destroy(parser->errors);
  parser->errors = nullptr;
}

alc_ast_t *alc_parser_parse(alc_parser_t *parser)
{
  ALC_ASSERT(parser != nullptr);

  alc_ast_t **toplevels = vector_create(alc_ast_t *);

  while (parser->pos < parser->tokens_num) {
    alc_ast_t *top = parse_top(parser);
    if ALC_UNLIKELY (top == nullptr)
      continue;

    vector_push(toplevels, top);
  }

  alc_ast_t *root = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  root->pos = 0;
  root->kind = ALC_AST_KIND_ROOT;
  root->data.ROOT.toplevel_statements =
    vector_get_length(toplevels) > 0 ?
      vector_to_array(toplevels, &root->data.ROOT.toplevel_statements_num) :
      nullptr;
  vector_destroy(toplevels);

  return root;
}

alc_ast_t *parse_ids(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  if (strcmp(p->tokens[p->pos].value, "struct") == 0)
    return parse_struct(p, ALC_AST_STRUCT_KIND_DEFAULT);
  else if (strcmp(p->tokens[p->pos].value, "partial") == 0)
    return parse_partial_struct(p);
  else if (strcmp(p->tokens[p->pos].value, "enum") == 0)
    return parse_enum(p);
  else if (strcmp(p->tokens[p->pos].value, "union") == 0)
    return parse_union(p);
  else if (strcmp(p->tokens[p->pos].value, "using") == 0)
    return parse_typedef(p);
  else if (strcmp(p->tokens[p->pos].value, "scope") == 0)
    return parse_scope(p);
  else if (strcmp(p->tokens[p->pos].value, "extern") == 0)
    return parse_extern(p);
  else if (strcmp(p->tokens[p->pos].value, "import") == 0)
    return parse_import(p);
  if (strcmp(p->tokens[p->pos].value, "return") == 0)
    return parse_stmt_return(p);
  else if (strcmp(p->tokens[p->pos].value, "goto") == 0)
    return parse_stmt_goto(p);
  else if (strcmp(p->tokens[p->pos].value, "break") == 0)
    return parse_stmt_break(p);
  else if (strcmp(p->tokens[p->pos].value, "continue") == 0)
    return parse_stmt_continue(p);
  else if (strcmp(p->tokens[p->pos].value, "fallthrough") == 0)
    return parse_stmt_fallthrough(p);
  else if (strcmp(p->tokens[p->pos].value, "if") == 0)
    return parse_stmt_if(p);
  else if (strcmp(p->tokens[p->pos].value, "else") == 0)
    return parse_stmt_else(p);
  else if (strcmp(p->tokens[p->pos].value, "loop") == 0)
    return parse_stmt_loop(p);
  else if (strcmp(p->tokens[p->pos].value, "while") == 0)
    return parse_stmt_while(p);
  else if (strcmp(p->tokens[p->pos].value, "do") == 0)
    return parse_stmt_do_while(p);
  else if (strcmp(p->tokens[p->pos].value, "for") == 0)
    return parse_stmt_for(p);
  else if (strcmp(p->tokens[p->pos].value, "foreach") == 0)
    return parse_stmt_foreach(p);
  else if (strcmp(p->tokens[p->pos].value, "switch") == 0)
    return parse_stmt_switch(p);
  else if (strcmp(p->tokens[p->pos].value, "defer") == 0)
    return parse_stmt_defer(p);
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

  return (void *)-1;
}

alc_parser_error_t *alc_parser_get_errors(const alc_parser_t *parser, usize *out_n)
{
  ALC_ASSERT(parser != nullptr);
  ALC_ASSERT(parser->errors != nullptr);
  ALC_ASSERT(out_n != nullptr);

  *out_n = vector_get_length(parser->errors);
  return parser->errors;
}

alc_token_t *peek(const alc_parser_t *p, s32 adv)
{
  ALC_ASSUME(p != nullptr);
  return p->pos + adv < p->tokens_num ? &p->tokens[p->pos + adv] : nullptr;
}

void add_error_unexpected_token(alc_parser_t *p, usize pos, usize expected_token_types_num, ...)
{
  alc_parser_error_t unexpected_token_error = {
    .data = { { 0 } },
    .pos = pos,
    .len = 1,
    .type = ALC_PARSER_ERROR_TYPE_UNEXPECTED_TOKEN,
  };

  if ALC_LIKELY (expected_token_types_num > 0) {
    alc_token_type_t *expected_token_types =
      vector_reserve(alc_token_type_t, expected_token_types_num);

    __builtin_va_list ap;
    va_start(ap, expected_token_types_num);
    for (usize i = 0; i < expected_token_types_num; i++) {
      vector_push(expected_token_types, va_arg(ap, alc_token_type_t));
    }
    va_end(ap);

    unexpected_token_error.data.UNEXPECTED_TOKEN.expected_token_types = vector_to_array(
      expected_token_types, &unexpected_token_error.data.UNEXPECTED_TOKEN.expected_token_types_num);

    vector_destroy(expected_token_types);
  }

  add_error(p, unexpected_token_error);
}

void add_error_unexpected_value(alc_parser_t *p, usize pos, usize expected_values_num, ...)
{
  alc_parser_error_t unexpected_value_error = {
    .data = { { 0 } },
    .pos = pos,
    .len = 1,
    .type = ALC_PARSER_ERROR_TYPE_UNEXPECTED_VALUE,
  };

  if ALC_LIKELY (expected_values_num > 0) {
    const char **expected_values = vector_reserve(const char *, expected_values_num);

    __builtin_va_list ap;
    va_start(ap, expected_values_num);
    for (usize i = 0; i < expected_values_num; i++) {
      vector_push(expected_values, va_arg(ap, const char *));
    }
    va_end(ap);

    unexpected_value_error.data.UNEXPECTED_VALUE.expected_values = vector_to_array(
      expected_values, &unexpected_value_error.data.UNEXPECTED_VALUE.expected_values_num);

    vector_destroy(expected_values);
  }

  add_error(p, unexpected_value_error);
}
