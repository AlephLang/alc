#ifndef __ALC_PARSER_H__
#define __ALC_PARSER_H__

#include <alc/token.h>
#include <alc/defs.h>
#include <alc/ast.h>

typedef struct {
  union {
    struct {
      alc_token_type_t *expected_token_types;
      usize expected_token_types_num;
    } UNEXPECTED_TOKEN;
    struct {
      const char **expected_values;
      usize expected_values_num;
    } UNEXPECTED_VALUE;
    struct {
      alc_token_type_t expected_token_type;
    } UNEXPECTED_WHITESPACE;
  } data;
  usize pos;
  usize len;
  enum {
    ALC_PARSER_ERROR_TYPE_UNKNOWN,
    ALC_PARSER_ERROR_TYPE_UNEXPECTED_EOF,
    ALC_PARSER_ERROR_TYPE_UNEXPECTED_TOKEN,
    ALC_PARSER_ERROR_TYPE_UNEXPECTED_VALUE,
    ALC_PARSER_ERROR_TYPE_UNEXPECTED_WHITESPACE,
    ALC_PARSER_ERROR_TYPE_ASSIGN_OPERATOR_IN_NON_TOPLEVEL_EXPRESSION,
    ALC_PARSER_ERROR_TYPE_TWO_ASSIGN_OPERATORS_IN_EXPRESSION,
  } type;
} alc_parser_error_t;

typedef struct __alc_parser_t alc_parser_t;

ALC_API alc_parser_t *alc_parser_create(alc_token_t *tokens, usize tokens_num);
ALC_API void alc_parser_destroy(alc_parser_t *parser);

ALC_API alc_ast_t *alc_parser_parse(alc_parser_t *parser);

ALC_API alc_parser_error_t *alc_parser_get_errors(const alc_parser_t *parser, usize *out_n);

#endif // __ALC_PARSER_H__
