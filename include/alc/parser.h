#ifndef __ALC_PARSER_H__
#define __ALC_PARSER_H__

#include <alc/vector.h>
#include <alc/token.h>
#include <alc/defs.h>
#include <alc/ast.h>

typedef struct {
  union {
    struct {
      Alc_Token_Type *expected_token_types;
      usize expected_token_types_num;
    } UNEXPECTED_TOKEN;
    struct {
      const char **expected_values;
      usize expected_values_num;
    } UNEXPECTED_VALUE;
    struct {
      Alc_Token_Type expected_token_type;
    } UNEXPECTED_WHITESPACE;
  } data;
  usize pos;
  usize len;
  enum {
    ALC_PARSER_ERROR_TYPE_UNEXPECTED_EOF,
    ALC_PARSER_ERROR_TYPE_UNEXPECTED_TOKEN,
    ALC_PARSER_ERROR_TYPE_UNEXPECTED_VALUE,
    ALC_PARSER_ERROR_TYPE_UNEXPECTED_WHITESPACE,
    ALC_PARSER_ERROR_TYPE_ASSIGN_OPERATOR_IN_NON_TOPLEVEL_EXPRESSION,
    ALC_PARSER_ERROR_TYPE_TWO_ASSIGN_OPERATORS_IN_EXPRESSION,
  } type;
} Alc_Parser_Error;

typedef struct __Alc_Parser {
  Alc_Token *tokens;
  usize tokens_num;

  Alc_Vector(Alc_Parser_Error) errors;

  usize pos;
} Alc_Parser;

ALC_API Alc_Parser *alc_parser_create(Alc_Token *tokens, usize tokens_num);
ALC_API void alc_parser_destroy(Alc_Parser *parser);

ALC_API Alc_Ast *alc_parser_parse(Alc_Parser *parser);

ALC_API Alc_Vector(Alc_Parser_Error) alc_parser_get_errors(const Alc_Parser *parser);

#endif // __ALC_PARSER_H__
