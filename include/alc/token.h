#ifndef __ALC_TOKEN_H__
#define __ALC_TOKEN_H__

// #define ALC_TOKEN_TYPE_X(_name, _str_value)

#include "alc/defs.h"
#define ALC_TOKEN_TYPES                         \
  ALC_TOKEN_TYPE_X(ERROR, "<error>")            \
  ALC_TOKEN_TYPE_X(ID, "<id>")                  \
  ALC_TOKEN_TYPE_X(NUMBER, "<num>")             \
  ALC_TOKEN_TYPE_X(NUMBER_HEX, "<num-hex>")     \
  ALC_TOKEN_TYPE_X(NUMBER_BIN, "<num-bin>")     \
  ALC_TOKEN_TYPE_X(NUMBER_OCT, "<num-oct>")     \
  ALC_TOKEN_TYPE_X(NUMBER_FLOAT, "<num-float>") \
  ALC_TOKEN_TYPE_X(STRING, "<string>")          \
  ALC_TOKEN_TYPE_X(SYMBOL, "<symbol>")          \
  ALC_TOKEN_TYPE_X(LPAREN, "(")                 \
  ALC_TOKEN_TYPE_X(RPAREN, ")")                 \
  ALC_TOKEN_TYPE_X(LBRACK, "[")                 \
  ALC_TOKEN_TYPE_X(RBRACK, "]")                 \
  ALC_TOKEN_TYPE_X(LCBRACK, "{")                \
  ALC_TOKEN_TYPE_X(RCBRACK, "}")                \
  ALC_TOKEN_TYPE_X(LARROW, "<")                 \
  ALC_TOKEN_TYPE_X(RARROW, ">")                 \
  ALC_TOKEN_TYPE_X(COLON, ":")                  \
  ALC_TOKEN_TYPE_X(SEMICOLON, ";")              \
  ALC_TOKEN_TYPE_X(COMMA, ",")                  \
  ALC_TOKEN_TYPE_X(PERIOD, ".")                 \
  ALC_TOKEN_TYPE_X(AMPERSAND, "&")              \
  ALC_TOKEN_TYPE_X(PIPE, "|")                   \
  ALC_TOKEN_TYPE_X(CIRCUMFLEX, "^")             \
  ALC_TOKEN_TYPE_X(TILDE, "~")                  \
  ALC_TOKEN_TYPE_X(EXCLMARK, "~")               \
  ALC_TOKEN_TYPE_X(PLUS, "+")                   \
  ALC_TOKEN_TYPE_X(MINUS, "-")                  \
  ALC_TOKEN_TYPE_X(ASTERISK, "*")               \
  ALC_TOKEN_TYPE_X(SLASH, "/")                  \
  ALC_TOKEN_TYPE_X(PERCENT, "%")                \
  ALC_TOKEN_TYPE_X(EQ, "=")                     \
  ALC_TOKEN_TYPE_X(AT, "@")                     \
  ALC_TOKEN_TYPE_X(HASH, "#")

#define ALC_TOKEN_TYPE_FULL_NAME(_name) ALC_TOKEN_TYPE_##_name

typedef enum {
#define ALC_TOKEN_TYPE_X(_name, _str_value) ALC_TOKEN_TYPE_FULL_NAME(_name),
  ALC_TOKEN_TYPES
#undef ALC_TOKEN_TYPE_X
} alc_token_type_t;

typedef struct {
  usize line, pos, len;
  char *value;
  alc_token_type_t type;
  b8 has_whitespace_after;
} alc_token_t;

ALC_API void alc_token_to_string(alc_token_t *token, char *buf, usize n);

#endif // __ALC_TOKEN_H__
