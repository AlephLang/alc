#include "alc/token.h"

void alc_token_to_string(alc_token_t *token, char *buf, usize n)
{
  ALC_ASSERT(token != nullptr);
  ALC_ASSERT(buf != nullptr);
  ALC_ASSERT(n > 0);

  const char *token_name;
  switch (token->type) {
#define ALC_TOKEN_TYPE_X(_name, _str_value) \
  case ALC_TOKEN_TYPE_FULL_NAME(_name):     \
    token_name = #_name;                    \
    break;
    ALC_TOKEN_TYPES
#undef ALC_TOKEN_TYPE_X
  }

  static const char *true_false_strs[2] = { "false", "true" };

  switch (token->type) {
  case ALC_TOKEN_TYPE_ERROR:
  case ALC_TOKEN_TYPE_ID:
  case ALC_TOKEN_TYPE_NUMBER:
  case ALC_TOKEN_TYPE_NUMBER_HEX:
  case ALC_TOKEN_TYPE_NUMBER_BIN:
  case ALC_TOKEN_TYPE_NUMBER_OCT:
  case ALC_TOKEN_TYPE_NUMBER_FLOAT:
  case ALC_TOKEN_TYPE_STRING:
  case ALC_TOKEN_TYPE_SYMBOL:
    ALC_ASSERT(token->value != nullptr);
    snprintf(
      buf, n,
      "{ type: { %s { value: \"%s\" } }, line: %zu, pos: %zu, len: %zu, has_whitespace_after: %s }",
      token_name, token->value, token->line, token->pos, token->len,
      true_false_strs[!!token->has_whitespace_after]);
    break;
  default:
    snprintf(buf, n, "{ type: { %s }, line: %zu, pos: %zu, len: %zu, has_whitespace_after: %s }",
             token_name, token->line, token->pos, token->len,
             true_false_strs[!!token->has_whitespace_after]);
    break;
  }
}
