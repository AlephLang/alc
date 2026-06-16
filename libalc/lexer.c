#include "alc/lexer.h"
#include "alc/defs.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "containers/vector.h"
#include "global.h"
#include <ctype.h>
#include <string.h>

#define CHAR_CASES             \
  CHAR_CASE_X('.', PERIOD)     \
  CHAR_CASE_X(',', COMMA)      \
  CHAR_CASE_X('!', EXCLMARK)   \
  CHAR_CASE_X('*', ASTERISK)   \
  CHAR_CASE_X('+', PLUS)       \
  CHAR_CASE_X('-', MINUS)      \
  CHAR_CASE_X('^', CIRCUMFLEX) \
  CHAR_CASE_X('&', AMPERSAND)  \
  CHAR_CASE_X('|', PIPE)       \
  CHAR_CASE_X('(', LPAREN)     \
  CHAR_CASE_X(')', RPAREN)     \
  CHAR_CASE_X('[', LBRACK)     \
  CHAR_CASE_X(']', RBRACK)     \
  CHAR_CASE_X('{', LCBRACK)    \
  CHAR_CASE_X('}', RCBRACK)    \
  CHAR_CASE_X('<', LARROW)     \
  CHAR_CASE_X('>', RARROW)     \
  CHAR_CASE_X('=', EQ)         \
  CHAR_CASE_X('%', PERCENT)    \
  CHAR_CASE_X('@', AT)         \
  CHAR_CASE_X(':', COLON)      \
  CHAR_CASE_X(';', SEMICOLON)  \
  CHAR_CASE_X('#', HASH)       \
  CHAR_CASE_X('~', TILDE)

static alc_token_t gen_token(const char *value, alc_token_type_t type, usize line, usize llp,
                             usize raw_pos, usize len, b8 has_whitespace_after);
static inline alc_token_t process_error(alc_lexer_t *l);
static inline alc_token_t process_string(alc_lexer_t *l);
static inline alc_token_t process_symbol(alc_lexer_t *l);
static inline alc_token_t process_id(alc_lexer_t *l);
static inline alc_token_t process_num(alc_lexer_t *l);
static inline alc_token_t process_num_hex(alc_lexer_t *l);
static inline alc_token_t process_num_bin(alc_lexer_t *l);
static inline alc_token_t process_num_oct(alc_lexer_t *l);
static inline b8 is_start_of_id(char c);
static inline b8 is_part_of_id(char c);
static inline b8 is_processable(char c);
static inline void skip_whitespace(alc_lexer_t *l);
static inline void skip_c_comments(alc_lexer_t *l);
static inline void skip_cpp_comments(alc_lexer_t *l);
static inline b8 is_whitespace_at(const alc_lexer_t *l, usize pos);
static inline char peek(const alc_lexer_t *l, usize adv);
static char *memcpy_cond(char *dst, const char *src, usize n, b8 (*cond)(char c));

static b8 skip_separators(char c)
{
  return c != '\'' && c != '_';
}

alc_lexer_t alc_lexer_create(const char *src)
{
  ALC_ASSERT(src != nullptr);

  return (alc_lexer_t){
    .src = src,
    .src_len = strlen(src),
  };
}

b8 alc_lexer_tokenize(alc_lexer_t *lexer, alc_token_t **out_tokens, usize *out_n)
{
  ALC_ASSERT(lexer != nullptr);
  ALC_ASSERT(out_tokens != nullptr);
  ALC_ASSERT(out_n != nullptr);

  alc_token_t *tokens = vector_create(alc_token_t);
  b8 failed = false;

  while (lexer->pos < lexer->src_len) {
    skip_whitespace(lexer);

    if ALC_UNLIKELY (lexer->pos >= lexer->src_len)
      break;

    char c = lexer->src[lexer->pos];
    switch (c) {
#define CHAR_CASE_X(_char, _type_name)                                                            \
  case (_char): {                                                                                 \
    if ALC_LIKELY (!failed) {                                                                     \
      vector_push(tokens,                                                                         \
                  gen_token(nullptr, ALC_TOKEN_TYPE_FULL_NAME(_type_name), lexer->line,           \
                            lexer->llp, lexer->pos, 1, is_whitespace_at(lexer, lexer->pos + 1))); \
    }                                                                                             \
    lexer->pos++;                                                                                 \
  } break;
      CHAR_CASES
#undef CHAR_CASE_X

    case '/': {
      char peeked = peek(lexer, 1);
      switch (peeked) {
      case '/': {
        skip_cpp_comments(lexer);
      } break;
      case '*': {
        skip_c_comments(lexer);
      } break;
      default:
        if ALC_LIKELY (!failed) {
          vector_push(tokens, gen_token(nullptr, ALC_TOKEN_TYPE_SLASH, lexer->line, lexer->llp,
                                        lexer->pos, 1, is_whitespace_at(lexer, lexer->pos + 1)));
        }
        lexer->pos++;
        break;
      }
    } break;

    case '"': {
      alc_token_t string_token = process_string(lexer);
      if ALC_LIKELY (!failed)
        vector_push(tokens, string_token);
      break;
    }
    case '\'': {
      alc_token_t symbol_token = process_symbol(lexer);
      if ALC_LIKELY (!failed)
        vector_push(tokens, symbol_token);
      break;
    }
    default:
      if (is_start_of_id(c)) {
        alc_token_t id_token = process_id(lexer);
        if ALC_LIKELY (!failed)
          vector_push(tokens, id_token);
        break;
      } else if (isdigit(c)) {
        alc_token_t num_token = process_num(lexer);
        if ALC_LIKELY (!failed)
          vector_push(tokens, num_token);
        break;
      }

      if ALC_LIKELY (!failed) {
        failed = true;
        vector_clear(tokens);
      }
      printf("FAILURE: %zu:%zu: %c\n", lexer->line + 1, lexer->pos - lexer->llp,
             lexer->src[lexer->pos]);
      vector_push(tokens, process_error(lexer));
      break;
    }
  }

  if ALC_LIKELY (vector_get_length(tokens) > 0)
    *out_tokens = vector_to_array(tokens, out_n);
  else {
    *out_tokens = nullptr;
    *out_n = 0;
  }
  vector_destroy(tokens);
  return !failed;
}

static alc_token_t gen_token(const char *value, alc_token_type_t type, usize line, usize llp,
                             usize raw_pos, usize len, b8 has_whitespace_after)
{
  char *value_in_arena = nullptr;
  if (value != nullptr) {
    usize val_len = strlen(value);
    value_in_arena = alloc_arena_allocate_aligned(&ctx()->arena, val_len + 1, 1);
    memcpy(value_in_arena, value, val_len + 1);
  }

  return (alc_token_t){
    .line = line,
    .pos = raw_pos - llp,
    .len = len,
    .value = value_in_arena,
    .type = type,
    .has_whitespace_after = has_whitespace_after,
  };
}

static inline alc_token_t process_error(alc_lexer_t *l)
{
  usize start = l->pos;
  for (; l->pos < l->src_len && !is_processable(l->src[l->pos]); l->pos++)
    ;

  usize len = l->pos - start;
  const char *s_ptr = &l->src[start];
  char *value = alloc_arena_allocate_aligned(&ctx()->arena, sizeof(char) * (len + 1), 1);
  memcpy(value, s_ptr, len * sizeof(char));
  value[len] = 0;

  alc_token_t token = gen_token(value, ALC_TOKEN_TYPE_ERROR, l->line, l->llp, start, len,
                                is_whitespace_at(l, l->pos));
  return token;
}

static char *get_string_value(alc_lexer_t *l, char terminator)
{
  l->pos++;

  usize start = l->pos;
  while (l->pos < l->src_len) {
    char c = l->src[l->pos];
    if (c == terminator) {
      l->pos++;
      break;
    } else if (c == '\n' || c == '\r') {
      l->pos++;
      l->line++;
      l->llp = l->pos;
      continue;
    } else if (c == '\\' && peek(l, 1) == terminator) {
      l->pos += 2;
      continue;
    }
    l->pos++;
  }

  usize len = l->pos - start;
  char *data = alloc_arena_allocate_aligned(&ctx()->arena, len, 1);
  memcpy(data, &l->src[start], len - 1);
  data[len - 1] = 0;

  return data;
}

static inline alc_token_t process_string(alc_lexer_t *l)
{
  usize start = l->pos, line = l->line, llp = l->llp;
  char *value = get_string_value(l, '\"');
  return gen_token(value, ALC_TOKEN_TYPE_STRING, line, llp, start, l->pos - start,
                   is_whitespace_at(l, l->pos));
}

static inline alc_token_t process_symbol(alc_lexer_t *l)
{
  usize start = l->pos, line = l->line, llp = l->llp;
  char *value = get_string_value(l, '\'');
  return gen_token(value, ALC_TOKEN_TYPE_SYMBOL, line, llp, start, l->pos - start,
                   is_whitespace_at(l, l->pos));
}

static inline alc_token_t process_id(alc_lexer_t *l)
{
  usize start = l->pos;
  for (; l->pos < l->src_len && is_part_of_id(l->src[l->pos]); l->pos++)
    ;
  usize len = l->pos - start;
  char *value = alloc_arena_allocate_aligned(&ctx()->arena, len + 1, 1);
  memcpy(value, &l->src[start], len);
  value[len] = 0;

  return gen_token(value, ALC_TOKEN_TYPE_ID, l->line, l->llp, start, len,
                   is_whitespace_at(l, l->pos));
}

static inline alc_token_t process_num(alc_lexer_t *l)
{
  if (l->src[l->pos] == '0') {
    switch (peek(l, 1)) {
    case 'x':
      return process_num_hex(l);
    case 'b':
      return process_num_bin(l);
    case 'o':
      return process_num_oct(l);
    default:
      break;
    }
  }

  b8 is_float = false;
  usize start = l->pos;
  for (; l->pos < l->src_len && (isdigit(l->src[l->pos]) || l->src[l->pos] == '\'' ||
                                 l->src[l->pos] == '_' || l->src[l->pos] == '.');
       l->pos++) {
    if (l->src[l->pos] == '.') {
      if ALC_UNLIKELY (is_float)
        break;
      is_float = true;
    }
  }

  usize len = l->pos - start;
  char *value = alloc_arena_allocate_aligned(&ctx()->arena, len + 1, 1);
  char *end = memcpy_cond(value, &l->src[start], len, skip_separators);
  *end = 0;
  return gen_token(value, is_float ? ALC_TOKEN_TYPE_NUMBER_FLOAT : ALC_TOKEN_TYPE_NUMBER, l->line,
                   l->llp, start, len, is_whitespace_at(l, l->pos));
}

static inline alc_token_t process_num_hex(alc_lexer_t *l)
{
  l->pos += 2;
  usize start = l->pos;
  for (; l->pos < l->src_len &&
         (isxdigit(l->src[l->pos]) || l->src[l->pos] == '\'' || l->src[l->pos] == '_');
       l->pos++)
    ;
  usize len = l->pos - start;
  char *value = alloc_arena_allocate_aligned(&ctx()->arena, len + 1, 1);
  char *end = memcpy_cond(value, &l->src[start], len, skip_separators);
  *end = 0;

  return gen_token(value, ALC_TOKEN_TYPE_NUMBER_HEX, l->line, l->llp, start - 2, len + 2,
                   is_whitespace_at(l, l->pos));
}

static inline alc_token_t process_num_bin(alc_lexer_t *l)
{
  l->pos += 2;
  usize start = l->pos;
  for (; l->pos < l->src_len && (l->src[l->pos] == '0' || l->src[l->pos] == '1' ||
                                 l->src[l->pos] == '\'' || l->src[l->pos] == '_');
       l->pos++)
    ;
  usize len = l->pos - start;
  char *value = alloc_arena_allocate_aligned(&ctx()->arena, len + 1, 1);
  char *end = memcpy_cond(value, &l->src[start], len, skip_separators);
  *end = 0;

  return gen_token(value, ALC_TOKEN_TYPE_NUMBER_BIN, l->line, l->llp, start - 2, len + 2,
                   is_whitespace_at(l, l->pos));
}

static inline alc_token_t process_num_oct(alc_lexer_t *l)
{
  l->pos += 2;
  usize start = l->pos;
  for (; l->pos < l->src_len && ((l->src[l->pos] >= '0' && l->src[l->pos] <= '7') ||
                                 l->src[l->pos] == '\'' || l->src[l->pos] == '_');
       l->pos++)
    ;
  usize len = l->pos - start;
  char *value = alloc_arena_allocate_aligned(&ctx()->arena, len + 1, 1);
  char *end = memcpy_cond(value, &l->src[start], len, skip_separators);
  *end = 0;

  return gen_token(value, ALC_TOKEN_TYPE_NUMBER_OCT, l->line, l->llp, start - 2, len + 2,
                   is_whitespace_at(l, l->pos));
}

static inline b8 is_start_of_id(char c)
{
  return isalpha(c) || c == '_' || c == '$';
}

static inline b8 is_part_of_id(char c)
{
  return is_start_of_id(c) || isdigit(c);
}

static inline b8 is_processable(char c)
{
  switch (c) {
#define CHAR_CASE_X(_char, _type_name) case (_char):
    CHAR_CASES
#undef CHAR_CASE_X
  case '"':
  case '\'':
    return true;
  default:
    return is_start_of_id(c) || isdigit(c) || isspace(c);
  }
}

static inline void skip_whitespace(alc_lexer_t *l)
{
  for (; l->pos < l->src_len && isspace(l->src[l->pos]); l->pos++) {
    switch (l->src[l->pos]) {
    case '\n':
    case '\r':
      l->llp = l->pos + 1;
      l->line++;
      break;
    default:
      break;
    }
  }
}

static inline void skip_c_comments(alc_lexer_t *l)
{
  for (; l->pos < l->src_len; l->pos++) {
    char c1 = peek(l, 0), c2 = peek(l, 1);
    if (c1 == '*' && c2 == '/') {
      l->pos += 2;
      break;
    }

    if (c1 == '\n' || c1 == '\r') {
      l->line++;
      l->llp = l->pos + 1;
    }
  }
}

static inline void skip_cpp_comments(alc_lexer_t *l)
{
  for (; l->pos < l->src_len; l->pos++) {
    char c = l->src[l->pos];
    if (c == '\n' || c == '\r') {
      l->pos++;
      l->line++;
      l->llp = l->pos;
      break;
    }
  }
}

static inline b8 is_whitespace_at(const alc_lexer_t *l, usize pos)
{
  return pos >= l->src_len || isspace(l->src[pos]);
}

static inline char peek(const alc_lexer_t *l, usize adv)
{
  return l->pos + adv >= l->src_len ? 0 : l->src[l->pos + adv];
}

static char *memcpy_cond(char *dst, const char *src, usize n, b8 (*cond)(char c))
{
  for (; n; n--, src++) {
    if (cond(*src))
      *dst++ = *src;
  }
  return dst;
}
