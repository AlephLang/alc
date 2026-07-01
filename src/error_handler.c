#include "error_handler.h"
#include <alc/ast.h>
#include <alc/parser.h>
#include "ansi.h"
#include <alc/token.h>
#include <alc/defs.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char s_buf[4096] = { 0 };
static inline void clear_s_buf(void)
{
  memset(s_buf, 0, sizeof(s_buf));
}

static inline Source_Line *src_to_source_lines(const char *src,
                                               usize *out_n); // Return value must be freed
static inline usize number_size(usize a);
static const char *token_to_string(Alc_Token *token);
static const char *token_type_to_string(Alc_Token_Type type);
static void insert_at(char *dst, const char *src, usize pos);
static void get_message_start(char *dst, usize n, const char *filename, usize line, usize start,
                              const char *message, Ansi_Mode ansi_mode);
static void highlight_token(Error_Handler *handler, char *dst, usize n, usize token_index,
                            Ansi_Mode ansi_mode, const char *message_after);
static void highlight_token_range(Error_Handler *handler, char *dst, usize n, usize start_index,
                                  usize end_index, Ansi_Mode ansi_mode, const char *message_after);
static void highlight_token_by_pointer(Error_Handler *handler, char *dst, usize n, Alc_Token *token,
                                       Ansi_Mode ansi_mode, const char *message_after);
static void highlight_after_token(Error_Handler *handler, char *dst, usize n, usize token_index,
                                  Ansi_Mode ansi_mode, const char *message_after);

Error_Handler error_handler_create(const char *filename, const char *src)
{
  ALC_ASSUME(filename != nullptr);
  ALC_ASSUME(src != nullptr);

  Error_Handler handler = { 0 };
  handler.filename = filename;
  handler.source_lines = src_to_source_lines(src, &handler.source_lines_num);
  handler.lnoffset = number_size(handler.source_lines_num);

  clear_s_buf();

  return handler;
}

void error_handler_destroy(Error_Handler *handler)
{
  ALC_ASSUME(handler != nullptr);

  if (handler->source_lines != nullptr)
    free(handler->source_lines);

  handler->source_lines = nullptr;
  handler->source_lines_num = 0;
}

void error_handler_set_tokens(Error_Handler *handler, Alc_Token *tokens, usize tokens_num)
{
  ALC_ASSUME(handler != nullptr);

  handler->tokens = tokens;
  handler->tokens_num = tokens_num;
}

void error_handler_handle_lexer_errors(Error_Handler *handler, Alc_Token *invalid_tokens,
                                       usize invalid_tokens_num)
{
  ALC_ASSUME(handler != nullptr);
  ALC_ASSUME(invalid_tokens != nullptr);
  ALC_ASSUME(invalid_tokens_num > 0);

  for (usize i = 0; i < invalid_tokens_num; i++) {
    Alc_Token *token = &invalid_tokens[i];

    char message_start[4096] = { 0 };
    get_message_start(message_start, 4096, handler->filename, token->line, token->pos, "error",
                      ANSI_COLOR_RED | ANSI_GRAPHICS_BOLD);

    char message[4096] = { 0 };
    snprintf(message, 4096, "unrecognized token '%s%s%s'", ansi_graphics(ANSI_GRAPHICS_BOLD),
             token->value, ansi_reset());

    char token_line[4096] = { 0 };
    highlight_token_by_pointer(handler, token_line, 4096, token,
                               ANSI_COLOR_RED | ANSI_GRAPHICS_BOLD, "");

    printf("%s%s\n%s", message_start, message, token_line);
  }
}

void error_handler_handle_parser_errors(Error_Handler *handler, Alc_Parser_Error *errors,
                                        usize errors_num)
{
  ALC_ASSUME(handler != nullptr);
  ALC_ASSUME(errors != nullptr);
  ALC_ASSUME(errors_num > 0);

  for (usize i = 0; i < errors_num; i++) {
    Alc_Parser_Error *error = &errors[i];

    char message_start[4096] = { 0 };
    switch (error->type) {
    case ALC_PARSER_ERROR_TYPE_UNEXPECTED_EOF: {
      get_message_start(message_start, 4096, handler->filename, handler->source_lines_num,
                        handler->tokens[handler->tokens_num - 1].pos +
                          handler->tokens[handler->tokens_num - 1].len,
                        "error", ANSI_COLOR_RED | ANSI_GRAPHICS_BOLD);
    } break;

    case ALC_PARSER_ERROR_TYPE_UNEXPECTED_TOKEN:
    case ALC_PARSER_ERROR_TYPE_UNEXPECTED_VALUE:
    case ALC_PARSER_ERROR_TYPE_UNEXPECTED_WHITESPACE:
    case ALC_PARSER_ERROR_TYPE_ASSIGN_OPERATOR_IN_NON_TOPLEVEL_EXPRESSION:
    case ALC_PARSER_ERROR_TYPE_TWO_ASSIGN_OPERATORS_IN_EXPRESSION: {
      get_message_start(message_start, 4096, handler->filename, handler->tokens[error->pos].line,
                        handler->tokens[error->pos].pos, "error",
                        ANSI_COLOR_RED | ANSI_GRAPHICS_BOLD);
    } break;
    }

    char reason[4096] = { 0 };
    switch (error->type) {
    case ALC_PARSER_ERROR_TYPE_UNEXPECTED_EOF: {
      snprintf(reason, 4096, "unexpected end of file");
    } break;

    case ALC_PARSER_ERROR_TYPE_UNEXPECTED_TOKEN: {
      snprintf(reason, 4096, "unexpected token '%s%s%s'", ansi_graphics(ANSI_GRAPHICS_BOLD),
               token_to_string(&handler->tokens[error->pos]), ansi_reset());
    } break;

    case ALC_PARSER_ERROR_TYPE_UNEXPECTED_VALUE: {
      snprintf(reason, 4096, "unexpected value '%s%s%s'", ansi_graphics(ANSI_GRAPHICS_BOLD),
               handler->tokens[error->pos].value, ansi_reset());
    } break;

    case ALC_PARSER_ERROR_TYPE_UNEXPECTED_WHITESPACE: {
      snprintf(reason, 4096, "unexpected whitespace after '%s%s%s'",
               ansi_graphics(ANSI_GRAPHICS_BOLD), token_to_string(&handler->tokens[error->pos]),
               ansi_reset());
    } break;

    case ALC_PARSER_ERROR_TYPE_ASSIGN_OPERATOR_IN_NON_TOPLEVEL_EXPRESSION: {
      snprintf(reason, 4096, "assign operator in non-toplevel expression");
    } break;

    case ALC_PARSER_ERROR_TYPE_TWO_ASSIGN_OPERATORS_IN_EXPRESSION: {
      snprintf(reason, 4096, "two assign operators in one expression");
    } break;
    }

    char highlight[4096] = { 0 };
    switch (error->type) {
    case ALC_PARSER_ERROR_TYPE_UNEXPECTED_EOF: {
      highlight_after_token(handler, highlight, 4096, handler->tokens_num - 1,
                            ANSI_COLOR_RED | ANSI_GRAPHICS_BOLD, "");
    } break;

    case ALC_PARSER_ERROR_TYPE_UNEXPECTED_TOKEN: {
      char msg_after[2048] = { 0 };
      char *p_msg_after = msg_after;
      usize written = 0;
      for (usize j = 0; j < error->data.UNEXPECTED_TOKEN.expected_token_types_num && written < 2048;
           j++) {
        const char *fmt = j > 0 ? ", %s" : " %s";
        written +=
          snprintf(p_msg_after, 2048 - written, fmt,
                   token_type_to_string(error->data.UNEXPECTED_TOKEN.expected_token_types[j]));
        p_msg_after = msg_after + strlen(msg_after);
      }

      if (error->len == 1)
        highlight_token(handler, highlight, 4096, error->pos, ANSI_COLOR_RED | ANSI_GRAPHICS_BOLD,
                        msg_after);
      else
        highlight_token_range(handler, highlight, 4096, error->pos, error->pos + error->len - 1,
                              ANSI_COLOR_RED | ANSI_GRAPHICS_BOLD, msg_after);
    } break;

    case ALC_PARSER_ERROR_TYPE_UNEXPECTED_VALUE: {
      char msg_after[2048] = { 0 };
      char *p_msg_after = msg_after;
      usize written = 0;
      for (usize j = 0; j < error->data.UNEXPECTED_VALUE.expected_values_num && written < 2048;
           j++) {
        const char *fmt = j > 0 ? ", \"%s\"" : " \"%s\"";
        written += snprintf(p_msg_after, 2048 - written, fmt,
                            error->data.UNEXPECTED_VALUE.expected_values[j]);
        p_msg_after = msg_after + strlen(msg_after);
      }

      highlight_token_range(handler, highlight, 4096, error->pos, error->pos + error->len - 1,
                            ANSI_COLOR_RED | ANSI_GRAPHICS_BOLD, msg_after);
    } break;

    case ALC_PARSER_ERROR_TYPE_UNEXPECTED_WHITESPACE: {
      char msg_after[2048] = { 0 };
      if (error->data.UNEXPECTED_WHITESPACE.expected_token_type != ALC_TOKEN_TYPE_ERROR)
        snprintf(msg_after, 2048, " %s",
                 token_type_to_string(error->data.UNEXPECTED_WHITESPACE.expected_token_type));

      highlight_after_token(handler, highlight, 4096, error->pos,
                            ANSI_COLOR_RED | ANSI_GRAPHICS_BOLD, msg_after);
    } break;

    case ALC_PARSER_ERROR_TYPE_ASSIGN_OPERATOR_IN_NON_TOPLEVEL_EXPRESSION: {
      highlight_token_range(handler, highlight, 4096, error->pos, error->pos + error->len - 1,
                            ANSI_COLOR_RED | ANSI_GRAPHICS_BOLD, "");
    } break;

    case ALC_PARSER_ERROR_TYPE_TWO_ASSIGN_OPERATORS_IN_EXPRESSION: {
      highlight_token_range(handler, highlight, 4096, error->pos, error->pos + error->len - 1,
                            ANSI_COLOR_RED | ANSI_GRAPHICS_BOLD, "");
    } break;
    }

    printf("%s%s\n%s", message_start, reason, highlight);
  }
}

static inline Source_Line *src_to_source_lines(const char *src, usize *out_n)
{
  const char *s;

  usize src_len = strlen(src);
  if (src_len == 0) {
    *out_n = 0;
    return nullptr;
  }

  usize lines_n = 0;
  for (s = src; *s; s++)
    if (*s == '\n' || *s == '\r')
      lines_n++;

  *out_n = lines_n;

  Source_Line *source_lines = malloc(sizeof(Source_Line) * lines_n);

  s = src;
  for (usize i = 0; i < lines_n; i++) {
    const char *start = s;
    const char *end = start;
    while (*s) {
      char c = *s;
      end = s++;
      if (c == '\n' || c == '\r')
        break;
    }
    source_lines[i].start_ptr = start;
    source_lines[i].length = (usize)(end - start);
  }

  return source_lines;
}

static inline usize number_size(usize a)
{
  if ALC_UNLIKELY (a == 0)
    return 1;

  usize len = 0;
  while (a > 0) {
    a /= 10;
    len++;
  }

  return len;
}

static const char *token_to_string(Alc_Token *token)
{
  switch (token->type) {
  case ALC_TOKEN_TYPE_ID:
  case ALC_TOKEN_TYPE_NUMBER:
  case ALC_TOKEN_TYPE_NUMBER_HEX:
  case ALC_TOKEN_TYPE_NUMBER_BIN:
  case ALC_TOKEN_TYPE_NUMBER_OCT:
  case ALC_TOKEN_TYPE_NUMBER_FLOAT:
  case ALC_TOKEN_TYPE_STRING:
  case ALC_TOKEN_TYPE_SYMBOL:
    return token->value;

  default:
    return token_type_to_string(token->type);
  }
}

static const char *token_type_to_string(Alc_Token_Type type)
{
  switch (type) {
#define ALC_TOKEN_TYPE_X(_name, _str_value) \
  case ALC_TOKEN_TYPE_FULL_NAME(_name):     \
    return (_str_value);
    ALC_TOKEN_TYPES
#undef ALC_TOKEN_TYPE_X
  }
  ALC_NOREACH();
}

static void insert_at(char *dst, const char *src, usize pos)
{
  if (src == nullptr)
    return;

  usize src_len = strlen(src);
  if (src_len == 0)
    return;

  usize dst_2_len = strlen(&dst[pos]);
  memmove(&dst[pos + src_len], &dst[pos], sizeof(char) * dst_2_len);
  memcpy(&dst[pos], src, sizeof(char) * src_len);
}

static void get_message_start(char *dst, usize n, const char *filename, usize line, usize start,
                              const char *message, Ansi_Mode ansi_mode)
{
  const char *color = ansi_color(ansi_mode);
  const char *graphics = ansi_graphics(ansi_mode);
  snprintf(dst, n, "%s%s:%zu:%zu: %s%s%s: %s", ansi_graphics(ANSI_GRAPHICS_BOLD), filename,
           line + 1, start, color, graphics, message, ansi_reset());
}

static void highlight_token(Error_Handler *handler, char *dst, usize n, usize token_index,
                            Ansi_Mode ansi_mode, const char *message_after)
{
  Alc_Token *token = &handler->tokens[token_index];
  highlight_token_by_pointer(handler, dst, n, token, ansi_mode, message_after);
}

static void highlight_token_by_pointer(Error_Handler *handler, char *dst, usize n, Alc_Token *token,
                                       Ansi_Mode ansi_mode, const char *message_after)
{
  Source_Line *source_line = &handler->source_lines[token->line];
  usize line_num = token->line + 1;
  usize line_num_len = number_size(line_num);
  char formatted_line[4096] = { 0 };
  memcpy(formatted_line, source_line->start_ptr, sizeof(char) * ALC_MIN(source_line->length, 3000));

  insert_at(formatted_line, ansi_reset(), token->pos + token->len);

  const char *color = ansi_color(ansi_mode);
  const char *graphics = ansi_graphics(ansi_mode);
  insert_at(formatted_line, graphics, token->pos);
  insert_at(formatted_line, color, token->pos);

  char mark[2048] = { 0 };
  memset(mark, ' ', sizeof(char) * ALC_MIN(2047, token->pos));
  memset(mark + token->pos, '~', sizeof(char) * ALC_MIN(2047 - token->pos, token->len));
  mark[token->pos] = '^';

  char fmt[4096];
  snprintf(fmt, n, " %zu | %s\n %%+%zus | %s%s%s%s\033[0m\n", line_num, formatted_line,
           line_num_len, color, graphics, mark, message_after);
  snprintf(dst, n, fmt, " ");
}

static void highlight_token_range(Error_Handler *handler, char *dst, usize n, usize start_index,
                                  usize end_index, Ansi_Mode ansi_mode, const char *message_after)
{
  b8 continue_after = handler->tokens[start_index].line != handler->tokens[end_index].line;

  Alc_Token *start_token = &handler->tokens[start_index];
  Alc_Token *end_token;
  do {
    end_token = &handler->tokens[end_index--];
  } while (start_token->line != end_token->line);

  if (start_token == end_token) {
    highlight_token(handler, dst, n, start_index, ansi_mode, message_after);
    return;
  }

  Source_Line *source_line = &handler->source_lines[start_token->line];
  usize line_num = start_token->line + 1;
  usize line_num_len = number_size(line_num);
  char formatted_line[4096] = { 0 };
  memcpy(formatted_line, source_line->start_ptr, sizeof(char) * ALC_MIN(source_line->length, 3000));

  insert_at(formatted_line, ansi_reset(), end_token->pos + end_token->len);

  const char *color = ansi_color(ansi_mode);
  const char *graphics = ansi_graphics(ansi_mode);
  insert_at(formatted_line, color, start_token->pos);
  insert_at(formatted_line, graphics, start_token->pos);

  usize range_length = end_token->pos - start_token->pos + end_token->len;

  char mark[2048] = { 0 };
  memset(mark, '~', sizeof(char) * ALC_MIN(2047, range_length + (continue_after ? 4 : 0)));
  mark[0] = '^';
  mark[2047] = 0;

  char fmt[4096];

  snprintf(fmt, n, " %zu | %s%s\n %%+%zus | %s%s %%+%zus%s\033[0m\n", line_num, formatted_line,
           continue_after ? " ..." : "", line_num_len, color, graphics,
           start_token->pos + range_length - 1 + (continue_after ? 4 : 0), message_after);
  snprintf(dst, n, fmt, " ", mark);
}

static void highlight_after_token(Error_Handler *handler, char *dst, usize n, usize token_index,
                                  Ansi_Mode ansi_mode, const char *message_after)
{
  Alc_Token *token = token_index >= handler->tokens_num ?
                       &handler->tokens[handler->tokens_num - 1] :
                       &handler->tokens[token_index];

  Source_Line *source_line = &handler->source_lines[token->line];
  usize line_num = token->line + 1;
  usize line_num_len = number_size(line_num);
  char src_line[4096] = { 0 };
  memcpy(src_line, source_line->start_ptr, sizeof(char) * source_line->length);

  const char *color = ansi_color(ansi_mode);
  const char *graphics = ansi_graphics(ansi_mode);

  const char *mark = "^";
  char fmt[4096];
  snprintf(fmt, n, " %zu | %s\n %%+%zus | %s%s %%+%zus%s\033[0m\n", line_num, src_line,
           line_num_len, color, graphics, token->pos + token->len, message_after);
  snprintf(dst, n, fmt, " ", mark);
}
