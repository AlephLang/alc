#include "error_handler.h"
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
// static const char *token_type_to_string(Alc_Token_Type type);
static void insert_at(char *dst, const char *src, usize pos);
static void get_message_start(char *dst, usize n, const char *filename, usize line, usize start,
                              const char *message, Ansi_Mode ansi_mode);
static void get_highlighted_token(Error_Handler *handler, char *dst, usize n, Alc_Token *token,
                                  Ansi_Mode ansi_mode, const char *message_after);
// static void get_highlight_after_token(Error_Handler *handler, char *dst, usize n, Alc_Token *token,
//                                       Ansi_Mode ansi_mode, const char *message_after);

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
    get_highlighted_token(handler, token_line, 4096, token, ANSI_COLOR_RED | ANSI_GRAPHICS_BOLD,
                          "");

    printf("%s%s\n%s", message_start, message, token_line);
  }
}

void error_handler_handle_parser_errors(Error_Handler *handler, Alc_Parser_Error *errors,
                                        usize errors_num)
{
  ALC_ASSUME(handler != nullptr);
  ALC_ASSUME(errors != nullptr);
  ALC_ASSUME(errors_num > 0);

  ALC_TODO("Parser errors.");
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

/*
static const char *token_type_to_string(Alc_Token_Type type)
{
  switch (type) {
#define ALC_TOKEN_TYPE_X(_name, _str_value) \
  case ALC_TOKEN_TYPE_FULL_NAME(_name):     \
    return (_str_value);
    ALC_TOKEN_TYPES
#undef ALC_TOKEN_TYPE_X
  }
}
*/

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
  const char *color = ansi_color(ansi_get_color(ansi_mode));
  const char *graphics = ansi_graphics(ansi_get_graphics(ansi_mode));
  snprintf(dst, n, "%s%s:%zu:%zu: %s%s%s: %s", ansi_graphics(ANSI_GRAPHICS_BOLD), filename,
           line + 1, start, color, graphics, message, ansi_reset());
}

static void get_highlighted_token(Error_Handler *handler, char *dst, usize n, Alc_Token *token,
                                  Ansi_Mode ansi_mode, const char *message_after)
{
  Source_Line *source_line = &handler->source_lines[token->line];
  usize line_num = token->line + 1;
  usize line_num_len = number_size(line_num);
  char formatted_line[4096] = { 0 };
  memcpy(formatted_line, source_line->start_ptr, sizeof(char) * source_line->length);

  insert_at(formatted_line, ansi_reset(), token->pos + token->len);

  const char *color = ansi_color(ansi_get_color(ansi_mode));
  const char *graphics = ansi_graphics(ansi_get_graphics(ansi_mode));
  insert_at(formatted_line, graphics, token->pos);
  insert_at(formatted_line, color, token->pos);

  char mark[2048];
  mark[0] = '^';
  mark[token->len] = 0;
  for (usize i = 1; i < ALC_MIN(token->len, 2048); i++)
    mark[i] = '~';

  char fmt[4096];
  snprintf(fmt, n, " %zu | %s\n %%+%zus | %s%s %%+%zus%s\033[0m\n", line_num, formatted_line,
           line_num_len, color, graphics, token->pos + token->len - 1, message_after);
  snprintf(dst, n, fmt, " ", mark);
}

/*
static void get_highlight_after_token(Error_Handler *handler, char *dst, usize n, Alc_Token *token,
                                      Ansi_Mode ansi_mode, const char *message_after)
{
  Source_Line *source_line = &handler->source_lines[token->line];
  usize line_num = token->line + 1;
  usize line_num_len = number_size(line_num);
  char src_line[4096] = { 0 };
  memcpy(src_line, source_line->start_ptr, sizeof(char) * source_line->length);

  const char *color = ansi_color(ansi_get_color(ansi_mode));
  const char *graphics = ansi_graphics(ansi_get_graphics(ansi_mode));

  const char *mark = "^";
  char fmt[4096];
  snprintf(fmt, n, " %zu | %s\n %%+%zus | %s%s %%+%zus%s\033[0m\n", line_num, src_line,
           line_num_len, color, graphics, token->pos + token->len + 1, message_after);
  snprintf(dst, n, fmt, " ", mark);
}
*/
