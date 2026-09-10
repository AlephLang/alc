#include "error_handler.h"
#include <alc/analyzer.h>
#include <alc/defs.h>
#include <alc/entry.h>
#include <alc/parser.h>
#include <alc/sourcefile.h>
#include <alc/token.h>
#include "ansi.h"
#include <stdio.h>
#include <string.h>

#define ALC_COMPILER_NAME "alc"

typedef struct {
  const char *src;
  Alc_Token *tokens;
  usize tokens_len;
} Highlight_Data;

static void error_directory(Alc_Error *error);
static void error_file(Alc_Error *error);
static void error_lexer(Alc_Error *error);
static void error_parser(Alc_Error *error);
static void error_analysis(Alc_Error *error);

static void error_analysis_unresolvable_import(Alc_Analysis_Error *error);
static void error_analysis_type_redef(Alc_Analysis_Error *error);
static void error_analysis_variable_redecl(Alc_Analysis_Error *error);
static void error_analysis_function_redef(Alc_Analysis_Error *error);

static void internal_message(char *buf, usize n, const char *message, const char *text,
                             Ansi_Mode ansi_mode);
static void file_message(char *buf, usize n, const char *file_path, const char *message,
                         const char *text, Ansi_Mode ansi_mode);
static void highlight_token(char *dst, usize n, Highlight_Data *data, usize index,
                            Ansi_Mode ansi_mode, const char *message);
static void highlight_token_span(char *dst, usize n, Highlight_Data *data, usize index, usize len,
                                 Ansi_Mode ansi_mode, const char *message);
static void highlight_eof(char *dst, usize n, const char *src, Ansi_Mode ansi_mode,
                          const char *message);
static const char *token_to_string(Alc_Token *token);
static const char *token_type_to_string(Alc_Token_Type type);

void handle_error(Alc_Error *error)
{
  switch (error->kind) {
  case ALC_ERROR_KIND_DIRECTORY: {
    error_directory(error);
  } break;

  case ALC_ERROR_KIND_FILE: {
    error_file(error);
  } break;

  case ALC_ERROR_KIND_LEXER: {
    error_lexer(error);
  } break;

  case ALC_ERROR_KIND_PARSER: {
    error_parser(error);
  } break;

  case ALC_ERROR_KIND_ANALYSIS: {
    error_analysis(error);
  } break;
  }
}

static void error_directory(Alc_Error *error)
{
  const char *path = error->DIRECTORY.path;
#define _REQUIRED_SIZE (sizeof(error->DIRECTORY.path) + 64)

  char text[_REQUIRED_SIZE];
  snprintf(text, _REQUIRED_SIZE, "%s: Failed to open directory", path);

  char error_message[_REQUIRED_SIZE];
  internal_message(error_message, _REQUIRED_SIZE, "error", text,
                   ANSI_GRAPHICS_BOLD | ANSI_COLOR_RED);

  fprintf(stderr, "%s\n", error_message);

#undef _REQUIRED_SIZE
}

static void error_file(Alc_Error *error)
{
  const char *path = error->FILE.path;
#define _REQUIRED_SIZE (sizeof(error->FILE.path) + 64)

  char text[_REQUIRED_SIZE];
  snprintf(text, _REQUIRED_SIZE, "%s: Failed to open file", path);

  char error_message[_REQUIRED_SIZE];
  internal_message(error_message, _REQUIRED_SIZE, "error", text,
                   ANSI_GRAPHICS_BOLD | ANSI_COLOR_RED);

  fprintf(stderr, "%s\n", error_message);

#undef _REQUIRED_SIZE
}

static void error_lexer(Alc_Error *error)
{
  for (usize i = 0; i < error->LEXER.error_tokens_num; i++) {
    Alc_Token *error_token = &error->LEXER.error_tokens[i];

    char file_path[MAX_PATH_SIZE];
    alc_source_file_get_path(error->LEXER.sourcefile, file_path, MAX_PATH_SIZE);

    char message_text[512];
    snprintf(message_text, 512, "unrecognized token '%s%s%s'", ansi_graphics(ANSI_GRAPHICS_BOLD),
             error_token->value, ansi_reset());

    char message[512];
    file_message(message, 512, file_path, "error", message_text,
                 ANSI_GRAPHICS_BOLD | ANSI_COLOR_RED);

    char hl[4096];
    Highlight_Data data = {
      .src = error->LEXER.sourcefile->data,
      .tokens = error->LEXER.error_tokens,
      .tokens_len = error->LEXER.error_tokens_num,
    };
    highlight_token(hl, 4096, &data, i, ANSI_GRAPHICS_BOLD | ANSI_COLOR_RED, "");

    fprintf(stderr, "%s\n%s\n", message, hl);
  }
}

static void error_parser(Alc_Error *error)
{
  // FIXME: There's a ton of copypasta, need to shrink it down.

  for (usize i = 0; i < error->PARSER.parser_errors_num; i++) {
    Alc_Parser_Error *parser_error = &error->PARSER.parser_errors[i];

    char file_path[MAX_PATH_SIZE];
    alc_source_file_get_path(error->PARSER.sourcefile, file_path, MAX_PATH_SIZE);

    char message_text[512];

    switch (parser_error->type) {
    case ALC_PARSER_ERROR_TYPE_UNEXPECTED_EOF: {
      char message[512];
      file_message(message, 512, file_path, "error", "unexpected eof",
                   ANSI_GRAPHICS_BOLD | ANSI_COLOR_RED);

      char hl[4096];
      highlight_eof(hl, 4096, error->PARSER.sourcefile->data, ANSI_GRAPHICS_BOLD | ANSI_COLOR_RED,
                    "");

      fprintf(stderr, "%s\n%s\n", message, hl);
    } break;

    case ALC_PARSER_ERROR_TYPE_UNEXPECTED_TOKEN: {
      Alc_Token *token = &error->PARSER.tokens[parser_error->pos];
      snprintf(message_text, 512, "unexpected token '%s%s%s'", ansi_graphics(ANSI_GRAPHICS_BOLD),
               token_to_string(token), ansi_reset());

      char message[512];
      file_message(message, 512, file_path, "error", message_text,
                   ANSI_GRAPHICS_BOLD | ANSI_COLOR_RED);

      char hl[4096];
      Highlight_Data data = {
        .src = error->PARSER.sourcefile->data,
        .tokens = error->PARSER.tokens,
        .tokens_len = error->PARSER.tokens_num,
      };

      char expected[512];
      char *ep = expected;
      for (usize k = 512, j = 0; j < parser_error->UNEXPECTED_TOKEN.expected_token_types_num && k;
           j++) {
        usize written =
          snprintf(ep, k, j > 0 ? ", %s" : " %s",
                   token_type_to_string(parser_error->UNEXPECTED_TOKEN.expected_token_types[j]));
        ep += written;
        k -= written;
      }

      highlight_token_span(hl, 4096, &data, parser_error->pos, parser_error->len,
                           ANSI_GRAPHICS_BOLD | ANSI_COLOR_RED, expected);

      fprintf(stderr, "%s\n%s\n", message, hl);
    } break;

    case ALC_PARSER_ERROR_TYPE_UNEXPECTED_VALUE: {
      Alc_Token *token = &error->PARSER.tokens[parser_error->pos];
      snprintf(message_text, 512, "unexpected value '%s%s%s'", ansi_graphics(ANSI_GRAPHICS_BOLD),
               token->value, ansi_reset());

      char message[512];
      file_message(message, 512, file_path, "error", message_text,
                   ANSI_GRAPHICS_BOLD | ANSI_COLOR_RED);

      char hl[4096];
      Highlight_Data data = {
        .src = error->PARSER.sourcefile->data,
        .tokens = error->PARSER.tokens,
        .tokens_len = error->PARSER.tokens_num,
      };

      char expected[512];
      char *ep = expected;
      for (usize k = 512, j = 0; j < parser_error->UNEXPECTED_VALUE.expected_values_num && k; j++) {
        usize written = snprintf(ep, k, j > 0 ? ", %s" : " %s",
                                 parser_error->UNEXPECTED_VALUE.expected_values[j]);
        ep += written;
        k -= written;
      }

      highlight_token_span(hl, 4096, &data, parser_error->pos, parser_error->len,
                           ANSI_GRAPHICS_BOLD | ANSI_COLOR_RED, expected);

      fprintf(stderr, "%s\n%s\n", message, hl);
    } break;

    case ALC_PARSER_ERROR_TYPE_UNEXPECTED_WHITESPACE: {
      Alc_Token *token = &error->PARSER.tokens[parser_error->pos];

      const char *expected_token =
        token_type_to_string(parser_error->UNEXPECTED_WHITESPACE.expected_token_type);

      snprintf(message_text, 512,
               "unexpected whitespace after '%s%s%s', '%s%s%s' was expected after",
               ansi_graphics(ANSI_GRAPHICS_BOLD), token_to_string(token), ansi_reset(),
               ansi_graphics(ANSI_GRAPHICS_BOLD), expected_token, ansi_reset());

      char message[512];
      file_message(message, 512, file_path, "error", message_text,
                   ANSI_GRAPHICS_BOLD | ANSI_COLOR_RED);

      char hl[4096];
      Highlight_Data data = {
        .src = error->PARSER.sourcefile->data,
        .tokens = error->PARSER.tokens,
        .tokens_len = error->PARSER.tokens_num,
      };

      char expected[512];
      snprintf(expected, 512, " %s expected after", expected_token);

      highlight_token_span(hl, 4096, &data, parser_error->pos, parser_error->len,
                           ANSI_GRAPHICS_BOLD | ANSI_COLOR_RED, expected);

      fprintf(stderr, "%s\n%s\n", message, hl);
    } break;

    case ALC_PARSER_ERROR_TYPE_ASSIGN_OPERATOR_IN_NON_TOPLEVEL_EXPRESSION: {
      char operator[256], *op = operator;
      usize k = 256;
      for (usize j = 0; j < parser_error->len && k; j++) {
        usize written =
          snprintf(op, k, "%s", token_to_string(&error->PARSER.tokens[parser_error->pos + j]));
        op += written;
        k -= written;
      }

      snprintf(message_text, 512, "assign operator '%s%s%s' in non-toplevel expression",
               ansi_graphics(ANSI_GRAPHICS_BOLD), operator, ansi_reset());

      char message[512];
      file_message(message, 512, file_path, "error", message_text,
                   ANSI_GRAPHICS_BOLD | ANSI_COLOR_RED);

      char hl[4096];
      Highlight_Data data = {
        .src = error->PARSER.sourcefile->data,
        .tokens = error->PARSER.tokens,
        .tokens_len = error->PARSER.tokens_num,
      };

      highlight_token_span(hl, 4096, &data, parser_error->pos, parser_error->len,
                           ANSI_GRAPHICS_BOLD | ANSI_COLOR_RED, "");

      fprintf(stderr, "%s\n%s\n", message, hl);
    } break;

    case ALC_PARSER_ERROR_TYPE_TWO_ASSIGN_OPERATORS_IN_EXPRESSION: {
      snprintf(message_text, 512, "two or more assign operators in one expression");

      char message[512];
      file_message(message, 512, file_path, "error", message_text,
                   ANSI_GRAPHICS_BOLD | ANSI_COLOR_RED);

      char hl[4096];
      Highlight_Data data = {
        .src = error->PARSER.sourcefile->data,
        .tokens = error->PARSER.tokens,
        .tokens_len = error->PARSER.tokens_num,
      };

      highlight_token_span(hl, 4096, &data, parser_error->pos, parser_error->len,
                           ANSI_GRAPHICS_BOLD | ANSI_COLOR_RED, "");

      fprintf(stderr, "%s\n%s\n", message, hl);
    } break;
    }
  }
}

static void error_analysis(Alc_Error *error)
{
  switch (error->ANALYSIS.error_data.kind) {
  case ALC_ANALYSIS_ERROR_UNRESOLVABLE_IMPORT: {
    error_analysis_unresolvable_import(&error->ANALYSIS.error_data);
  } break;

  case ALC_ANALYSIS_ERROR_TYPE_REDEF: {
    error_analysis_type_redef(&error->ANALYSIS.error_data);
  } break;

  case ALC_ANALYSIS_ERROR_VARIABLE_REDECL: {
    error_analysis_variable_redecl(&error->ANALYSIS.error_data);
  } break;

  case ALC_ANALYSIS_ERROR_FUNCTION_REDEF: {
    error_analysis_function_redef(&error->ANALYSIS.error_data);
  } break;
  }
}

static void error_analysis_unresolvable_import(Alc_Analysis_Error *error)
{
  ALC_UNUSED_DEBUG(error);
  ALC_TODO("Handle UNRESOLVABLE_IMPORT error");
}

static void error_analysis_type_redef(Alc_Analysis_Error *error)
{
  char file_path[MAX_PATH_SIZE];
  alc_source_file_get_path(error->sourcefile, file_path, MAX_PATH_SIZE);

  char message_text[512];
  snprintf(message_text, 512, "redefinition of type '%s%s%s'", ansi_graphics(ANSI_GRAPHICS_BOLD),
           error->TYPE_REDEF.name, ansi_reset());

  char message[512];
  file_message(message, 512, file_path, "error", message_text, ANSI_GRAPHICS_BOLD | ANSI_COLOR_RED);

  char hl[4096];
  Highlight_Data hl_data = {
    .src = error->sourcefile->data,
    .tokens = error->sourcefile->tokens,
    .tokens_len = error->sourcefile->tokens_len,
  };
  highlight_token(hl, 4096, &hl_data, error->TYPE_REDEF.ast->pos,
                  ANSI_GRAPHICS_BOLD | ANSI_COLOR_RED, "");

  fprintf(stderr, "%s\n%s\n", message, hl);

  if (error->TYPE_REDEF.where_defined.sourcefile != nullptr) {
    Alc_Source_File *hint_source_file = error->TYPE_REDEF.where_defined.sourcefile;
    Alc_Ast *hint_ast = error->TYPE_REDEF.where_defined.ast;

    char hint_file_path[MAX_PATH_SIZE];
    alc_source_file_get_path(hint_source_file, hint_file_path, MAX_PATH_SIZE);

    char hint_message_text[512];
    snprintf(hint_message_text, 512, "type '%s%s%s' is already defined in this file",
             ansi_graphics(ANSI_GRAPHICS_BOLD), error->TYPE_REDEF.name, ansi_reset());

    char hint_message[512];
    file_message(hint_message, 512, hint_file_path, "note", hint_message_text,
                 ANSI_GRAPHICS_BOLD | ANSI_COLOR_CYAN);

    char hint_hl[4096];
    Highlight_Data hint_hl_data = {
      .src = hint_source_file->data,
      .tokens = hint_source_file->tokens,
      .tokens_len = hint_source_file->tokens_len,
    };
    highlight_token(hint_hl, 4096, &hint_hl_data, hint_ast->pos,
                    ANSI_GRAPHICS_BOLD | ANSI_COLOR_CYAN, "");

    fprintf(stderr, "%s\n%s\n", hint_message, hint_hl);
  }
}

static void error_analysis_variable_redecl(Alc_Analysis_Error *error)
{
  ALC_UNUSED_DEBUG(error);
  ALC_TODO("Handle VARIABLE_REDECL error");
}

static void error_analysis_function_redef(Alc_Analysis_Error *error)
{
  ALC_UNUSED_DEBUG(error);
  ALC_TODO("Handle FUNCTION_REDEF error");
}

static void internal_message(char *buf, usize n, const char *message, const char *text,
                             Ansi_Mode ansi_mode)
{
  snprintf(buf, n, "%s" ALC_COMPILER_NAME ": %s%s%s: %s%s", ansi_graphics(ANSI_GRAPHICS_BOLD),
           ansi_graphics(ansi_mode), ansi_color(ansi_mode), message, ansi_reset(), text);
}

static void file_message(char *buf, usize n, const char *file_path, const char *message,
                         const char *text, Ansi_Mode ansi_mode)
{
  snprintf(buf, n, "%s%s: %s%s%s: %s%s", ansi_graphics(ANSI_GRAPHICS_BOLD), file_path,
           ansi_color(ansi_mode), ansi_graphics(ansi_mode), message, ansi_reset(), text);
}

static void highlight_token(char *dst, usize n, Highlight_Data *data, usize index,
                            Ansi_Mode ansi_mode, const char *message)
{
  highlight_token_span(dst, n, data, index, 1, ansi_mode, message);
}

static void highlight_token_span(char *dst, usize n, Highlight_Data *data, usize index, usize len,
                                 Ansi_Mode ansi_mode, const char *message)
{
  ALC_ASSUME(index < data->tokens_len);
  ALC_ASSUME(len > 0);
  ALC_ASSUME(index + len - 1 < data->tokens_len);

  b8 continue_after = data->tokens[index].line != data->tokens[index + len - 1].line;

  Alc_Token *start_token = &data->tokens[index];
  Alc_Token *end_token;
  do
    end_token = &data->tokens[index + --len];
  while (start_token->line != end_token->line);

  usize line_num = start_token->line;
  const char *line_start = data->src;
  for (usize l = line_num; *line_start && l; line_start++)
    if (*line_start == '\n')
      l--;

  const char *line_end = line_start;
  for (; *line_end && *line_end != '\n'; line_end++)
    ;

  usize line_len = line_end - line_start;

  char line[2048], *lp = line;
  char mark[2048], *mp = mark;
  usize k = 2048;

  usize written = snprintf(lp, k, "  %zu | ", line_num + 1);
  k -= written;
  lp += written;
  memset(mp, ' ', sizeof(char) * (written - 2));
  mp[written - 2] = '|';
  mp[written - 1] = ' ';
  mp += written;

  usize span_start = start_token->pos;
  usize span_end = end_token->pos + end_token->len;
  usize span_len = span_end - span_start;

  // Pre-span copy
  usize pre_span_copy_len = ALC_MIN(k - 1, span_start);
  if ALC_LIKELY (pre_span_copy_len > 0) {
    memcpy(lp, line_start, sizeof(char) * pre_span_copy_len);
    memset(mp, ' ', sizeof(char) * pre_span_copy_len);

    lp += pre_span_copy_len;
    mp += pre_span_copy_len;
    k -= pre_span_copy_len;
  }

  // ANSI color and graphics mode
  const char *c = ansi_color(ansi_mode);
  const char *g = ansi_graphics(ansi_mode);
  for (; *c && k; k--, c++, lp++, mp++) {
    *lp = *c;
    *mp = *c;
  }
  for (; *g && k; k--, g++, lp++, mp++) {
    *lp = *g;
    *mp = *g;
  }

  // Span copy
  usize span_copy_len = ALC_MIN(k - 1, span_len);
  if ALC_LIKELY (span_copy_len > 0) {
    memcpy(lp, line_start + span_start, sizeof(char) * span_copy_len);
    memset(mp, '~', sizeof(char) * span_copy_len);
    *mp = '^';
    lp += span_copy_len;
    mp += span_copy_len;
    k -= span_copy_len;

    if ALC_UNLIKELY (continue_after) { // " ..." copy
      static const char c_str[] = " ...";
      usize c_len = ALC_MIN(k - 1, sizeof(c_str) - 1);

      memcpy(lp, c_str, c_len);
      memset(mp, '~', c_len);
      lp += c_len;
      mp += c_len;
      k -= c_len;
    }
  }

  usize mk = k;

  // Message copy
  usize message_copy_len = ALC_MIN(mk - 1, strlen(message));
  if ALC_LIKELY (message_copy_len > 0) {
    memcpy(mp, message, sizeof(char) * message_copy_len);
    mp += message_copy_len;
    mk -= message_copy_len;
  }

  // ANSI reset
  {
    const char *reset = ansi_reset();
    for (const char *r = reset; *r && k; k--, r++, lp++)
      *lp = *r;

    // Separate reset for mark, because it may be longer than the line buffer.
    for (const char *r = reset; *r && mk; mk--, r++, mp++)
      *mp = *r;
  }

  *mp = 0; // Mark ends here

  // Post-span copy
  usize post_span_copy_len = ALC_MIN(k - 1, line_len - span_end);
  if ALC_LIKELY (post_span_copy_len > 0) {
    memcpy(lp, line_start + line_len - (line_len - span_end), sizeof(char) * post_span_copy_len);
    lp += post_span_copy_len;
    k -= post_span_copy_len;
  }
  *lp = 0;

  snprintf(dst, n, "%s\n%s", line, mark);
}

static void highlight_eof(char *dst, usize n, const char *src, Ansi_Mode ansi_mode,
                          const char *message)
{
  usize line_num = 1;
  for (const char *s = src; *s; s++)
    if (*s == '\n')
      line_num++;

  const char *line_end = src + strlen(src);
  const char *line_start = line_end;
  usize line_length = (usize)(line_end - line_start);
  for (; line_start >= src && *line_start != '\n'; line_start--)
    ;
  if (*line_start == '\n')
    line_start++;

  char line[2048], *lp = line;
  char mark[2048], *mp = mark;
  usize k = 2048;

  usize written = snprintf(lp, k, "  %zu | ", line_num);
  memset(mp, ' ', sizeof(char) * written - 2);
  mp[written - 2] = '|';
  mp[written - 1] = ' ';
  lp += written;
  mp += written;
  k -= written;

  usize line_copy_len = ALC_MIN(k - 1, line_length);
  if (line_copy_len > 0) {
    memcpy(lp, line_start, sizeof(char) * line_copy_len);
    memset(mp, ' ', sizeof(char) * line_copy_len);
    lp += line_copy_len;
    mp += line_copy_len;
    k -= line_copy_len;
  }

  *lp = 0;

  for (const char *c = ansi_color(ansi_mode); *c && k; k--, c++, mp++)
    *mp = *c;
  for (const char *g = ansi_graphics(ansi_mode); *g && k; k--, g++, mp++)
    *mp = *g;

  const char *marker = " ^";
  for (; *marker && k; marker++, mp++, k--)
    *mp = *marker;

  usize message_copy_len = ALC_MIN(k - 1, strlen(message));
  if (message_copy_len > 0) {
    memcpy(mp, message, message_copy_len);
    mp += message_copy_len;
    k -= message_copy_len;
  }

  for (const char *r = ansi_reset(); *r && k; k--, r++, mp++)
    *mp = *r;

  *mp = 0;

  snprintf(dst, n, "%s\n%s", line, mark);
}

static const char *token_to_string(Alc_Token *token)
{
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
    return token->value;

  default:
    break;
  }

  return token_type_to_string(token->type);
}

static const char *token_type_to_string(Alc_Token_Type type)
{
  switch (type) {
#define ALC_TOKEN_TYPE_X(_name, _str_value) \
  case ALC_TOKEN_TYPE_FULL_NAME(_name):     \
    return _str_value;
    ALC_TOKEN_TYPES
#undef ALC_TOKEN_TYPE_X
  default:
    ALC_NOREACH();
  }
}
