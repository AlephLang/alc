#ifndef __ERROR_HANDLER_H__
#define __ERROR_HANDLER_H__

#include <alc/parser.h>
#include <alc/defs.h>
#include <alc/token.h>

typedef struct {
  const char *start_ptr;
  usize length;
} Source_Line;

typedef struct {
  const char *filename;
  Source_Line *source_lines;
  usize source_lines_num;
  Alc_Token *tokens;
  usize tokens_num;
  usize lnoffset;
} Error_Handler;

Error_Handler error_handler_create(const char *filename, const char *src);
void error_handler_destroy(Error_Handler *handler);

void error_handler_set_tokens(Error_Handler *handler, Alc_Token *tokens, usize tokens_num);

void error_handler_handle_lexer_errors(Error_Handler *handler, Alc_Token *invalid_tokens,
                                       usize invalid_tokens_num);
void error_handler_handle_parser_errors(Error_Handler *handler, Alc_Parser_Error *errors,
                                        usize errors_num);

#endif // __ERROR_HANDLER_H__
