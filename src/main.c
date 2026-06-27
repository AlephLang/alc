#include <alc/defs.h>
#include <alc/ast.h>
#include <alc/parser.h>
#include <alc/token.h>
#include <alc/lexer.h>
#include <stdio.h>
#include <alc/alc.h>
#include <stdlib.h>
#include "error_handler.h"

enum {
  EXIT_FLAG_SUCCESS = 0,
  EXIT_FLAG_FAILED_TO_OPEN = (1 << 0),
  EXIT_FLAG_FAILED_TO_TOKENIZE = (1 << 1),
  EXIT_FLAG_FAILED_TO_PARSE = (1 << 2),
};

s32 main(s32 argc, char **argv)
{
  if (!alc_initialize()) {
    fprintf(stderr, "Failed to initialize ALC.\n");
    return -1;
  }

  if (argc < 2) {
    ALC_TODO("Print usage");
    return -2;
  }

  s32 result = 0;

  for (s32 i = 1; i < argc; i++) {
    const char *file_name = argv[i];
    FILE *f = fopen(file_name, "r");
    if ALC_UNLIKELY (f == nullptr) {
      ALC_TODO("Report failed to open file.");
      result |= EXIT_FLAG_FAILED_TO_OPEN;
      continue;
    }

    fseek(f, 0, SEEK_END);
    s32 size = ftell(f);
    fseek(f, 0, SEEK_SET);
    if (size == 0)
      continue;

    char *data = malloc(sizeof(char) * (size + 1));
    fread(data, sizeof(char), size, f);
    data[size] = 0;
    fclose(f);

    Error_Handler error_handler = error_handler_create(file_name, data);

    Alc_Lexer lexer = alc_lexer_create(data);
    Alc_Token *tokens = nullptr;
    usize n_tokens;
    if ALC_UNLIKELY (!alc_lexer_tokenize(&lexer, &tokens, &n_tokens)) {
      error_handler_handle_lexer_errors(&error_handler, tokens, n_tokens);
      result |= EXIT_FLAG_FAILED_TO_TOKENIZE;
      continue;
    }

#ifdef _DEBUG
    for (usize i = 0; i < n_tokens; i++) {
      char buf[1024] = { 0 };
      alc_token_to_string(&tokens[i], buf, 1024);
      printf("(%zu) %s\n", i, buf);
    }
#endif

    error_handler_set_tokens(&error_handler, tokens, n_tokens);

    Alc_Parser *parser = alc_parser_create(tokens, n_tokens);
    Alc_Ast *root = alc_parser_parse(parser);
#ifdef _DEBUG
    alc_ast_print(root);
#endif

    usize parser_errors_num;
    Alc_Parser_Error *parser_errors = alc_parser_get_errors(parser, &parser_errors_num);
    if ALC_UNLIKELY (parser_errors_num > 0) {
      error_handler_handle_parser_errors(&error_handler, parser_errors, parser_errors_num);

      result |= EXIT_FLAG_FAILED_TO_PARSE;
      alc_parser_destroy(parser);
      continue;
    }

    alc_parser_destroy(parser);

    error_handler_destroy(&error_handler);

    free(data);
  }

  alc_shutdown();
  return result;
}
