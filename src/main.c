#include <alc/ast.h>
#include <alc/parser.h>
#include <alc/token.h>
#include <alc/lexer.h>
#include <stdio.h>
#include <alc/alc.h>
#include <stdlib.h>

enum {
  EXIT_FLAG_SUCCESS = 0,
  EXIT_FLAG_FAILED_TO_OPEN = (1 << 0),
  EXIT_FLAG_FAILED_TO_TOKENIZE = (1 << 1),
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

    Alc_Lexer lexer = alc_lexer_create(data);
    Alc_Token *tokens = nullptr;
    usize n_tokens;
    if ALC_UNLIKELY (!alc_lexer_tokenize(&lexer, &tokens, &n_tokens)) {
      ALC_TODO("Report tokenization failure.");
      result |= EXIT_FLAG_FAILED_TO_TOKENIZE;
      continue;
    }

    for (usize i = 0; i < n_tokens; i++) {
      char buf[1024] = { 0 };
      alc_token_to_string(&tokens[i], buf, 1024);
      printf("(%zu) %s\n", i, buf);
    }

    Alc_Parser *parser = alc_parser_create(tokens, n_tokens);
    Alc_Ast *root = alc_parser_parse(parser);
    alc_ast_print(root);

    alc_parser_destroy(parser);
  }

  alc_shutdown();
  return result;
}
