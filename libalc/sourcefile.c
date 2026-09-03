#include "alc/sourcefile.h"
#include "alc/filesystem.h"
#include "alc/program.h"
#include "alc/module.h"
#include "alc/defs.h"
#include "alc/lexer.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "alc/vector.h"
#include "alc/alloc_arena.h"
#include "global.h"
#include "debug.h"
#include <stdio.h>
#include <string.h>

#ifdef _DEBUG

#ifdef __ALC_DEBUG_TOKENS__
#define _DEBUG_TOKENS
#endif

#ifdef __ALC_DEBUG_ASTS__
#define _DEBUG_ASTS
#endif

#endif

#if defined(_DEBUG_TOKENS) || defined(_DEBUG_ASTS)
#define _DEBUG_FILE
#endif

Alc_Source_File alc_source_file_create(struct __Alc_Module *module, const char *name,
                                       Alc_File *file)
{
  usize name_len = strlen(name) + 1;

  usize file_size = alc_filesystem_file_get_size(file) + 1;

  Alc_Source_File out = {
    .name = alc_alloc_arena_allocate_aligned(&ctx()->arena, sizeof(char) * name_len, 1),
    .module = module,
    .data = alc_alloc_arena_allocate_aligned(&ctx()->arena, file_size, 1),
    .tokens = nullptr,
    .tokens_len = 0,
    .root = nullptr,
  };
  memcpy(out.name, name, sizeof(char) * name_len);
  alc_filesystem_file_read(file, out.data, file_size);

  return out;
}

void alc_source_file_destroy(Alc_Source_File *file)
{
  memset(file, 0, sizeof(Alc_Source_File));
}

b8 alc_source_file_parse(Alc_Source_File *file)
{
#ifdef _DEBUG_FILE
  {
    char file_path[MAX_PATH_SIZE];
    alc_source_file_get_path(file, file_path, MAX_PATH_SIZE);
    printf("(%s) '%s' source file debug data:\n", file_path, file->name);
  }
#endif

  Alc_Lexer lexer = alc_lexer_create(file->data);
  if ALC_UNLIKELY (!alc_lexer_tokenize(&lexer, &file->tokens, &file->tokens_len)) {
    alc_program_add_error(file->module->program, {
                                                   .LEXER.sourcefile = file,
                                                   .LEXER.error_tokens = file->tokens,
                                                   .LEXER.error_tokens_num = file->tokens_len,
                                                   .kind = ALC_ERROR_KIND_LEXER,
                                                 });
    return false;
  }

#ifdef _DEBUG_TOKENS
  printf(">>> Tokens:\n");
  for (usize i = 0; i < file->tokens_len; i++) {
    char buf[1024] = { 0 };
    alc_token_to_string(&file->tokens[i], buf, 1024);
    printf("(%zu) %s\n", i, buf);
  }
#endif

  if ALC_UNLIKELY (file->tokens_len == 0)
    return true;

  Alc_Parser *parser = alc_parser_create(file->tokens, file->tokens_len);
  Alc_Ast *root = alc_parser_parse(parser);
#ifdef _DEBUG_ASTS
  printf(">>> AST:\n");
  alc_ast_print(root);
#endif

  Alc_Vector(Alc_Parser_Error) parser_errors_v = alc_parser_get_errors(parser);
  if ALC_UNLIKELY (alc_vector_get_length(parser_errors_v) > 0) {
    usize parser_errors_num;
    Alc_Parser_Error *parser_errors = alc_vector_to_array(parser_errors_v, &parser_errors_num);
    alc_program_add_error(file->module->program, {
                                                   .PARSER.sourcefile = file,
                                                   .PARSER.parser_errors = parser_errors,
                                                   .PARSER.parser_errors_num = parser_errors_num,
                                                   .PARSER.tokens = file->tokens,
                                                   .PARSER.tokens_num = file->tokens_len,
                                                   .kind = ALC_ERROR_KIND_PARSER,
                                                 });

    alc_parser_destroy(parser);
    return false;
  }

  file->root = root;

  alc_parser_destroy(parser);

  return true;
}

usize alc_source_file_get_path(Alc_Source_File *file, char *out, usize n)
{
  usize written = alc_module_get_path(file->module, out, n);
  n -= written;
  out += written;
  return written + snprintf(out, n, "%s", file->name);
}

usize alc_source_file_get_absolute_path(Alc_Source_File *file, char *out, usize n)
{
  usize written = alc_module_get_absolute_path(file->module, out, n);
  n -= written;
  out += written;
  return written + snprintf(out, n, "%s", file->name);
}
