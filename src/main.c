#include <alc/filesystem.h>
#include <alc/program.h>
#include <alc/vector.h>
#include <alc/defs.h>
#include <alc/ast.h>
#include <alc/parser.h>
#include <alc/token.h>
#include <alc/lexer.h>
#include <assert.h>
#include <stdio.h>
#include <alc/alc.h>
#include <alc/sourcefile.h>
#include <alc/module.h>
#include "error_handler.h"
#include <string.h>

s32 main(s32 argc, char **argv)
{
  if ALC_UNLIKELY (!alc_initialize()) {
    fprintf(stderr, "Failed to initialize ALC.\n");
    return -1;
  }

  char *path = nullptr;
  if (argc >= 2) {
    path = argv[1]; // TODO: make it more robust

    if ALC_UNLIKELY (!alc_filesystem_directory_exists(path)) {
      Alc_Error e = { .kind = ALC_ERROR_KIND_DIRECTORY };
      e.DIRECTORY.path = path;
      handle_error(&e);
      alc_shutdown();
      return -2;
    }
  }

  Alc_Program *program_context = alc_program_create(path);

  printf("program relative path: %s\n", program_context->path);
  printf("program absolute path: %s\n", program_context->absolute_path);

  if ALC_UNLIKELY (!alc_program_build_module_tree(program_context)) {
    Alc_Vector(Alc_Error) module_errors = alc_program_get_errors(program_context);
    for (usize i = 0, module_errors_len = alc_vector_get_length(module_errors);
         i < module_errors_len; i++)
      handle_error(&module_errors[i]);

    alc_program_destroy(program_context);
    alc_shutdown();
    return -3;
  }

  if ALC_UNLIKELY (!alc_program_parse_modules(program_context)) {
    Alc_Vector(Alc_Error) parse_errors = alc_program_get_errors(program_context);
    for (usize i = 0, parse_errors_len = alc_vector_get_length(parse_errors); i < parse_errors_len;
         i++)
      handle_error(&parse_errors[i]);

    alc_program_destroy(program_context);
    alc_shutdown();
    return -4;
  }

  alc_program_generate_entries(program_context);

  if ALC_UNLIKELY (!alc_program_analyze(program_context)) {
    Alc_Vector(Alc_Error) analysis_errors = alc_program_get_errors(program_context);
    for (usize i = 0, analysis_errors_len = alc_vector_get_length(analysis_errors);
         i < analysis_errors_len; i++)
      handle_error(&analysis_errors[i]);

    alc_program_destroy(program_context);
    alc_shutdown();
    return -5;
  }

  alc_program_destroy(program_context);

  alc_shutdown();
  return 0;
}
