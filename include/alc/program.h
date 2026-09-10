#ifndef __ALC_PROGRAM_H__
#define __ALC_PROGRAM_H__

#include <alc/analyzer.h>
#include <alc/type.h>
#include <alc/parser.h>
#include <alc/vector.h>
#include <alc/token.h>
#include <alc/defs.h>
#include <alc/module.h>
#include <alc/sourcefile.h>
#include <alc/entry.h>

typedef struct {
  union {
    struct {
      char *path;
    } DIRECTORY;
    struct {
      char *path;
    } FILE;
    struct {
      Alc_Source_File *sourcefile;
      Alc_Token *error_tokens;
      usize error_tokens_num;
    } LEXER;
    struct {
      Alc_Source_File *sourcefile;
      Alc_Parser_Error *parser_errors;
      usize parser_errors_num;
      Alc_Token *tokens;
      usize tokens_num;
    } PARSER;
    struct {
      Alc_Analysis_Error error_data;
    } ANALYSIS;
  };
  enum {
    ALC_ERROR_KIND_DIRECTORY,
    ALC_ERROR_KIND_FILE,
    ALC_ERROR_KIND_LEXER,
    ALC_ERROR_KIND_PARSER,
    ALC_ERROR_KIND_ANALYSIS,
  } kind;
} Alc_Error;

typedef struct __Alc_Program {
  char path[MAX_PATH_SIZE];
  char absolute_path[MAX_PATH_SIZE];

  Alc_Type_Storage type_storage;

  Alc_Module *root_module;

  Alc_Vector(Alc_Error) errors;

  Alc_Vector(Alc_Entry) entries_import;
  Alc_Vector(Alc_Entry) entries_type;
  Alc_Vector(Alc_Entry) entries_global;
  Alc_Vector(Alc_Entry) entries_function;
} Alc_Program;

ALC_API Alc_Program *alc_program_create(const char *path);
ALC_API void alc_program_destroy(Alc_Program *program);

ALC_API b8 alc_program_build_module_tree(Alc_Program *program);
ALC_API b8 alc_program_parse_modules(Alc_Program *program);

ALC_API void alc_program_generate_entries(Alc_Program *program);

ALC_API b8 alc_program_analyze(Alc_Program *program);

#define alc_program_add_error(_program, ...) \
  alc_program_add_error_impl((_program), (Alc_Error)__VA_ARGS__)
ALC_API void alc_program_add_error_impl(Alc_Program *program, Alc_Error error);

ALC_API Alc_Vector(Alc_Error) alc_program_get_errors(Alc_Program *program);

#endif // __ALC_PROGRAM_H__
