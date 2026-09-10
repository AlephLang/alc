#include "alc/program.h"
#include "alc/analyzer.h"
#include "alc/defs.h"
#include "alc/filesystem.h"
#include "alc/module.h"
#include "alc/type.h"
#include "alc/vector.h"
#include "alc/alloc_arena.h"
#include "global.h"
#include <string.h>

Alc_Program *alc_program_create(const char *path)
{
  Alc_Program *program = alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Program));
  alc_filesystem_path_absolute(path, program->absolute_path);

  if (path != nullptr)
    alc_filesystem_path_simplify(path, program->path);

  program->errors = alc_vector_create(Alc_Error);

  program->root_module = alc_module_create(program, nullptr, nullptr);

  program->type_storage = alc_type_storage_create(1024);

  return program;
}

void alc_program_destroy(Alc_Program *program)
{
  alc_type_storage_destroy(&program->type_storage);

  alc_module_destroy(program->root_module);
  alc_vector_destroy(program->errors);

  if ALC_UNLIKELY (program->entries_import != nullptr)
    alc_vector_destroy(program->entries_import);
  if ALC_UNLIKELY (program->entries_type != nullptr)
    alc_vector_destroy(program->entries_type);
  if ALC_UNLIKELY (program->entries_global != nullptr)
    alc_vector_destroy(program->entries_global);
  if ALC_UNLIKELY (program->entries_function != nullptr)
    alc_vector_destroy(program->entries_function);

  memset(program, 0, sizeof(Alc_Program));
}

b8 alc_program_build_module_tree(Alc_Program *program)
{
  return alc_module_populate_tree(program->root_module);
}

b8 alc_program_parse_modules(Alc_Program *program)
{
  return alc_module_parse_tree(program->root_module);
}

void alc_program_generate_entries(Alc_Program *program)
{
  if ALC_UNLIKELY (program->entries_import != nullptr)
    alc_vector_destroy(program->entries_import);
  if ALC_UNLIKELY (program->entries_type != nullptr)
    alc_vector_destroy(program->entries_type);
  if ALC_UNLIKELY (program->entries_global != nullptr)
    alc_vector_destroy(program->entries_global);
  if ALC_UNLIKELY (program->entries_function != nullptr)
    alc_vector_destroy(program->entries_function);

  program->entries_import = alc_vector_create(Alc_Entry);
  program->entries_type = alc_vector_create(Alc_Entry);
  program->entries_global = alc_vector_create(Alc_Entry);
  program->entries_function = alc_vector_create(Alc_Entry);

  alc_module_generate_entries(program->root_module);
}

b8 alc_program_analyze(Alc_Program *program)
{
  b8 result = true;

  // TODO: Validate imports
  // result = result &&
  //          alc_analyzer_validate_and_emplace_import_entries(program, program->entries_import);
  result = result && alc_analyzer_validate_and_emplace_type_entries(program, program->entries_type);
  result = result &&
           alc_analyzer_validate_and_emplace_global_entries(program, program->entries_global);
  result = result &&
           alc_analyzer_validate_and_emplace_function_entries(program, program->entries_function);

  return result;
}

void alc_program_add_error_impl(Alc_Program *program, Alc_Error error)
{
  alc_vector_push(program->errors, error);
}

Alc_Vector(Alc_Error) alc_program_get_errors(Alc_Program *program)
{
  return program->errors;
}
