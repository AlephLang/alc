#include "alc/program.h"
#include "alc/defs.h"
#include "alc/filesystem.h"
#include "alc/module.h"
#include "alc/vector.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include <string.h>

Alc_Program *alc_program_create(const char *path)
{
  Alc_Program *program = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Program));
  alc_filesystem_path_absolute(path, program->absolute_path);

  if (path != nullptr)
    alc_filesystem_path_simplify(path, program->path);

  program->errors = alc_vector_create(Alc_Error);

  program->root_module = alc_module_create(program, nullptr, nullptr);

  return program;
}

void alc_program_destroy(Alc_Program *program)
{
  alc_module_destroy(program->root_module);
  alc_vector_destroy(program->errors);

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

void alc_program_add_error_impl(Alc_Program *program, Alc_Error error)
{
  alc_vector_push(program->errors, error);
}

Alc_Vector(Alc_Error) alc_program_get_errors(Alc_Program *program)
{
  return program->errors;
}
