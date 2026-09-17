#include "alc/module.h"
#include "alc/defs.h"
#include "alc/entry.h"
#include "alc/filesystem.h"
#include "alc/global_variable.h"
#include "alc/hashtable.h"
#include "alc/sourcefile.h"
#include "alc/program.h"
#include "alc/vector.h"
#include "alc/alloc_arena.h"
#include "global.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static Alc_Foreach_Result _submodule_destroy(usize index, void *value, void *user_data);
static Alc_Foreach_Result _submodule_parse(usize index, void *value, void *user_data);
static Alc_Foreach_Result _submodule_generate_entries(usize index, void *value, void *user_data);

Alc_Module *alc_module_create(Alc_Program *program, const char *name, Alc_Module *parent)
{
  ALC_ASSERT((name == nullptr && parent == nullptr) || (name != nullptr && parent != nullptr));

  usize name_len = name != nullptr ? strlen(name) + 1 : 0;

  Alc_Module *out = malloc(sizeof(Alc_Module));
  out->name = name != nullptr ?
                alc_alloc_arena_allocate_aligned(&ctx()->arena, sizeof(char) * name_len, 1) :
                nullptr;
  out->source_files = alc_vector_create(Alc_Source_File);
  out->submodules = alc_hashtable_create(sizeof(Alc_Module *), true);
  out->parent = parent;
  out->program = program;

  if ALC_LIKELY (out->name != nullptr)
    memcpy(out->name, name, sizeof(char) * name_len);

  return out;
}

void alc_module_destroy(Alc_Module *module)
{
  for (usize i = 0, source_files_len = alc_vector_get_length(module->source_files);
       i < source_files_len; i++)
    alc_source_file_destroy(&module->source_files[i]);
  alc_vector_destroy(module->source_files);

  alc_hashtable_foreach(&module->submodules, _submodule_destroy, nullptr);
  alc_hashtable_destroy(&module->submodules);

  free(module);
}

b8 alc_module_populate_tree(Alc_Module *module)
{
  char path[MAX_PATH_SIZE];
  usize written = alc_module_get_path(module, path, MAX_PATH_SIZE);

  // We assume that directory does exist
  Alc_Directory *module_dir = alc_filesystem_directory_open(path, false);
  if ALC_UNLIKELY (module_dir == nullptr) {
    Alc_Error e = {
      .kind = ALC_ERROR_KIND_DIRECTORY,
      .DIRECTORY.path =
        alc_alloc_arena_allocate_aligned(&ctx()->arena, sizeof(char) * MAX_PATH_SIZE, 1),
    };
    memcpy(e.DIRECTORY.path, path, sizeof(char) * MAX_PATH_SIZE);
    alc_program_add_error(module->program, e);
    return false;
  }

  char *p = &path[written];
  char *o = p;

  Alc_Directory_Content content = alc_filesystem_directory_get_content(module_dir);

  alc_filesystem_directory_close(module_dir);

  b8 result = true;

  for (usize i = 0, files_len = alc_vector_get_length(content.files); i < files_len; i++, p = o) {
    const char *file_name = content.files[i];
    for (; *file_name; file_name++, p++)
      *p = *file_name;
    *p = 0;

    char file_extension[MAX_PATH_NODE_SIZE];
    alc_filesystem_path_get_file_extension(content.files[i], file_extension);

    if ALC_UNLIKELY (strcmp(file_extension, ALEPH_SOURCE_FILE_EXTENSION) != 0)
      continue;

    Alc_File *file = alc_filesystem_file_open(path, ALC_FILE_READ_ONLY);
    if ALC_UNLIKELY (file == nullptr) {
      Alc_Error e = {
        .kind = ALC_ERROR_KIND_FILE,
        .FILE.path =
          alc_alloc_arena_allocate_aligned(&ctx()->arena, sizeof(char) * MAX_PATH_SIZE, 1),
      };
      memcpy(e.FILE.path, path, sizeof(char) * MAX_PATH_SIZE);
      alc_program_add_error(module->program, e);
      result = false;
      continue;
    }

    if ALC_UNLIKELY (alc_filesystem_file_is_empty(file)) {
      alc_filesystem_file_close(file);
      continue;
    }

    Alc_Source_File source_file = alc_source_file_create(module, content.files[i], file);
    alc_vector_push(module->source_files, source_file);

    alc_filesystem_file_close(file);
  }

  for (usize i = 0, dirs_len = alc_vector_get_length(content.directories); i < dirs_len;
       i++, p = o) {
    const char *dir_name = content.directories[i];

    for (; *dir_name; dir_name++, p++)
      *p = *dir_name;
    *p = 0;

    Alc_Directory *dir = alc_filesystem_directory_open(path, false);
    if ALC_UNLIKELY (dir == nullptr) {
      Alc_Error e = {
        .kind = ALC_ERROR_KIND_DIRECTORY,
        .DIRECTORY.path =
          alc_alloc_arena_allocate_aligned(&ctx()->arena, sizeof(char) * MAX_PATH_SIZE, 1),
      };
      memcpy(e.DIRECTORY.path, path, sizeof(char) * MAX_PATH_SIZE);
      alc_program_add_error(module->program, e);
      result = false;
      continue;
    }

    b8 is_empty = alc_filesystem_directory_is_empty(dir);
    alc_filesystem_directory_close(dir);

    if ALC_UNLIKELY (is_empty)
      continue;

    const char *submodule_name = content.directories[i];
    Alc_Module *submodule = alc_module_create(module->program, submodule_name, module);
    alc_module_populate_tree(submodule);

    if ALC_UNLIKELY (alc_module_is_empty(submodule)) {
      alc_module_destroy(submodule);
      continue;
    }

    ALC_DEBUG_ASSUME(alc_hashtable_get(&module->submodules, submodule_name) == nullptr);
    alc_hashtable_put(&module->submodules, submodule_name, submodule);
  }

  alc_directory_content_destroy(&content);

  return result;
}

b8 alc_module_parse_tree(Alc_Module *module)
{
  b8 result = true;
  for (usize i = 0, source_files_len = alc_vector_get_length(module->source_files);
       i < source_files_len; i++)
    result = result && alc_source_file_parse(&module->source_files[i]);

  alc_hashtable_foreach(&module->submodules, _submodule_parse, &result);

  return result;
}

void alc_module_generate_entries(Alc_Module *module)
{
  for (usize i = 0, source_files_len = alc_vector_get_length(module->source_files);
       i < source_files_len; i++)
    alc_source_file_generate_entries(&module->source_files[i]);

  alc_hashtable_foreach(&module->submodules, _submodule_generate_entries, nullptr);
}

usize alc_module_get_path(Alc_Module *module, char *out, usize n)
{
  if ALC_UNLIKELY (module->parent == nullptr) {
    if (module->program->path[0] == '/' && strlen(module->program->path) == 1) {
      *out++ = '/';
      *out = 0;
      return 1;
    }
    return snprintf(out, n, "%s/", module->program->path);
  }

  usize written = alc_module_get_path(module->parent, out, n);
  n -= written;
  out += written;

  return written + snprintf(out, n, "%s/", module->name);
}

usize alc_module_get_absolute_path(Alc_Module *module, char *out, usize n)
{
  if ALC_UNLIKELY (module->parent == nullptr) {
    if (module->program->absolute_path[0] == '/' && strlen(module->program->absolute_path) == 1) {
      *out++ = '/';
      *out = 0;
      return 1;
    }
    return snprintf(out, n, "%s/", module->program->absolute_path);
  }

  usize written = alc_module_get_path(module->parent, out, n);
  n -= written;
  out += written;

  return written + snprintf(out, n, "%s/", module->name);
}

b8 alc_module_is_empty(Alc_Module *module)
{
  return alc_vector_get_length(module->source_files) == 0 &&
         alc_hashtable_is_empty(&module->submodules);
}

Alc_Type *alc_module_find_type(Alc_Module *module, const char *name)
{
  while (module != nullptr) {
    for (usize i = 0, source_files_len = alc_vector_get_length(module->source_files);
         i < source_files_len; i++) {
      Alc_Source_File *source_file = &module->source_files[i];

      Alc_Type *type = alc_source_file_find_type(source_file, name);
      if (type != nullptr &&
          (type->scope == ALC_ENTRY_SCOPE_GLOBAL || type->scope == ALC_ENTRY_SCOPE_LOCAL_MODULE))
        return type;
    }

    module = module->parent;
  }

  return nullptr;
}

Alc_Global_Variable *alc_module_find_global(Alc_Module *module, const char *name)
{
  while (module != nullptr) {
    for (usize i = 0, source_files_len = alc_vector_get_length(module->source_files);
         i < source_files_len; i++) {
      Alc_Source_File *source_file = &module->source_files[i];

      Alc_Global_Variable *gvar = alc_source_file_find_global(source_file, name);
      if (gvar != nullptr &&
          (gvar->scope == ALC_ENTRY_SCOPE_GLOBAL || gvar->scope == ALC_ENTRY_SCOPE_LOCAL_MODULE))
        return gvar;
    }

    module = module->parent;
  }

  return nullptr;
}

usize alc_module_to_namespace_string(char *buf, usize n, const Alc_Module *module,
                                     const Alc_Module *relative_module)
{
  // FIXME: Use heap stack instead of recursion

  if (module == relative_module)
    return 0;

  if (module->parent != nullptr) {
    usize written = alc_module_to_namespace_string(buf, n, module->parent, relative_module);
    n -= written;
    buf += written;
  }

  return snprintf(buf, n, "%s::", module->name);
}

static Alc_Foreach_Result _submodule_destroy(usize index, void *value, void *user_data)
{
  ALC_UNUSED_PERMIT(index);
  ALC_UNUSED_PERMIT(user_data);

  Alc_Module *submodule = value;

  alc_module_destroy(submodule);

  return ALC_FOREACH_CONTINUE;
}

static Alc_Foreach_Result _submodule_parse(usize index, void *value, void *user_data)
{
  ALC_UNUSED_PERMIT(index);

  Alc_Module *submodule = value;
  b8 *result = user_data;

  *result = *result && alc_module_parse_tree(submodule);

  return ALC_FOREACH_CONTINUE;
}

static Alc_Foreach_Result _submodule_generate_entries(usize index, void *value, void *user_data)
{
  ALC_UNUSED_PERMIT(index);
  ALC_UNUSED_PERMIT(user_data);

  Alc_Module *submodule = value;

  alc_module_generate_entries(submodule);

  return ALC_FOREACH_CONTINUE;
}
