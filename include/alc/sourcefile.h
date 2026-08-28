#ifndef __ALC_SOURCE_FILE_H__
#define __ALC_SOURCE_FILE_H__

#include <alc/filesystem.h>
#include <alc/defs.h>
#include <alc/ast.h>
#include <alc/token.h>
#include <alc/vector.h>

typedef struct __Alc_Module Alc_Module;

typedef struct {
  char *name;
  struct __Alc_Module *module;
  char *data;
  Alc_Token *tokens;
  usize tokens_len;
  Alc_Ast *root;
} Alc_Source_File;

ALC_API Alc_Source_File alc_source_file_create(struct __Alc_Module *module, const char *name,
                                               Alc_File *file);

ALC_API b8 alc_source_file_parse(Alc_Source_File *file);

ALC_API usize alc_source_file_get_path(Alc_Source_File *file, char *out, usize n);
ALC_API usize alc_source_file_get_absolute_path(Alc_Source_File *file, char *out, usize n);

#endif // __ALC_SOURCE_FILE_H__
