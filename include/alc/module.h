#ifndef __ALC_MODULE_H__
#define __ALC_MODULE_H__

#include "alc/global_variable.h"
#include <alc/entry.h>
#include <alc/type.h>
#include <alc/hashtable.h>
#include <alc/sourcefile.h>
#include <alc/vector.h>

typedef struct __Alc_Program Alc_Program;

typedef struct __Alc_Module {
  char *name;
  Alc_Vector(Alc_Source_File) source_files;
  Alc_Hashtable(Alc_Module *) submodules;
  struct __Alc_Module *parent;

  Alc_Program *program;
} Alc_Module;

ALC_API Alc_Module *alc_module_create(Alc_Program *program, const char *name, Alc_Module *parent);
ALC_API void alc_module_destroy(Alc_Module *module);

ALC_API b8 alc_module_populate_tree(Alc_Module *module);
ALC_API b8 alc_module_parse_tree(Alc_Module *module);

ALC_API void alc_module_generate_entries(Alc_Module *module);

ALC_API usize alc_module_get_path(Alc_Module *module, char *out, usize n);
ALC_API usize alc_module_get_absolute_path(Alc_Module *module, char *out, usize n);

ALC_API b8 alc_module_is_empty(Alc_Module *module);

ALC_API Alc_Type *alc_module_find_type(Alc_Module *module, const char *name);
ALC_API Alc_Global_Variable *alc_module_find_global(Alc_Module *module, const char *name);

ALC_API usize alc_module_to_namespace_string(char *buf, usize n, const Alc_Module *module,
                                             const Alc_Module *relative_module);

#endif // __ALC_MODULE_H__
