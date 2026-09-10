#ifndef __ALC_ENTRY_H__
#define __ALC_ENTRY_H__

#include <alc/ast.h>

typedef struct __Alc_Source_File Alc_Source_File;
typedef struct __Alc_Type Alc_Type;

typedef enum {
  ALC_ENTRY_SCOPE_GLOBAL,
  ALC_ENTRY_SCOPE_LOCAL_MODULE,
  ALC_ENTRY_SCOPE_LOCAL_FILE,
} Alc_Entry_Scope;

typedef struct {
  Alc_Source_File *file;
  Alc_Ast *ast;
  void *data;
  Alc_Entry_Scope scope;
} Alc_Entry;

#endif // __ALC_ENTRY_H__
