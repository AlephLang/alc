#ifndef __ALC_ENTRY_H__
#define __ALC_ENTRY_H__

#include <alc/sourcefile.h>
#include <alc/ast.h>

typedef enum {
  ALC_ENTRY_SCOPE_GLOBAL,
  ALC_ENTRY_SCOPE_LOCAL_MODULE,
  ALC_ENTRY_SCOPE_LOCAL_FILE,
} Alc_Entry_Scope;

typedef struct {
  Alc_Source_File *file;
  Alc_Ast *ast;
  Alc_Entry_Scope scope;
} Alc_Entry;

#endif // __ALC_ENTRY_H__
