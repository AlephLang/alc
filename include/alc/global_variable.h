#ifndef __ALC_GLOBAL_VARIABLE_H__
#define __ALC_GLOBAL_VARIABLE_H__

#include <alc/defs.h>
#include <alc/type.h>
#include <alc/entry.h>
#include <alc/variable.h>

typedef struct {
  Alc_Variable *var_data;
  Alc_Entry_Scope scope;
} Alc_Global_Variable;

ALC_API Alc_Global_Variable alc_global_variable_create(Alc_Variable *var, Alc_Entry_Scope scope);
ALC_API void alc_global_variable_destroy(Alc_Global_Variable *gvar);

#endif // __ALC_GLOBAL_VARIABLE_H__
