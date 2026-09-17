#ifndef __ALC_VARIABLE_H__
#define __ALC_VARIABLE_H__

#include <alc/entry.h>
#include <alc/defs.h>
#include <alc/type.h>

typedef struct __Alc_Variable {
  char *name;
  Alc_Type *type;
  b8 is_extern;
} Alc_Variable;

ALC_API Alc_Variable *alc_variable_create(const char *name, Alc_Type *type);
ALC_API void alc_variable_destroy(Alc_Variable *var);

ALC_API Alc_Variable *alc_variable_extern_create(const char *name, Alc_Type *type);

#endif // __ALC_VARIABLE_H__
