#include "alc/variable.h"
#include <stdlib.h>
#include <string.h>

Alc_Variable *alc_variable_create(const char *name, Alc_Type *type)
{
  usize name_len = strlen(name) + 1;
  Alc_Variable *var = malloc(sizeof(Alc_Variable) + (sizeof(char) * name_len));
  var->name = (char *)var + sizeof(Alc_Variable);
  var->type = type;
  var->is_extern = false;
  memcpy(var->name, name, sizeof(char) * name_len);
  return var;
}

void alc_variable_destroy(Alc_Variable *var)
{
  free(var);
}

Alc_Variable *alc_variable_extern_create(const char *name, Alc_Type *type)
{
  Alc_Variable *var = alc_variable_create(name, type);
  var->is_extern = true;
  return var;
}
