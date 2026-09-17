#include "alc/global_variable.h"
#include "alc/variable.h"
#include <string.h>

Alc_Global_Variable alc_global_variable_create(Alc_Variable *var, Alc_Entry_Scope scope)
{
  return (Alc_Global_Variable){
    .var_data = var,
    .scope = scope,
  };
}

void alc_global_variable_destroy(Alc_Global_Variable *gvar)
{
  alc_variable_destroy(gvar->var_data);

  memset(gvar, 0, sizeof(Alc_Global_Variable));
}
