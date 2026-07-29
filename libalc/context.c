#include "alc/context.h"
#include "alc/defs.h"
#include "alc/scope.h"
#include <string.h>

Alc_Context alc_context_create(void)
{
  return (Alc_Context){
    .global_scope = alc_scope_create(ALC_SCOPE_KIND_GLOBAL, nullptr, nullptr, nullptr),
  };
}

void alc_context_destroy(Alc_Context *context)
{
  alc_scope_destroy(&context->global_scope);

  memset(context, 0, sizeof(Alc_Context));
}
