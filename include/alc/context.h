#ifndef __ALC_CONTEXT_H__
#define __ALC_CONTEXT_H__

#include <alc/defs.h>
#include <alc/scope.h>

typedef struct {
  Alc_Scope global_scope;
} Alc_Context;

ALC_API Alc_Context alc_context_create(void);
ALC_API void alc_context_destroy(Alc_Context *context);

#endif // __ALC_CONTEXT_H__
