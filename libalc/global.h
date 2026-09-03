#ifndef __ALC_GLOBAL_H__
#define __ALC_GLOBAL_H__

#include "alc/alloc_arena.h"

typedef struct {
  Alc_Alloc_Arena arena;
} Ctx;

Ctx *ctx(void);

#endif // __ALC_GLOBAL_H__
