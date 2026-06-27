#ifndef __ALC_GLOBAL_H__
#define __ALC_GLOBAL_H__

#include "allocs/alloc_arena.h"

typedef struct {
  Alloc_Arena arena;
} Ctx;

Ctx *ctx(void);

#endif // __ALC_GLOBAL_H__
