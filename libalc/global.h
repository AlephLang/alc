#ifndef __ALC_GLOBAL_H__
#define __ALC_GLOBAL_H__

#include "allocs/alloc_arena.h"

typedef struct {
  alloc_arena_t arena;
} ctx_t;

ctx_t *ctx(void);

#endif // __ALC_GLOBAL_H__
