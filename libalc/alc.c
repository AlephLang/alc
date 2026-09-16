#include "alc/alc.h"
#include "alc/defs.h"
#include "alc/alloc_arena.h"
#include "allocs/alloc_arena_debug.h"
#include "global.h"
#include "debug.h"

#ifdef __ALC_DEBUG_ARENA__
#ifdef __ALC_DEBUG_ARENA_DISPLAY_CONTENT__
#define _DISPLAY_CONTENT true
#else
#define _DISPLAY_CONTENT false
#endif
#endif

static Ctx _ctx = { 0 };
static b8 initialized = false;

b8 alc_initialize(void)
{
  ALC_ASSERT(!initialized);

  _ctx.arena = alc_alloc_arena_create();

  initialized = true;
  return true;
}

void alc_shutdown(void)
{
  ALC_ASSERT(initialized);

#ifdef __ALC_DEBUG_ARENA__
  alc_alloc_arena_debug_print(&_ctx.arena, _DISPLAY_CONTENT);
#endif

  alc_alloc_arena_destroy(&_ctx.arena);

  initialized = false;
}

Ctx *ctx(void)
{
  ALC_ASSERT(initialized);

  return &_ctx;
}
