#include "alc/alc.h"
#include "alc/defs.h"
#include "allocs/alloc_arena.h"
#include "global.h"

static Ctx _ctx = { 0 };
static b8 initialized = false;

b8 alc_initialize(void)
{
  ALC_ASSERT(!initialized);

  _ctx.arena = alloc_arena_create();

  initialized = true;
  return true;
}

void alc_shutdown(void)
{
  ALC_ASSERT(initialized);

#ifdef _DEBUG_ARENA_ALLOC
  alloc_arena_print_blocks(&_ctx.arena, false);
#endif

  alloc_arena_destroy(&_ctx.arena);

  initialized = false;
}

Ctx *ctx(void)
{
  ALC_ASSERT(initialized);

  return &_ctx;
}
