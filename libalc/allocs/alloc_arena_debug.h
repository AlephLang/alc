#ifndef __ALC_ALLOC_ARENA_DEBUG_H__
#define __ALC_ALLOC_ARENA_DEBUG_H__

#include <alc/alloc_arena.h>
#include "debug.h"

#ifdef __ALC_DEBUG_ARENA__
void alc_alloc_arena_debug_print(Alc_Alloc_Arena *alloc, b8 show_content);
#else
#define alc_alloc_arena_debug_print(...)
#endif

#endif // __ALC_ALLOC_ARENA_DEBUG_H__
