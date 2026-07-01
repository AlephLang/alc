#ifndef __ALC_ALLOC_ARENA_H__
#define __ALC_ALLOC_ARENA_H__

#include "alc/defs.h"
#include "alc/vector.h"

// NOTE: To debug arena, uncomment this define:
// #define _DEBUG_ARENA_ALLOC

typedef struct {
  void *memory;
  uptr cursor;
  usize size;
} Alloc_Arena_Block;

typedef struct {
  Alc_Vector(Alloc_Arena_Block) blocks;
  usize blocks_num;
#ifdef _DEBUG_ARENA_ALLOC
  usize allocations;
#endif
} Alloc_Arena;

Alloc_Arena alloc_arena_create(void);
void alloc_arena_destroy(Alloc_Arena *alloc);

void *alloc_arena_allocate_aligned(Alloc_Arena *alloc, usize size, usize alignment);
static inline void *alloc_arena_allocate(Alloc_Arena *alloc, usize size)
{
  return alloc_arena_allocate_aligned(alloc, size, 16);
}

void alloc_arena_drop(Alloc_Arena *alloc);

#endif // __ALC_ALLOC_ARENA_H__
