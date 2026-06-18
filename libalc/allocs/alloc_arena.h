#ifndef __ALC_ALLOC_ARENA_H__
#define __ALC_ALLOC_ARENA_H__

#include "alc/defs.h"
#include "containers/vector.h"

// NOTE: To debug arena, uncomment this define:
#define _DEBUG_ARENA_ALLOC

typedef struct {
  void *memory;
  uptr cursor;
  usize size;
} alloc_arena_block_t;

typedef struct {
  Vector(alloc_arena_block_t) blocks;
  usize blocks_num;
#ifdef _DEBUG_ARENA_ALLOC
  usize allocations;
#endif
} alloc_arena_t;

alloc_arena_t alloc_arena_create(void);
void alloc_arena_destroy(alloc_arena_t *alloc);

void *alloc_arena_allocate_aligned(alloc_arena_t *alloc, usize size, usize alignment);
static inline void *alloc_arena_allocate(alloc_arena_t *alloc, usize size)
{
  return alloc_arena_allocate_aligned(alloc, size, 16);
}

void alloc_arena_drop(alloc_arena_t *alloc);

#endif // __ALC_ALLOC_ARENA_H__
