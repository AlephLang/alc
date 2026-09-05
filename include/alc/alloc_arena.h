#ifndef __ALC_ALLOC_ARENA_H__
#define __ALC_ALLOC_ARENA_H__

#include <alc/defs.h>
#include <alc/vector.h>

typedef struct {
  void *memory;
  uptr cursor;
  usize size;
} Alc_Alloc_Arena_Block;

typedef struct {
  Alc_Vector(Alc_Alloc_Arena_Block) blocks;
  usize blocks_num;
} Alc_Alloc_Arena;

Alc_Alloc_Arena alc_alloc_arena_create(void);
void alc_alloc_arena_destroy(Alc_Alloc_Arena *alloc);

void *alc_alloc_arena_allocate_aligned(Alc_Alloc_Arena *alloc, usize size, usize alignment);
static inline void *alc_alloc_arena_allocate(Alc_Alloc_Arena *alloc, usize size)
{
  return alc_alloc_arena_allocate_aligned(alloc, size, 16);
}

void alc_alloc_arena_drop(Alc_Alloc_Arena *alloc);

#endif // __ALC_ALLOC_ARENA_H__
