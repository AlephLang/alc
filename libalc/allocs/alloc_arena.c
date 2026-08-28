#include "allocs/alloc_arena.h"
#include "alc/defs.h"
#include "alc/vector.h"
#include <stdlib.h>

#define MIN_BLOCK_SIZE (1 << 20)

static inline Alloc_Arena_Block *add_block(Alloc_Arena *alloc, usize size);
static inline u64 get_aligned(u64 x, u64 alignment);
static void *try_allocate_from_block(Alloc_Arena_Block *alloc_block, usize size, usize alignment);

Alloc_Arena alloc_arena_create(void)
{
  return (Alloc_Arena){
    .blocks = alc_vector_create(Alloc_Arena_Block),
    .blocks_num = 0,
  };
}

void alloc_arena_destroy(Alloc_Arena *alloc)
{
  ALC_ASSUME(alloc != nullptr);

  for (usize i = 0; i < alloc->blocks_num; i++) {
    ALC_ASSUME(alloc->blocks[i].memory != nullptr);
    free(alloc->blocks[i].memory);
  }

  alc_vector_destroy(alloc->blocks);
  alloc->blocks = nullptr;
  alloc->blocks_num = 0;
}

void *alloc_arena_allocate_aligned(Alloc_Arena *alloc, usize size, usize alignment)
{
  ALC_ASSUME(alloc != nullptr);
  ALC_ASSUME(size > 0);
  ALC_ASSUME(alignment > 0);
  ALC_ASSUME(size + alignment < (4llu << 30llu));

  for (s64 i = alloc->blocks_num - 1; i >= 0; i--) {
    Alloc_Arena_Block *cur_block = &alloc->blocks[i];
    void *out_block;
    out_block = try_allocate_from_block(cur_block, size, alignment);

    if (out_block != nullptr)
      return out_block;
  }

  void *block = add_block(alloc, get_aligned(size + alignment, MIN_BLOCK_SIZE));
  return try_allocate_from_block(block, size, alignment);
}

void alloc_arena_drop(Alloc_Arena *alloc)
{
  ALC_ASSUME(alloc != nullptr);

  for (usize i = 0; i < alloc->blocks_num; i++)
    alloc->blocks[i].cursor = (uptr)alloc->blocks[i].memory;
}

static inline Alloc_Arena_Block *add_block(Alloc_Arena *alloc, usize size)
{
  void *memory = malloc(size);
  uptr cursor = (uptr)memory;

  Alloc_Arena_Block block = {
    .memory = memory,
    .cursor = cursor,
    .size = size,
  };

  alc_vector_push(alloc->blocks, block);
  return &alloc->blocks[alloc->blocks_num++];
}

static void *try_allocate_from_block(Alloc_Arena_Block *alloc_block, usize size, usize alignment)
{
  uptr block;

  uptr base = alloc_block->cursor;
  uptr aligned_block = get_aligned(base, alignment);
  uptr aligned_block_end = aligned_block + size;
  if (aligned_block > (uptr)alloc_block->memory + alloc_block->size)
    return nullptr;

  block = aligned_block;
  alloc_block->cursor = aligned_block_end;

  return (void *)block;
}

static inline u64 get_aligned(u64 x, u64 alignment)
{
  return x + (-x & (alignment - 1));
}
