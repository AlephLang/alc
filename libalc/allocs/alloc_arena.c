#include "allocs/alloc_arena.h"
#include "alc/defs.h"
#include "containers/vector.h"
#include <stdlib.h>

#define MIN_BLOCK_SIZE (1 << 20)

static inline alloc_arena_block_t *add_block(alloc_arena_t *alloc, usize size);
static inline u64 get_aligned(u64 x, u64 alignment);

alloc_arena_t alloc_arena_create(void)
{
  return (alloc_arena_t){
    .blocks = vector_create(alloc_arena_block_t),
    .blocks_num = 0,
  };
}

void alloc_arena_destroy(alloc_arena_t *alloc)
{
  ALC_ASSUME(alloc != nullptr);

  for (usize i = 0; i < alloc->blocks_num; i++) {
    ALC_ASSUME(alloc->blocks[i].memory != nullptr);
    free(alloc->blocks[i].memory);
  }

  vector_destroy(alloc->blocks);
  alloc->blocks = nullptr;
  alloc->blocks_num = 0;
}

void *alloc_arena_allocate_aligned(alloc_arena_t *alloc, usize size, usize alignment)
{
  ALC_ASSUME(alloc != nullptr);
  ALC_ASSUME(size > 0);
  ALC_ASSUME(alignment > 0);
  ALC_ASSUME(size + alignment < (4llu << 30llu));

// NOTE: To debug arena allocations, uncomment this define:
// #define _DEBUG_ARENA_ALLOC
#ifdef _DEBUG_ARENA_ALLOC
  uptr base;
#endif
  uptr block;

  b8 allocated = false;

  for (s64 i = alloc->blocks_num - 1; i >= 0; i--) {
    alloc_arena_block_t *cur_block = &alloc->blocks[i];

    uptr raw_block = cur_block->cursor;
    uptr aligned_block = get_aligned(raw_block, alignment);
    uptr aligned_block_end = aligned_block + size;
    if (aligned_block_end < (uptr)cur_block->memory + cur_block->size) {
#ifdef _DEBUG_ARENA_ALLOC
      base = raw_block;
#endif
      block = aligned_block;
      cur_block->cursor = aligned_block_end;
      allocated = true;
      break;
    }
  }

  if (!allocated) {
    alloc_arena_block_t *alloc_block =
      add_block(alloc, get_aligned(size + alignment, MIN_BLOCK_SIZE));
    uptr raw_block = alloc_block->cursor;
    uptr aligned_block = get_aligned(raw_block, alignment);
    uptr aligned_block_end = aligned_block + size;
#ifdef _DEBUG_ARENA_ALLOC
    base = raw_block;
#endif
    block = aligned_block;
    alloc_block->cursor = aligned_block_end;
  }

#ifdef _DEBUG_ARENA_ALLOC
  static usize alloc_i = 0;
  printf("arena: Allocation #%zu:\n", ++alloc_i);
  printf("       - base:         %p\n", (void *)base);
  printf("       - start:        %p\n", (void *)block);
  printf("       - end:          %p\n", (void *)(block + size));
  printf("       - size:         %zu\n", size);
  printf("       - range:        (%p)%p...%p\n", (void *)base, (void *)block,
         (void *)(block + size));
  printf("       - alignment:    %zu\n", alignment);
  printf("       - align offset: %zu\n", block - base);
#endif

  return (void *)block;
}

void alloc_arena_drop(alloc_arena_t *alloc)
{
  ALC_ASSUME(alloc != nullptr);

  for (usize i = 0; i < alloc->blocks_num; i++)
    alloc->blocks[i].cursor = (uptr)alloc->blocks[i].memory;
}

static inline alloc_arena_block_t *add_block(alloc_arena_t *alloc, usize size)
{
  void *memory = malloc(size);
  uptr cursor = (uptr)memory;

  alloc_arena_block_t block = {
    .memory = memory,
    .cursor = cursor,
    .size = size,
  };

  vector_push(alloc->blocks, block);
  return &alloc->blocks[alloc->blocks_num++];
}

static inline u64 get_aligned(u64 x, u64 alignment)
{
  return x + (-x & (alignment - 1));
}
