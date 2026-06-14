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

  usize required_size = size + alignment;
  alloc_arena_block_t *suitable_block = nullptr;

  for (s64 i = alloc->blocks_num - 1; i >= 0; i--) {
    alloc_arena_block_t *cur_block = &alloc->blocks[i];

    usize available_memory = ((usize)cur_block->memory + cur_block->size) - cur_block->cursor;
    if (available_memory >= required_size) {
      suitable_block = cur_block;
      break;
    }
  }

  if (suitable_block == nullptr)
    suitable_block = add_block(alloc, get_aligned(required_size, MIN_BLOCK_SIZE));

  void *out_block = (void *)suitable_block->cursor;
  suitable_block->cursor += required_size;

  return out_block;
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
