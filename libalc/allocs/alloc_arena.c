#include "allocs/alloc_arena.h"
#include "alc/defs.h"
#include "alc/vector.h"
#include <stdlib.h>

#ifdef _DEBUG_ARENA_ALLOC
#include <ctype.h>
#endif

#define MIN_BLOCK_SIZE (1 << 20)

static inline Alloc_Arena_Block *add_block(Alloc_Arena *alloc, usize size);
static inline u64 get_aligned(u64 x, u64 alignment);
#ifndef _DEBUG_ARENA_ALLOC
static void *try_allocate_from_block(Alloc_Arena_Block *alloc_block, usize size, usize alignment);
#else
static void *try_allocate_from_block(Alloc_Arena_Block *alloc_block, usize size, usize alignment,
                                     usize *alloc_i);
#endif

Alloc_Arena alloc_arena_create(void)
{
  return (Alloc_Arena){
    .blocks = alc_vector_create(Alloc_Arena_Block),
    .blocks_num = 0,
#ifdef _DEBUG_ARENA_ALLOC
    .allocations = 0,
#endif
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
#ifndef _DEBUG_ARENA_ALLOC
    out_block = try_allocate_from_block(cur_block, size, alignment);
#else
    out_block = try_allocate_from_block(cur_block, size, alignment, &alloc->allocations);
#endif

    if (out_block != nullptr)
      return out_block;
  }

  void *block = add_block(alloc, get_aligned(size + alignment, MIN_BLOCK_SIZE));
#ifndef _DEBUG_ARENA_ALLOC
  return try_allocate_from_block(block, size, alignment);
#else
  return try_allocate_from_block(block, size, alignment, &alloc->allocations);
#endif
}

void alloc_arena_drop(Alloc_Arena *alloc)
{
  ALC_ASSUME(alloc != nullptr);

  for (usize i = 0; i < alloc->blocks_num; i++)
    alloc->blocks[i].cursor = (uptr)alloc->blocks[i].memory;
}

#ifdef _DEBUG_ARENA_ALLOC
void alloc_arena_print_blocks(const Alloc_Arena *alloc, b8 show_content)
{
  ALC_ASSUME(alloc != nullptr);

  printf("(%s): Allocator 0x%016lX:\n", __FUNCTION__, (uptr)alloc);

  for (usize i = 0; i < alloc->blocks_num; i++) {
    printf("##### BLOCK %zu\n", i + 1);

    Alloc_Arena_Block *block = &alloc->blocks[i];

    uptr base = (uptr)block->memory;
    usize allocated = block->cursor - base;

    printf("base: 0x%016lX\n", base);
    printf("size: %zu B / %0.2f KiB / %0.2f MiB / %0.2f GiB\n", block->size,
           block->size / (f32)ALC_KIB(1), block->size / (f32)ALC_MIB(1),
           block->size / (f32)ALC_GIB(1));
    printf("range: 0x%016lX...0x%016lX\n", base, base + block->size);
    printf("cursor: 0x%016lX\n", block->cursor);
    printf("allocated: %zu B / %0.2f KiB / %0.2f MiB / %0.2f GiB (%0.2f%%)\n", allocated,
           allocated / (f32)ALC_KIB(1), allocated / (f32)ALC_MIB(1), allocated / (f32)ALC_GIB(1),
           allocated / (f32)block->size * 100.0f);

    if (!show_content)
      return;

    printf("content:\n");

    usize i = 0;
    while (i < allocated) {
      printf("0x%016lX:", base + i);
      for (usize j = 0; j < 0x10; j++) {
        if ALC_UNLIKELY (j == 8)
          putchar(' ');

        if ALC_LIKELY (i + j < block->size) {
          unsigned char value = ((unsigned char *)block->memory)[i + j];
          const char *color = value == 0                     ? "\033[31m" :
                              isgraph(value)                 ? "\033[32m" :
                              value == '\n' || value == '\r' ? "\033[33m" :
                                                               "\033[0m";

          printf(" %s%02X\033[0m", color, value);
        } else {
          printf("   ");
        }
      }

      printf(" | ");

      for (usize j = 0; j < 0x10; j++) {
        if ALC_LIKELY (i + j < block->size) {
          unsigned char value = ((unsigned char *)block->memory)[i + j];
          b8 print = isgraph(value);
          const char *color = value == 0                     ? "\033[31m" :
                              isgraph(value)                 ? "\033[32m" :
                              value == '\n' || value == '\r' ? "\033[33m" :
                                                               "\033[0m";
          printf("%s%c\033[0m", color, print ? (char)value : '.');
        } else {
          putchar(' ');
        }
      }

      printf(" |\n");

      i += 0x10;
    }
  }
}
#endif

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

#ifndef _DEBUG_ARENA_ALLOC
static void *try_allocate_from_block(Alloc_Arena_Block *alloc_block, usize size, usize alignment)
#else
static void *try_allocate_from_block(Alloc_Arena_Block *alloc_block, usize size, usize alignment,
                                     usize *alloc_i)
#endif
{
  uptr block;

  uptr base = alloc_block->cursor;
  uptr aligned_block = get_aligned(base, alignment);
  uptr aligned_block_end = aligned_block + size;
  if (aligned_block > (uptr)alloc_block->memory + alloc_block->size)
    return nullptr;

  block = aligned_block;
  alloc_block->cursor = aligned_block_end;

#ifdef _DEBUG_ARENA_ALLOC
  printf("arena: Allocation #%zu:\n", ++(*alloc_i));
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

static inline u64 get_aligned(u64 x, u64 alignment)
{
  return x + (-x & (alignment - 1));
}
