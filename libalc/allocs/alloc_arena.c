#include "alc/alloc_arena.h"
#include "alc/defs.h"
#include "alc/vector.h"
#include <stdio.h>
#include <stdlib.h>
#include "debug.h"
#include "alloc_arena_debug.h"

#ifdef __ALC_DEBUG_ARENA__
#include <ctype.h>
#endif

#define MIN_BLOCK_SIZE (1 << 20)

static inline Alc_Alloc_Arena_Block *add_block(Alc_Alloc_Arena *alloc, usize size);
static void *try_allocate_from_block(Alc_Alloc_Arena_Block *alc_alloc_block, usize size,
                                     usize alignment);

Alc_Alloc_Arena alc_alloc_arena_create(void)
{
  return (Alc_Alloc_Arena){
    .blocks = alc_vector_create(Alc_Alloc_Arena_Block),
    .blocks_num = 0,
  };
}

void alc_alloc_arena_destroy(Alc_Alloc_Arena *alloc)
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

void *alc_alloc_arena_allocate_aligned(Alc_Alloc_Arena *alloc, usize size, usize alignment)
{
  ALC_ASSUME(alloc != nullptr);
  ALC_ASSUME(size > 0);
  ALC_ASSUME(alignment > 0);
  ALC_ASSUME(size + alignment < (4llu << 30llu));

  for (s64 i = alloc->blocks_num - 1; i >= 0; i--) {
    Alc_Alloc_Arena_Block *cur_block = &alloc->blocks[i];
    void *out_block;
    out_block = try_allocate_from_block(cur_block, size, alignment);

    if (out_block != nullptr)
      return out_block;
  }

  void *block = add_block(alloc, alc_get_aligned(size + alignment, MIN_BLOCK_SIZE));
  return try_allocate_from_block(block, size, alignment);
}

void alc_alloc_arena_drop(Alc_Alloc_Arena *alloc)
{
  ALC_ASSUME(alloc != nullptr);

  for (usize i = 0; i < alloc->blocks_num; i++)
    alloc->blocks[i].cursor = (uptr)alloc->blocks[i].memory;
}

#ifdef __ALC_DEBUG_ARENA__
void alc_alloc_arena_debug_print(Alc_Alloc_Arena *alloc, b8 show_content)
{
  printf("(%s): Allocator %p:\n", __FUNCTION__, alloc);

  for (usize i = 0; i < alloc->blocks_num; i++) {
    printf("##### BLOCK %zu\n", i + 1);

    Alc_Alloc_Arena_Block *block = &alloc->blocks[i];

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

static inline Alc_Alloc_Arena_Block *add_block(Alc_Alloc_Arena *alloc, usize size)
{
  void *memory = malloc(size);
  uptr cursor = (uptr)memory;

  Alc_Alloc_Arena_Block block = {
    .memory = memory,
    .cursor = cursor,
    .size = size,
  };

  alc_vector_push(alloc->blocks, block);
  return &alloc->blocks[alloc->blocks_num++];
}

static void *try_allocate_from_block(Alc_Alloc_Arena_Block *alc_alloc_block, usize size,
                                     usize alignment)
{
  uptr base = alc_alloc_block->cursor;
  uptr aligned_block = alc_get_aligned(base, alignment);
  uptr aligned_block_end = aligned_block + size;
  if (aligned_block > (uptr)alc_alloc_block->memory + alc_alloc_block->size)
    return nullptr;

  uptr block = aligned_block;
  alc_alloc_block->cursor = aligned_block_end;

#ifdef __ALC_DEBUG_ARENA__
  printf("(%s) Allocation info:\n", __FUNCTION__);
  printf("\t*         base: %p\n", (void *)base);
  printf("\t*        start: %p\n", (void *)block);
  printf("\t*          end: %p\n", (void *)(block + size));
  printf("\t*         size: %zu\n", size);
  printf("\t*        range: (%p)%p...%p\n", (void *)base, (void *)block, (void *)(block + size));
  printf("\t*    alignment: %zu\n", alignment);
  printf("\t* align offset: %zu\n", block - base);
#endif

  return (void *)block;
}
