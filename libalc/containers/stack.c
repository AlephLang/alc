#include "alc/stack.h"
#include "alc/vector.h"
#include <stdlib.h>
#include <string.h>

#define DEFAULT_BLOCK_CAPACITY (1 << 10)

static inline Alc_Stack_Block create_block(Alc_Stack_Base *stack);

Alc_Stack_Base __alc_stack_create_impl(usize element_size, Alc_Stack_Create_Opts opts)
{
  Alc_Stack_Base out = {
    .element_size = element_size,
    .block_capacity = opts.block_capacity == 0 ? DEFAULT_BLOCK_CAPACITY : opts.block_capacity,
    .blocks = alc_vector_create(Alc_Stack_Block),
    .cur_block_idx = 0,
  };

  alc_vector_push(out.blocks, create_block(&out));

  return out;
}

void alc_stack_destroy(Alc_Stack_Base *stack)
{
  for (usize i = 0, blocks_len = alc_vector_get_length(stack->blocks); i < blocks_len; i++)
    free(stack->blocks[i].memory);

  memset(stack, 0, sizeof(Alc_Stack_Base));
}

void *alc_stack_get(Alc_Stack_Base *stack, usize index)
{
  usize block_index = index / stack->block_capacity;
  if ALC_UNLIKELY (block_index > stack->cur_block_idx)
    return nullptr;

  Alc_Stack_Block *block = &stack->blocks[block_index];
  usize index_in_block = index % stack->block_capacity;
  if ALC_UNLIKELY (index_in_block >= block->filled)
    return nullptr;

  void *slot = (char *)block->memory + (stack->element_size * index_in_block);
  return slot;
}

void *alc_stack_push(Alc_Stack_Base *stack, const void *value)
{
  if (stack->blocks[stack->cur_block_idx].filled == stack->block_capacity) {
    stack->cur_block_idx++;

    if (stack->cur_block_idx == alc_vector_get_length(stack->blocks))
      alc_vector_push(stack->blocks, create_block(stack));
  }

  Alc_Stack_Block *block = &stack->blocks[stack->cur_block_idx];
  void *slot = (char *)block->memory + (block->filled * stack->element_size);

  memcpy(slot, value, stack->element_size);

  block->filled++;

  return slot;
}

void alc_stack_pop(Alc_Stack_Base *stack, void *out_value)
{
  if (stack->blocks[stack->cur_block_idx].filled == 0) {
    if (stack->cur_block_idx == 0) {
      memset(out_value, 0, stack->element_size);
      return;
    }

    stack->cur_block_idx--;
  }

  Alc_Stack_Block *block = &stack->blocks[stack->cur_block_idx];

  void *slot = (char *)block->memory + ((block->filled - 1) * stack->element_size);
  if (out_value != nullptr)
    memcpy(out_value, slot, stack->element_size);
}

void *alc_stack_top(Alc_Stack_Base *stack)
{
  usize selected_block_idx = stack->cur_block_idx;
  if (stack->blocks[selected_block_idx].filled == 0) {
    if (selected_block_idx == 0)
      return nullptr;

    selected_block_idx--;
  }

  Alc_Stack_Block *block = &stack->blocks[selected_block_idx];
  void *slot = (char *)block->memory + ((block->filled - 1) * stack->element_size);

  return slot;
}

void *alc_stack_bottom(Alc_Stack_Base *stack)
{
  return stack->blocks[0].filled == 0 ? nullptr : stack->blocks[0].memory;
}

usize alc_stack_get_element_size(Alc_Stack_Base *stack)
{
  return stack->element_size;
}

usize alc_stack_get_length(Alc_Stack_Base *stack)
{
  return (stack->cur_block_idx * stack->block_capacity) +
         stack->blocks[stack->cur_block_idx].filled;
}

void alc_stack_drop(Alc_Stack_Base *stack)
{
  stack->cur_block_idx = 0;
}

void alc_stack_foreach(Alc_Stack_Base *stack, Alc_Stack_Foreach_Fn foreach_fn, void *user_data)
{
  for (usize i = 0; i <= stack->cur_block_idx; i++) {
    Alc_Stack_Block *block = &stack->blocks[i];
    for (usize j = 0; j < block->filled; j++) {
      void *slot = (char *)block->memory + (j * stack->element_size);
      foreach_fn((i * stack->block_capacity) + j, slot, user_data);
    }
  }
}

static inline Alc_Stack_Block create_block(Alc_Stack_Base *stack)
{
  return (Alc_Stack_Block){
    .memory = malloc(stack->block_capacity),
    .filled = 0,
  };
}
