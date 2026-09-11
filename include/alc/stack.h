#ifndef __ALC_STACK_H__
#define __ALC_STACK_H__

#include <alc/vector.h>
#include <alc/defs.h>

typedef struct {
  void *memory;
  usize filled;
} Alc_Stack_Block;

typedef struct {
  usize block_capacity;
  usize element_size;
  Alc_Vector(Alc_Stack_Block) blocks;
  usize cur_block_idx;
} Alc_Stack_Base;

#define Alc_Stack(_type) Alc_Stack_Base

typedef struct {
  usize block_capacity; // If set to zero, will be set to the default size.
} Alc_Stack_Create_Opts;

#define alc_stack_create(_type, ...) \
  __alc_stack_create_impl(sizeof(_type), (Alc_Stack_Create_Opts){ __VA_ARGS__ })
ALC_API Alc_Stack_Base __alc_stack_create_impl(usize element_size, Alc_Stack_Create_Opts opts);
ALC_API void alc_stack_destroy(Alc_Stack_Base *stack);

ALC_API void *alc_stack_get(Alc_Stack_Base *stack, usize index);
ALC_API void *alc_stack_push(Alc_Stack_Base *stack, const void *value);
ALC_API void *alc_stack_bump(Alc_Stack_Base *stack);
ALC_API void alc_stack_pop(Alc_Stack_Base *stack, void *out_value);
ALC_API void *alc_stack_top(Alc_Stack_Base *stack);
ALC_API void *alc_stack_bottom(Alc_Stack_Base *stack);
ALC_API usize alc_stack_get_element_size(Alc_Stack_Base *stack);
ALC_API usize alc_stack_get_length(Alc_Stack_Base *stack);
ALC_API void alc_stack_drop(Alc_Stack_Base *stack);

typedef Alc_Foreach_Fn_Result (*Alc_Stack_Foreach_Fn)(usize index, void *value, void *user_data);
ALC_API void alc_stack_foreach(Alc_Stack_Base *stack, Alc_Stack_Foreach_Fn foreach_fn,
                               void *user_data);

#endif // __ALC_STACK_H__
