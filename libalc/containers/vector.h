#ifndef __ALC_VECTOR_H__
#define __ALC_VECTOR_H__

#include "alc/defs.h"

#define Vector(_type) _type *

#define vector_create(_type) __vector_create_impl(sizeof(_type), 16)
#define vector_reserve(_type, _capacity) __vector_create_impl(sizeof(_type), (_capacity))
#define vector_destroy(_v) __vector_destroy_impl((_v))
#define vector_push(_v, _item)           \
  {                                      \
    __typeof__(_item) tmp = _item;       \
    _v = __vector_push_impl((_v), &tmp); \
  }
#define vector_pop(_v, _out_item) __vector_pop_impl((_v), (_out_item))
#define vector_get_capacity(_v) __vector_get_capacity_impl((_v))
#define vector_get_stride(_v) __vector_get_stride_impl((_v))
#define vector_get_length(_v) __vector_get_length_impl((_v))
#define vector_to_array(_v, _out_n) __vector_to_array_impl((_v), (_out_n))
#define vector_clear(_v) __vector_clear_impl((_v))

void *__vector_create_impl(usize stride, usize capacity);
void __vector_destroy_impl(void *vec);
void *__vector_push_impl(void *vec, const void *item);
void __vector_pop_impl(void *vec, void *out_item);
usize __vector_get_capacity_impl(const void *vec);
usize __vector_get_stride_impl(const void *vec);
usize __vector_get_length_impl(const void *vec);
void *__vector_to_array_impl(const void *vec, usize *out_n);
void __vector_clear_impl(void *vec);

#endif // __ALC_VECTOR_H__
