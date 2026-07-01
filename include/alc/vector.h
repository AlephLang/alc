#ifndef __ALC_VECTOR_H__
#define __ALC_VECTOR_H__

#include <alc/defs.h>

#define Alc_Vector(_type) _type *

#define alc_vector_create(_type) __alc_vector_create_impl(sizeof(_type), 16)
#define alc_vector_reserve(_type, _capacity) __alc_vector_create_impl(sizeof(_type), (_capacity))
#define alc_vector_destroy(_v) __alc_vector_destroy_impl((_v))
#define alc_vector_push(_v, _item)           \
  {                                          \
    __typeof__(_item) tmp = _item;           \
    _v = __alc_vector_push_impl((_v), &tmp); \
  }
#define alc_vector_pop(_v, _out_item) __alc_vector_pop_impl((_v), (_out_item))
#define alc_vector_get_capacity(_v) __alc_vector_get_capacity_impl((_v))
#define alc_vector_get_stride(_v) __alc_vector_get_stride_impl((_v))
#define alc_vector_get_length(_v) __alc_vector_get_length_impl((_v))
#define alc_vector_to_array(_v, _out_n) __alc_vector_to_array_impl((_v), (_out_n))
#define alc_vector_clear(_v) __alc_vector_clear_impl((_v))

ALC_API void *__alc_vector_create_impl(usize stride, usize capacity);
ALC_API void __alc_vector_destroy_impl(void *vec);
ALC_API void *__alc_vector_push_impl(void *vec, const void *item);
ALC_API void __alc_vector_pop_impl(void *vec, void *out_item);
ALC_API usize __alc_vector_get_capacity_impl(const void *vec);
ALC_API usize __alc_vector_get_stride_impl(const void *vec);
ALC_API usize __alc_vector_get_length_impl(const void *vec);
ALC_API void *__alc_vector_to_array_impl(const void *vec, usize *out_n);
ALC_API void __alc_vector_clear_impl(void *vec);

#endif // __ALC_VECTOR_H__
