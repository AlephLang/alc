#include "containers/vector.h"
#include "alc/defs.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include <stdlib.h>
#include <string.h>

typedef struct {
  usize capacity, stride, length;
} __header_t;

static inline void *resize(void *v);

static inline __header_t *get_header(void *v)
{
  return (__header_t *)((char *)v - sizeof(__header_t));
}

void *__vector_create_impl(usize stride, usize capacity)
{
  ALC_ASSUME(stride > 0);
  ALC_ASSUME(capacity > 0);

  void *block = malloc(stride * capacity + sizeof(__header_t));
  __header_t *header = block;
  header->capacity = capacity;
  header->stride = stride;
  header->length = 0;
  return (char *)block + sizeof(__header_t);
}

void __vector_destroy_impl(void *vec)
{
  ALC_ASSUME(vec != nullptr);

  void *start = get_header(vec);
  free(start);
}

void *__vector_push_impl(void *vec, const void *item)
{
  ALC_ASSUME(vec != nullptr);
  ALC_ASSUME(item != nullptr);

  __header_t *h = get_header(vec);
  if (h->length >= h->capacity) {
    vec = resize(vec);
    h = get_header(vec);
  }

  void *addr = (char *)vec + (h->length * h->stride);
  memcpy(addr, item, h->stride);
  h->length++;

  return vec;
}

void __vector_pop_impl(void *vec, void *out_item)
{
  ALC_ASSUME(vec != nullptr);
  ALC_ASSUME(out_item != nullptr);

  __header_t *h = get_header(vec);
  ALC_ASSUME(h->length > 0);
}

usize __vector_get_capacity_impl(const void *vec)
{
  ALC_ASSUME(vec != nullptr);

  return get_header((void *)vec)->capacity;
}

usize __vector_get_stride_impl(const void *vec)
{
  ALC_ASSUME(vec != nullptr);

  return get_header((void *)vec)->stride;
}

usize __vector_get_length_impl(const void *vec)
{
  ALC_ASSUME(vec != nullptr);

  return get_header((void *)vec)->length;
}

void *__vector_to_array_impl(const void *vec, usize *out_n)
{
  ALC_ASSUME(vec != nullptr);
  ALC_ASSUME(out_n != nullptr);

  __header_t *h = get_header((void *)vec);

  if ALC_UNLIKELY (h->length == 0) {
    *out_n = 0;
    return nullptr;
  }

  void *out_arr = alloc_arena_allocate(&ctx()->arena, h->stride * h->length);
  memcpy(out_arr, vec, h->stride * h->length);
  *out_n = h->length;

  return out_arr;
}

void __vector_clear_impl(void *vec)
{
  ALC_ASSUME(vec != nullptr);

  get_header(vec)->length = 0;
}

static inline void *resize(void *v)
{
  __header_t *old_h = get_header(v);

  void *new_block = malloc(sizeof(__header_t) + old_h->stride * old_h->capacity * 2);
  __header_t *new_h = new_block;
  new_h->capacity = old_h->capacity * 2;
  new_h->stride = old_h->stride;
  new_h->length = old_h->length;

  void *new_v = (char *)new_block + sizeof(__header_t);
  memcpy(new_v, v, old_h->stride * old_h->capacity);
  __vector_destroy_impl(v);
  return new_v;
}
