#include "containers/hashtable.h"
#include "alc/defs.h"
#include <stdlib.h>
#include <string.h>

#define FNV_PRIME (0x01000193)
#define FNV_OFFSET_BASIS (0x811c9dc5)

#define INITIAL_CAPACITY (1 << 10)

#define CONTROL_EMPTY 0x00

#define MAX_OCCUPANCY 0.75f

#define ALC_HASH_1_MASK (0xFFFFFF00)
#define ALC_HASH_2_MASK (0xFF)

#define ALC_HASH_1(_hash) (((_hash) & ALC_HASH_1_MASK) >> 8)
#define ALC_HASH_2(_hash) ((_hash) & ALC_HASH_2_MASK)

typedef u32 Alc_Hash_1;
typedef u8 Alc_Hash_2;

static Alc_Hash fnv_1a(const char *str);

Alc_Hashtable hashtable_create(usize stride, b8 is_pointer)
{
  ALC_ASSUME(stride > 0 || is_pointer);

  stride = is_pointer ? sizeof(void *) : stride;

  usize block_size = (sizeof(Alc_Control) + sizeof(char *) + stride) * INITIAL_CAPACITY;
  void *block = malloc(block_size);
  memset(block, 0, block_size);

  Alc_Hashtable ht = {
    .control_block = block,
    .key_block = block + (sizeof(Alc_Control) * INITIAL_CAPACITY),
    .value_block = block + ((sizeof(Alc_Control) + sizeof(char *)) * INITIAL_CAPACITY),

    .capacity = INITIAL_CAPACITY,
    .stride = stride,
    .occupied = 0,

    .is_pointer = is_pointer,
  };

  return ht;
}

void hashtable_destroy(Alc_Hashtable *ht)
{
  ALC_ASSUME(ht != nullptr);

  if (ht->occupied > 0) {
    for (usize i = 0; i < ht->capacity; i++) {
      Alc_Control control = ht->control_block[i];
      if (control != CONTROL_EMPTY)
        free(ht->key_block[i]);
    }
  }
  free(ht->control_block);
  memset(ht, 0, sizeof(Alc_Hashtable));
}

void *hashtable_put(Alc_Hashtable *ht, const char *key, const void *value)
{
  ALC_ASSUME(ht != nullptr);
  ALC_ASSUME(key != nullptr);
  ALC_ASSUME(value != nullptr);

  Alc_Hash hash = fnv_1a(key);
  Alc_Hash_1 h1 = ALC_HASH_1(hash);
  Alc_Hash_2 h2 = ALC_HASH_2(hash);

  usize pos = h1 % ht->capacity;
  loop
  {
    Alc_Control control = ht->control_block[pos];
    if (control == CONTROL_EMPTY) {
      usize key_size = strlen(key) + 1;
      ht->key_block[pos] = malloc(sizeof(char) * key_size);
      memcpy(ht->key_block[pos], key, sizeof(char) * key_size);

      void *slot = ht->value_block + (ht->stride * pos);
      if (ht->is_pointer)
        *(void **)slot = (void *)value;
      else
        memcpy(slot, value, ht->stride);

      ht->control_block[pos] = h2;
      ht->occupied++;
      return slot;
    } else if (control == h2 && strcmp(key, ht->key_block[pos]) == 0) {
      void *slot = ht->value_block + (ht->stride * pos);
      if (ht->is_pointer)
        *(void **)slot = (void *)value;
      else
        memcpy(slot, value, ht->stride);

      return slot;
    }

    pos = (pos + 1) % ht->capacity;
  }

  ALC_NOREACH();
}

void *hashtable_get(Alc_Hashtable *ht, const char *key)
{
  ALC_ASSUME(ht != nullptr);
  ALC_ASSUME(key != nullptr);

  Alc_Hash hash = fnv_1a(key);
  Alc_Hash_1 h1 = ALC_HASH_1(hash);
  Alc_Hash_2 h2 = ALC_HASH_2(hash);

  usize pos = h1 % ht->capacity;
  loop
  {
    Alc_Control control = ht->control_block[pos];

    if (control == CONTROL_EMPTY)
      break;
    else if (control == h2 && strcmp(ht->key_block[pos], key) == 0)
      return ht->value_block + (ht->stride * pos);

    pos = (pos + 1) % ht->capacity;
  }

  return nullptr;
}

void hashtable_foreach(Alc_Hashtable *ht, Alc_Foreach_Fn foreach_fn)
{
  ALC_ASSUME(ht != nullptr);
  ALC_ASSUME(foreach_fn != nullptr);

  if (ht->occupied == 0)
    return;

  for (usize i = 0; i < ht->capacity; i++) {
    Alc_Control control = ht->control_block[i];
    if (control != CONTROL_EMPTY)
      foreach_fn(i, ht->value_block + (ht->stride * i));
  }
}

static Alc_Hash fnv_1a(const char *str)
{
  Alc_Hash hash = FNV_OFFSET_BASIS;
  for (; *str; str++) {
    hash ^= *str;
    hash *= FNV_PRIME;
  }
  return hash;
}
