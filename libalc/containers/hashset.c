#include "alc/hashset.h"
#include "alc/defs.h"
#include "crypto.h"
#include <stdlib.h>
#include <string.h>

#define INITIAL_CAPACTIY (1 << 10)

#define CONTROL_EMPTY 0x00

#define MAX_OCCUPANCY 0.65F
#define GROW_FACTOR 2

#define ALC_HASH_1_MASK (~0xFFULL)
#define ALC_HASH_2_MASK (0xFFULL)

#define ALC_HASH_1(_hash) (((_hash) & ALC_HASH_1_MASK) >> 8)
#define ALC_HASH_2(_hash) ((_hash) & ALC_HASH_2_MASK)

typedef u64 Alc_Hash_1;
typedef u8 Alc_Hash_2;

static void grow_and_rehash(Alc_Hashset *set);

Alc_Hashset alc_hashset_create(void)
{
  usize block_size = (sizeof(u8) + sizeof(char *)) * INITIAL_CAPACTIY;
  void *block = malloc(block_size);
  memset(block, 0, block_size);

  Alc_Hashset set = {
    .control_block = block,
    .key_block = block + (sizeof(u8) * INITIAL_CAPACTIY),
    .capacity = INITIAL_CAPACTIY,
    .occupied = 0,
  };

  return set;
}

void alc_hashset_destroy(Alc_Hashset *set)
{
  if (set->occupied > 0) {
    for (usize i = 0; i < set->capacity; i++) {
      u8 control = set->control_block[i];
      if (control != CONTROL_EMPTY)
        free(set->key_block[i]);
    }
  }
  free(set->control_block);
  memset(set, 0, sizeof(Alc_Hashset));
}

void alc_hashset_set(Alc_Hashset *set, const char *key)
{
  Alc_Hash hash = alc_fnv_1a_str(key);
  Alc_Hash_1 h1 = ALC_HASH_1(hash);
  Alc_Hash_2 h2 = ALC_HASH_2(hash);

  usize pos = h1 % set->capacity;
  loop
  {
    u8 control = set->control_block[pos];
    if (control == CONTROL_EMPTY) {
      usize key_size = strlen(key) + 1;
      set->key_block[pos] = malloc(sizeof(char) * key_size);
      memcpy(set->key_block[pos], key, sizeof(char) * key_size);

      set->control_block[pos] = h2;
      set->occupied++;

      if ALC_UNLIKELY ((f32)set->occupied / (f32)set->capacity > MAX_OCCUPANCY)
        grow_and_rehash(set);

      break;
    } else if (control == h2 && strcmp(key, set->key_block[pos]) == 0)
      break;

    pos = (pos + 1) % set->capacity;
  }
}

void alc_hashset_unset(Alc_Hashset *set, const char *key)
{
  Alc_Hash hash = alc_fnv_1a_str(key);
  Alc_Hash_1 h1 = ALC_HASH_1(hash);
  Alc_Hash_2 h2 = ALC_HASH_2(hash);

  usize pos = h1 % set->capacity;
  loop
  {
    u8 control = set->control_block[pos];
    if (control == CONTROL_EMPTY)
      break;
    else if (control == h2 && strcmp(set->key_block[pos], key) == 0) {
      free(set->key_block[pos]);
      set->control_block[pos] = CONTROL_EMPTY;
      break;
    }

    pos = (pos + 1) % set->capacity;
  }
}

b8 alc_hashset_is_set(Alc_Hashset *set, const char *key)
{
  Alc_Hash hash = alc_fnv_1a_str(key);
  Alc_Hash_1 h1 = ALC_HASH_1(hash);
  Alc_Hash_2 h2 = ALC_HASH_2(hash);

  usize pos = h1 % set->capacity;
  loop
  {
    u8 control = set->control_block[pos];
    if (control == CONTROL_EMPTY)
      return false;
    else if (control == h2 && strcmp(set->key_block[pos], key) == 0)
      return true;

    pos = (pos + 1) % set->capacity;
  }

  ALC_NOREACH();
}
