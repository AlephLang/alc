#include "crypto.h"
#include <string.h>

// https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
#define FNV_PRIME (0x00000100000001B3ull)
#define FNV_OFFSET_BASIS (0xCBF29CE484222325ull)

Alc_Hash alc_fnv_1a(const void *memory, usize n)
{
  const u8 *s = memory;

  Alc_Hash hash = FNV_OFFSET_BASIS;

  for (; n; n--, s++) {
    hash ^= *s;
    hash *= FNV_PRIME;
  }

  return hash;
}

Alc_Hash alc_fnv_1a_str(const char *str)
{
  return alc_fnv_1a(str, sizeof(char) * strlen(str));
}
