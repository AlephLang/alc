#ifndef __ALC_HASHSET_H__
#define __ALC_HASHSET_H__

#include <alc/defs.h>

typedef struct {
  u8 *control_block;
  char **key_block;

  usize capacity;
  usize occupied;
} Alc_Hashset;

ALC_API Alc_Hashset alc_hashset_create(void);
ALC_API void alc_hashset_destroy(Alc_Hashset *set);

ALC_API void alc_hashset_set(Alc_Hashset *set, const char *key);
ALC_API void alc_hashset_unset(Alc_Hashset *set, const char *key);

ALC_API b8 alc_hashset_is_set(Alc_Hashset *set, const char *key);

#endif // __ALC_HASHSET_H__
