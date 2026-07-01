#ifndef __ALC_HASHTABLE_H__
#define __ALC_HASHTABLE_H__

#include <alc/defs.h>

typedef u8 Alc_Control;
typedef u32 Alc_Hash;

typedef struct {
  Alc_Control *control_block;
  char **key_block;
  void *value_block;

  usize capacity;
  usize stride;
  usize occupied;

  b8 is_pointer;
} Alc_Hashtable;

ALC_API Alc_Hashtable hashtable_create(usize stride, b8 is_pointer);
ALC_API void hashtable_destroy(Alc_Hashtable *ht);

ALC_API void hashtable_put(Alc_Hashtable *ht, const char *key, const void *value);
ALC_API void *hashtable_get(Alc_Hashtable *ht, const char *key);

typedef void (*Alc_Foreach_Fn)(usize index, void *value);
ALC_API void hashtable_foreach(Alc_Hashtable *ht, Alc_Foreach_Fn foreach_fn);

#endif // __ALC_HASHTABLE_H__
