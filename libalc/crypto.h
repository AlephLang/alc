#ifndef __ALC_CRYPTO_H__
#define __ALC_CRYPTO_H__

#include "alc/defs.h"

typedef u64 Alc_Hash;

Alc_Hash alc_fnv_1a(const void *memory, usize n);
Alc_Hash alc_fnv_1a_str(const char *str);

#endif // __ALC_CRYPTO_H__
