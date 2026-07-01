#ifndef __ALC_STRING_H__
#define __ALC_STRING_H__

#include <alc/defs.h>

typedef struct {
  char *c_str;
  usize len;
} Alc_String;

ALC_API Alc_String alc_string_create(void);
ALC_API Alc_String alc_string_create_from(const char *src);
ALC_API Alc_String alc_string_create_from_char(char c);

ALC_API void alc_string_destroy(Alc_String *str);

ALC_API void alc_string_append(Alc_String *dst, const Alc_String *src);
ALC_API void alc_string_append_cstr(Alc_String *dst, const char *src);
ALC_API void alc_string_append_char(Alc_String *dst, char c);

#endif // __ALC_STRING_H__
