#ifndef __ALC_STRING_H__
#define __ALC_STRING_H__

#include "alc/defs.h"

typedef struct {
  char *c_str;
  usize len;
} String;

String string_create(void);
String string_create_from(const char *src);
String string_create_from_char(char c);

void string_destroy(String *str);

void string_append(String *dst, const String *src);
void string_append_cstr(String *dst, const char *src);
void string_append_char(String *dst, char c);

#endif // __ALC_STRING_H__
