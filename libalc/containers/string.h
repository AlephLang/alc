#ifndef __ALC_STRING_H__
#define __ALC_STRING_H__

#include "alc/defs.h"

typedef struct {
  char *c_str;
  usize len;
} string_t;

string_t string_create(void);
string_t string_create_from(const char *src);
string_t string_create_from_char(char c);

void string_destroy(string_t *str);

void string_append(string_t *dst, const string_t *src);
void string_append_cstr(string_t *dst, const char *src);
void string_append_char(string_t *dst, char c);

#endif // __ALC_STRING_H__
