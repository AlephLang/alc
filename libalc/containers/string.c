#include "alc/string.h"
#include <stdlib.h>
#include <string.h>

Alc_String alc_string_create(void)
{
  char *empty_cstr = malloc(sizeof(char));
  *empty_cstr = 0;
  return (Alc_String){ .c_str = empty_cstr, .len = 0 };
}

Alc_String alc_string_create_from(const char *src)
{
  ALC_ASSUME(src != nullptr);

  usize len = strlen(src);
  char *value = malloc(sizeof(char) * (len + 1));
  memcpy(value, src, len + 1);
  return (Alc_String){ .c_str = value, .len = len };
}

Alc_String alc_string_create_from_char(char c)
{
  char *value = malloc(sizeof(char) * 2);
  value[0] = c;
  value[1] = 0;
  return (Alc_String){ .c_str = value, .len = 1 };
}

void alc_string_destroy(Alc_String *str)
{
  ALC_ASSUME(str != nullptr);
  ALC_ASSUME(str->c_str != nullptr);
  free(str->c_str);
  str->c_str = nullptr;
  str->len = 0;
}

void alc_string_append(Alc_String *dst, const Alc_String *src)
{
  ALC_ASSUME(dst != nullptr);
  ALC_ASSUME(dst->c_str != nullptr);
  ALC_ASSUME(src != nullptr);
  ALC_ASSUME(src->c_str != nullptr);

  if ALC_UNLIKELY (src->len == 0)
    return;
  else if ALC_UNLIKELY (dst->len == 0) {
    char *copied_data = malloc(sizeof(char) * (src->len + 1));
    memcpy(copied_data, src->c_str, src->len + 1);

    free(dst->c_str);
    dst->c_str = copied_data;
    dst->len = src->len;

    return;
  }

  usize new_len = dst->len + src->len;
  char *new_data = malloc(sizeof(char) * (new_len + 1));
  memcpy(new_data, dst->c_str, sizeof(char) * dst->len);
  memcpy(new_data + dst->len, src->c_str, src->len + 1);

  free(dst->c_str);
  dst->c_str = new_data;
  dst->len = new_len;
}

void alc_string_append_cstr(Alc_String *dst, const char *src)
{
  ALC_ASSUME(dst != nullptr);
  ALC_ASSUME(dst->c_str != nullptr);
  ALC_ASSUME(src != nullptr);

  usize src_len = strlen(src);
  if ALC_UNLIKELY (src_len == 0)
    return;
  else if ALC_UNLIKELY (dst->len == 0) {
    char *copied_data = malloc(sizeof(char) * (src_len + 1));
    memcpy(copied_data, src, sizeof(char) * (src_len + 1));

    free(dst->c_str);
    dst->c_str = copied_data;
    dst->len = src_len;

    return;
  }

  usize new_len = dst->len + src_len;
  char *new_data = malloc(sizeof(char) * (new_len + 1));
  memcpy(new_data, dst->c_str, sizeof(char) * dst->len);
  memcpy(new_data + dst->len, src, sizeof(char) * (src_len + 1));

  free(dst->c_str);
  dst->c_str = new_data;
  dst->len = new_len;
}

void alc_string_append_char(Alc_String *dst, char c)
{
  ALC_ASSUME(dst != nullptr);
  ALC_ASSUME(dst->c_str != nullptr);

  if ALC_UNLIKELY (dst->len == 0) {
    char *new_data = malloc(sizeof(char) * 2);
    new_data[0] = c;
    new_data[1] = 0;

    free(dst->c_str);
    dst->c_str = new_data;
    dst->len = 1;

    return;
  }

  char *new_data = malloc(sizeof(char) * (dst->len + 2));
  memcpy(new_data, dst->c_str, dst->len);
  new_data[dst->len - 1] = 0;
  new_data[dst->len - 2] = c;

  free(dst->c_str);
  dst->c_str = new_data;
  dst->len++;
}
