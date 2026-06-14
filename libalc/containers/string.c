#include "containers/string.h"
#include <stdlib.h>
#include <string.h>

string_t string_create(void)
{
  char *empty_cstr = malloc(sizeof(char));
  *empty_cstr = 0;
  return (string_t){ .c_str = empty_cstr, .len = 0 };
}

string_t string_create_from(const char *src)
{
  ALC_ASSUME(src != nullptr);

  usize len = strlen(src);
  char *value = malloc(sizeof(char) * (len + 1));
  memcpy(value, src, len + 1);
  return (string_t){ .c_str = value, .len = len };
}

string_t string_create_from_char(char c)
{
  char *value = malloc(sizeof(char) * 2);
  value[0] = c;
  value[1] = 0;
  return (string_t){ .c_str = value, .len = 1 };
}

void string_destroy(string_t *str)
{
  ALC_ASSUME(str != nullptr);
  ALC_ASSUME(str->c_str != nullptr);
  free(str->c_str);
  str->c_str = nullptr;
  str->len = 0;
}

void string_append(string_t *dst, const string_t *src)
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

void string_append_cstr(string_t *dst, const char *src)
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

void string_append_char(string_t *dst, char c)
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
