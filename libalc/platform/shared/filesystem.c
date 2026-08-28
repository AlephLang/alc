#include "alc/filesystem.h"
#include "alc/defs.h"
#include "alc/vector.h"
#include <string.h>
#include <stdlib.h>

void alc_filesystem_path_simplify(const char *path, char _STATIC_SIZE(buf, MAX_PATH_SIZE))
{
#if defined(WIN32) || defined(_WIN32) || defined(__WIN32__)
  {
    // Just to make sure that Windows' stupid path separators don't fuck everything up.

    char new_path[MAX_PATH_SIZE];
    char *n = new_path;
    const char *s = path;
    for (; *s; s++, n++)
      *n = *s == '\\' ? '/' : *s;
    path = new_path;
  }
#endif

  char *origin = buf;
  char *p = buf;
  char *s = buf;

  for (;; path++) {
    if (!*path || *path == '/') {
      *p = 0;
      if (p == origin && *path == '/') {
        // I'm not sure how Windows will behave in this case. Tests needed.
        *p++ = '/';
        *p = 0;
        s = p;
      } else if (strcmp(s, ".") == 0) {
        *s = 0;
        p = s;
      } else if (strcmp(s, "..") == 0) {
        if (s == origin) {
          *p++ = '/';
          *p = 0;
          s = p;
        } else if (s - 1 == origin) {
          *s = 0;
          p = s;
        } else if (*(s - 2) == '.') {
          *p++ = '/';
          *p = 0;
          s = p;
        } else {
          s -= 2;
          for (; s != origin && *s != '/'; s--)
            ;
          if (*s == '/')
            *++s = 0;
          else
            *s = 0;
          p = s;
        }
      } else if (s != p) {
        *p++ = '/';
        *p = 0;
        s = p;
      }

      if (!*path)
        break;

      continue;
    }

    *p++ = *path;
  }

  p--;
  if (*p == '/' && p != origin)
    *p = 0;
}

void alc_filesystem_path_get_file_name(const char *path, char _STATIC_SIZE(buf, MAX_PATH_NODE_SIZE))
{
  const char *p = strrchr(path, '/');
#if defined(WIN32) || defined(_WIN32) || defined(__WIN32__)
  if (p == nullptr)
    p = strrchr(path, '\\');
#endif

  p = p == nullptr ? path : p + 1;

  usize p_len = strlen(p) + 1;
  memcpy(buf, p, sizeof(char) * ALC_MIN(p_len, MAX_PATH_NODE_SIZE));
  buf[MAX_PATH_NODE_SIZE - 1] = 0;
}

void alc_filesystem_path_get_file_extension(const char *path,
                                            char _STATIC_SIZE(buf, MAX_PATH_NODE_SIZE))
{
  const char *dir_last = strrchr(path, '/');
#if defined(WIN32) || defined(_WIN32) || defined(__WIN32__)
  if (dir_last == nullptr)
    dir_last = strrchr(path, '\\');
#endif

  if (dir_last == nullptr)
    dir_last = path;

  char *ext = strchr(dir_last, '.');
  if (ext == nullptr || ext == dir_last) {
    buf[0] = 0;
    return;
  }

  ext++;
  usize ext_len = strlen(ext) + 1;
  memcpy(buf, ext, sizeof(char) * ALC_MIN(ext_len, MAX_PATH_NODE_SIZE));
  buf[MAX_PATH_NODE_SIZE - 1] = 0;
}

void alc_directory_content_destroy(Alc_Directory_Content *directory_content)
{
  for (usize i = 0, files_len = alc_vector_get_length(directory_content->files); i < files_len; i++)
    free((char *)directory_content->files[i]);

  for (usize i = 0, directories_len = alc_vector_get_length(directory_content->directories);
       i < directories_len; i++)
    free((char *)directory_content->directories[i]);

  alc_vector_destroy(directory_content->files);
  alc_vector_destroy(directory_content->directories);

  memset(directory_content, 0, sizeof(Alc_Directory_Content));
}
