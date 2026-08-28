#include "alc/filesystem.h"
#include "alc/defs.h"
#include "alc/vector.h"
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>

typedef struct __Alc_File {
  s32 fd;
  Alc_File_Flags flags;
} Alc_File;

typedef struct __Alc_Directory {
  char path[MAX_PATH_SIZE];
  DIR *dp;
} Alc_Directory;

#ifndef realpath
extern char *realpath(const char *restrict name, char *restrict resolved);
#endif

void alc_filesystem_path_absolute(const char *path, char _STATIC_SIZE(buf, MAX_PATH_SIZE))
{
  realpath(path, buf);
}

Alc_File *alc_filesystem_file_open(const char *path, Alc_File_Flags flags)
{
  ALC_ASSERT((flags & ALC_FILE_READ_WRITE) != 0);

  s32 open_flags = (flags & ALC_FILE_READ_WRITE) == ALC_FILE_READ_WRITE ? O_RDWR :
                   (flags & ALC_FILE_READ_ONLY)                         ? O_RDONLY :
                                                                          O_WRONLY;
  if ((flags & ALC_FILE_CREATE) != 0)
    open_flags |= O_CREAT;

  s32 fd = open(path, open_flags);
  if ALC_UNLIKELY (fd < 0)
    return nullptr;

  Alc_File *file = malloc(sizeof(Alc_File));
  file->fd = fd;
  file->flags = flags;

  return file;
}

void alc_filesystem_file_close(Alc_File *file)
{
  close(file->fd);
  free(file);
}

b8 alc_filesystem_file_exists(const char *path)
{
  return access(path, F_OK) == 0;
}

usize alc_filesystem_file_get_size(Alc_File *file)
{
  usize size = lseek(file->fd, 0, SEEK_END);

  lseek(file->fd, 0, SEEK_SET);

  return size;
}

usize alc_filesystem_file_read(Alc_File *file, char *buf, usize n)
{
  usize bytes_read = read(file->fd, buf, n);

  lseek(file->fd, 0, SEEK_SET);

  return bytes_read;
}

usize alc_filesystem_file_write(Alc_File *file, const char *src, usize n)
{
  usize bytes_written = write(file->fd, src, n);

  lseek(file->fd, 0, SEEK_SET);

  return bytes_written;
}

usize alc_filesystem_file_append(Alc_File *file, const char *src, usize n)
{
  lseek(file->fd, 0, SEEK_END);

  usize bytes_written = write(file->fd, src, n);

  lseek(file->fd, 0, SEEK_SET);

  return bytes_written;
}

b8 alc_filesystem_file_is_empty(Alc_File *file)
{
  return alc_filesystem_file_get_size(file) - 1 == 0; // Do not count EOF
}

Alc_Directory *alc_filesystem_directory_open(const char *path, b8 create)
{
  if (!alc_filesystem_directory_exists(path)) {
    if ALC_UNLIKELY (!create)
      return nullptr;

    if ALC_UNLIKELY (mkdir(path, 0) < 0)
      return nullptr;
  }

  DIR *dp = opendir(path);
  if ALC_UNLIKELY (dp == nullptr)
    return nullptr;

  Alc_Directory *directory = malloc(sizeof(Alc_Directory));
  directory->dp = dp;

  usize path_len = strlen(path) + 1;
  memcpy(directory->path, path, ALC_MIN(sizeof(char) * path_len, sizeof(directory->path)));

  return directory;
}

void alc_filesystem_directory_close(Alc_Directory *directory)
{
  closedir(directory->dp);
  free(directory);
}

b8 alc_filesystem_directory_exists(const char *path)
{
  struct stat st;
  return stat(path, &st) == 0 && S_ISDIR(st.st_mode);
}

b8 alc_filesystem_directory_is_empty(Alc_Directory *directory)
{
  usize entries = 0;

  loop
  {
    struct dirent *de = readdir(directory->dp);
    if (de == nullptr)
      break;

    if (strcmp(de->d_name, ".") == 0 || strcmp(de->d_name, "..") == 0)
      continue;

    entries++;
  }

  rewinddir(directory->dp);

  return entries == 0;
}

Alc_Directory_Content alc_filesystem_directory_get_content(Alc_Directory *directory)
{
  Alc_Directory_Content content = {
    .files = alc_vector_create(const char *),
    .directories = alc_vector_create(const char *),
  };

  loop
  {
    struct dirent *de = readdir(directory->dp);
    if (de == nullptr)
      break;

    if (strcmp(de->d_name, ".") == 0 || strcmp(de->d_name, "..") == 0)
      continue;

    b8 is_dir;

#ifdef _DIRENT_HAVE_D_TYPE
#ifndef DT_UNKNOWN
#define DT_UNKNOWN 0x0
#endif
#ifndef DT_DIR
#define DT_DIR 0x4
#endif
#ifndef DT_LNK
#define DT_LNK 0xA
#endif
    if (de->d_type != DT_UNKNOWN && de->d_type != DT_LNK)
      is_dir = de->d_type == DT_DIR;
    else
#else
#endif
    {
      char de_path[MAX_PATH_SIZE];
      char *p = de_path;
      memcpy(p, directory->path, sizeof(de_path));

      for (; *p; p++)
        ;

      if (p == de_path)
        is_dir = false;
      else {
        if (*p != '/')
          *p++ = '/';

        usize k = MAX_PATH_SIZE - (p - de_path);
        snprintf(p, k, "%s", de->d_name);

        struct stat st;
        is_dir = stat(de_path, &st) == 0 && S_ISDIR(st.st_mode);
      }
    }

    usize de_name_len = strlen(de->d_name) + 1;
    char *de_name = malloc(sizeof(char) * de_name_len);
    memcpy(de_name, de->d_name, sizeof(char) * de_name_len);

    Alc_Vector(const char *) v = is_dir ? content.directories : content.files;
    alc_vector_push(v, de_name);
  }

  rewinddir(directory->dp);

  return content;
}
