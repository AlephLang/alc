#ifndef __ALC_FILESYSTEM_H__
#define __ALC_FILESYSTEM_H__

#include <alc/string.h>
#include <alc/vector.h>
#include <alc/defs.h>

typedef struct __Alc_File Alc_File;
typedef struct __Alc_Directory Alc_Directory;

typedef enum {
  ALC_FILE_READ_ONLY = 1 << 0,
  ALC_FILE_WRITE_ONLY = 1 << 1,
  ALC_FILE_READ_WRITE = ALC_FILE_READ_ONLY | ALC_FILE_WRITE_ONLY,
  ALC_FILE_CREATE = 1 << 2,
} Alc_File_Flag_Bits;
typedef u8 Alc_File_Flags;

typedef struct {
  Alc_Vector(const char *) files;
  Alc_Vector(const char *) directories;
} Alc_Directory_Content;

ALC_API void alc_filesystem_path_simplify(const char *path, char _STATIC_SIZE(buf, MAX_PATH_SIZE));

// Will only work if 'path' exists.
ALC_API void alc_filesystem_path_absolute(const char *path, char _STATIC_SIZE(buf, MAX_PATH_SIZE));

ALC_API void alc_filesystem_path_get_file_name(const char *path,
                                               char _STATIC_SIZE(buf, MAX_PATH_NODE_SIZE));
ALC_API void alc_filesystem_path_get_file_extension(const char *path,
                                                    char _STATIC_SIZE(buf, MAX_PATH_NODE_SIZE));

ALC_API Alc_File *alc_filesystem_file_open(const char *path, Alc_File_Flags flags);
ALC_API void alc_filesystem_file_close(Alc_File *file);
ALC_API b8 alc_filesystem_file_exists(const char *path);
ALC_API usize alc_filesystem_file_get_size(Alc_File *file);
ALC_API usize alc_filesystem_file_read(Alc_File *file, char *buf, usize n);
ALC_API usize alc_filesystem_file_write(Alc_File *file, const char *src, usize n);
ALC_API usize alc_filesystem_file_append(Alc_File *file, const char *src, usize n);
ALC_API b8 alc_filesystem_file_is_empty(Alc_File *file);

ALC_API Alc_Directory *alc_filesystem_directory_open(const char *path, b8 create);
ALC_API void alc_filesystem_directory_close(Alc_Directory *directory);
ALC_API b8 alc_filesystem_directory_exists(const char *path);
ALC_API b8 alc_filesystem_directory_is_empty(Alc_Directory *directory);

// alc_directory_content_destroy() must be called to free data, returned by this function.
ALC_API Alc_Directory_Content alc_filesystem_directory_get_content(Alc_Directory *directory);

ALC_API void alc_directory_content_destroy(Alc_Directory_Content *directory_content);

#endif // __ALC_FILESYSTEM_H__
