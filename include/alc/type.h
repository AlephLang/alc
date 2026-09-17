#ifndef __ALC_TYPE_H__
#define __ALC_TYPE_H__

#include <alc/stack.h>
#include <alc/vector.h>
#include <alc/entry.h>
#include <alc/hashtable.h>
#include <alc/ast.h>

typedef struct __Alc_Program Alc_Program;
typedef struct __Alc_Module Alc_Module;
typedef struct __Alc_Source_File Alc_Source_File;

typedef struct {
  char *name;
  u64 value;
} Alc_Enum_Element;

typedef enum {
  ALC_TYPE_KIND_ERROR,
  ALC_TYPE_KIND_GENERIC_ERROR_INSTANCE,
  ALC_TYPE_KIND_VOID,
  ALC_TYPE_KIND_BOOL,
  ALC_TYPE_KIND_INT_8,
  ALC_TYPE_KIND_INT_16,
  ALC_TYPE_KIND_INT_32,
  ALC_TYPE_KIND_INT_64,
  ALC_TYPE_KIND_UINT_8,
  ALC_TYPE_KIND_UINT_16,
  ALC_TYPE_KIND_UINT_32,
  ALC_TYPE_KIND_UINT_64,
  ALC_TYPE_KIND_FLOAT_32,
  ALC_TYPE_KIND_FLOAT_64,
  ALC_TYPE_KIND_STRUCT,
  ALC_TYPE_KIND_GENERIC_STRUCT,
  ALC_TYPE_KIND_GENERIC_STRUCT_INSTANCE,
  ALC_TYPE_KIND_UNION,
  ALC_TYPE_KIND_ENUM,
  ALC_TYPE_KIND_FUNCTION,
  ALC_TYPE_KIND_ALIAS,
  ALC_TYPE_KIND_POINTER,
  ALC_TYPE_KIND_CARRAY,
  ALC_TYPE_KIND_SLICE,
  ALC_TYPE_KIND_TUPLE,
} Alc_Type_Kind;

typedef struct __Alc_Type {
  union {
    struct {
    } GENERIC_ERROR_INSTANCE;

    struct {
      char *name;
    } STRUCT;

    struct {
      char *name;
    } GENERIC_STRUCT;

    struct {
      char *name;
      struct __Alc_Type *bound_generic_struct;
      struct __Alc_Type **types;
      usize types_num;
    } GENERIC_STRUCT_INSTANCE;

    struct {
      char *name;
    } UNION;

    struct {
      char *name;
      Alc_Hashtable(Alc_Enum_Element) elements;
      usize required_size;
      b8 is_unsigned;
    } ENUM;

    struct {
      struct __Alc_Type *return_type;
      // TODO: array of arguments.
      b8 is_variadic;
    } FUNCTION;

    struct {
      char *name;
      struct __Alc_Type *aliased_type;
    } ALIAS;

    struct {
      struct __Alc_Type *indirected_type;
    } POINTER;

    struct {
      u64 length;
      struct __Alc_Type *stored_type;
    } CARRAY;

    struct {
      u64 length;
      struct __Alc_Type *stored_type;
    } SLICE;

    struct {
      struct __Alc_Type **types;
      usize types_num;
    } TUPLE;
  };

  Alc_Source_File *source_file;
  Alc_Ast *bound_ast;

  Alc_Type_Kind kind;

  Alc_Entry_Scope scope;
} Alc_Type;

typedef struct {
  Alc_Stack(Alc_Type) type_stack;

  struct {
    Alc_Type *type_error;
    Alc_Type *type_void;
    Alc_Type *type_bool;
    Alc_Type *type_int_8;
    Alc_Type *type_int_16;
    Alc_Type *type_int_32;
    Alc_Type *type_int_64;
    Alc_Type *type_uint_8;
    Alc_Type *type_uint_16;
    Alc_Type *type_uint_32;
    Alc_Type *type_uint_64;
    Alc_Type *type_float_32;
    Alc_Type *type_float_64;
    Alc_Type *type_usize;
    Alc_Type *type_ssize;
    Alc_Type *type_uptr;
    Alc_Type *type_sptr;
    Alc_Type *type_off_32;
    Alc_Type *type_off_64;
  } builtins;
} Alc_Type_Storage;

ALC_API Alc_Type_Storage alc_type_storage_create(usize block_capacity);
ALC_API void alc_type_storage_destroy(Alc_Type_Storage *storage);

ALC_API Alc_Type *alc_type_storage_allocate_type(Alc_Type_Storage *storage);
ALC_API Alc_Type *alc_type_storage_add_type(Alc_Type_Storage *storage, const Alc_Type *type);

ALC_API Alc_Type *alc_type_storage_find_duplicate(Alc_Type_Storage *storage, const Alc_Type *type);

ALC_API b8 alc_type_is_same(const Alc_Type *t1, const Alc_Type *t2);
ALC_API b8 alc_type_is_integer(const Alc_Type *t);
ALC_API b8 alc_type_is_integer_explicit(const Alc_Type *t);
ALC_API b8 alc_type_is_bool(const Alc_Type *t);
ALC_API b8 alc_type_is_pointer(const Alc_Type *t);
ALC_API b8 alc_type_is_signed(const Alc_Type *t);
ALC_API b8 alc_type_is_unsigned(const Alc_Type *t);
ALC_API b8 alc_type_is_enum(const Alc_Type *t);
ALC_API b8 alc_type_is_float(const Alc_Type *t);
ALC_API b8 alc_type_is_complete(const Alc_Type *t);
ALC_API b8 alc_type_is_error(const Alc_Type *t);

ALC_API Alc_Type *alc_type_propagate(Alc_Type_Storage *storage, Alc_Type *a, Alc_Type *b);

ALC_API u64 alc_type_get_size(const Alc_Type *t);

struct __alc_type_to_string_opts {
  struct {
    Alc_Module *relative_module;
    b8 relative;
    b8 include;
  } namespace;

  struct {
    b8 expand;
  } alias;
};

#define alc_type_to_string(_buf, _n, _t, ...) \
  __alc_type_to_string_impl((_buf), (_n), (_t), (struct __alc_type_to_string_opts){ __VA_ARGS__ })
ALC_API usize __alc_type_to_string_impl(char *buf, usize n, const Alc_Type *t,
                                        struct __alc_type_to_string_opts opts);

ALC_API Alc_Type *alc_type_get_builtin(Alc_Type_Storage *storage, const char *name);
ALC_API b8 alc_type_is_builtin(Alc_Type_Storage *storage, const char *name);

ALC_API Alc_Type *alc_type_resolve_from_ast(Alc_Program *program, Alc_Source_File *sourcefile,
                                            Alc_Ast *ast);

#endif // __ALC_TYPE_H__
