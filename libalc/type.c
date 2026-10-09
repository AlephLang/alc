#include "alc/type.h"
#include "alc/alloc_arena.h"
#include "alc/ast.h"
#include "alc/program.h"
#include "alc/defs.h"
#include "alc/module.h"
#include "alc/sourcefile.h"
#include "alc/stack.h"
#include "alc/vector.h"
#include "global.h"
#include <stdio.h>
#include <string.h>

#define RESERVE_CHUNKS_NUM 128

static inline Alc_Type *allocate_type_zero_init(Alc_Type_Storage *storage);
static inline const Alc_Type *unwrap_alias(const Alc_Type *t);
static Alc_Foreach_Result _find_duplicate_fn(usize index, void *value, void *user_data);

Alc_Type_Storage alc_type_storage_create(usize block_capacity)
{
  Alc_Type_Storage out = {
    .type_stack = alc_stack_create(Alc_Type, .block_capacity = block_capacity),
  };

  out.builtins.type_error = allocate_type_zero_init(&out);
  out.builtins.type_void = allocate_type_zero_init(&out);
  out.builtins.type_bool = allocate_type_zero_init(&out);
  out.builtins.type_int_8 = allocate_type_zero_init(&out);
  out.builtins.type_int_16 = allocate_type_zero_init(&out);
  out.builtins.type_int_32 = allocate_type_zero_init(&out);
  out.builtins.type_int_64 = allocate_type_zero_init(&out);
  out.builtins.type_uint_8 = allocate_type_zero_init(&out);
  out.builtins.type_uint_16 = allocate_type_zero_init(&out);
  out.builtins.type_uint_32 = allocate_type_zero_init(&out);
  out.builtins.type_uint_64 = allocate_type_zero_init(&out);
  out.builtins.type_float_32 = allocate_type_zero_init(&out);
  out.builtins.type_float_64 = allocate_type_zero_init(&out);
  out.builtins.type_usize = allocate_type_zero_init(&out);
  out.builtins.type_ssize = allocate_type_zero_init(&out);
  out.builtins.type_uptr = allocate_type_zero_init(&out);
  out.builtins.type_sptr = allocate_type_zero_init(&out);
  out.builtins.type_off_32 = allocate_type_zero_init(&out);
  out.builtins.type_off_64 = allocate_type_zero_init(&out);

  out.builtins.type_error->kind = ALC_TYPE_KIND_ERROR;
  out.builtins.type_void->kind = ALC_TYPE_KIND_VOID;
  out.builtins.type_bool->kind = ALC_TYPE_KIND_BOOL;
  out.builtins.type_int_8->kind = ALC_TYPE_KIND_INT_8;
  out.builtins.type_int_16->kind = ALC_TYPE_KIND_INT_16;
  out.builtins.type_int_32->kind = ALC_TYPE_KIND_INT_32;
  out.builtins.type_int_64->kind = ALC_TYPE_KIND_INT_64;
  out.builtins.type_uint_8->kind = ALC_TYPE_KIND_UINT_8;
  out.builtins.type_uint_16->kind = ALC_TYPE_KIND_UINT_16;
  out.builtins.type_uint_32->kind = ALC_TYPE_KIND_UINT_32;
  out.builtins.type_uint_64->kind = ALC_TYPE_KIND_UINT_64;
  out.builtins.type_float_32->kind = ALC_TYPE_KIND_FLOAT_32;
  out.builtins.type_float_64->kind = ALC_TYPE_KIND_FLOAT_64;

#define INITIALIZE_ALIAS(_dst_ptr, _name_array_buf, _aliased_type)                         \
  {                                                                                        \
    ((Alc_Type *)(_dst_ptr))->ALIAS.name =                                                 \
      alc_alloc_arena_allocate_aligned(&ctx()->arena, sizeof _name_array_buf, 1);          \
    memcpy(((Alc_Type *)(_dst_ptr))->ALIAS.name, _name_array_buf, sizeof _name_array_buf); \
    ((Alc_Type *)(_dst_ptr))->ALIAS.aliased_type = (Alc_Type *)(_aliased_type);            \
    ((Alc_Type *)(_dst_ptr))->kind = ALC_TYPE_KIND_ALIAS;                                  \
  }

  // TODO: Make these depend on target architecture
  INITIALIZE_ALIAS(out.builtins.type_usize, "usize", out.builtins.type_uint_64);
  INITIALIZE_ALIAS(out.builtins.type_ssize, "ssize", out.builtins.type_int_64);
  INITIALIZE_ALIAS(out.builtins.type_uptr, "uptr", out.builtins.type_uint_64);
  INITIALIZE_ALIAS(out.builtins.type_sptr, "sptr", out.builtins.type_int_64);

  INITIALIZE_ALIAS(out.builtins.type_off_32, "off32", out.builtins.type_int_32);
  INITIALIZE_ALIAS(out.builtins.type_off_64, "off64", out.builtins.type_int_64);

  return out;
}

void alc_type_storage_destroy(Alc_Type_Storage *storage)
{
  alc_stack_destroy(&storage->type_stack);

  memset(storage, 0, sizeof(Alc_Type_Storage));
}

Alc_Type *alc_type_storage_allocate_type(Alc_Type_Storage *storage)
{
  return alc_stack_bump(&storage->type_stack);
}

Alc_Type *alc_type_storage_add_type(Alc_Type_Storage *storage, const Alc_Type *type)
{
  Alc_Type *out_ptr = alc_type_storage_find_duplicate(storage, type);
  if ALC_LIKELY (out_ptr != nullptr)
    return out_ptr;

  out_ptr = alc_type_storage_allocate_type(storage);
  memcpy(out_ptr, type, sizeof(Alc_Type));

  return out_ptr;
}

Alc_Type *alc_type_storage_find_duplicate(Alc_Type_Storage *storage, const Alc_Type *type)
{
  struct {
    const Alc_Type *type;
    Alc_Type *result;
  } data = { .type = type, .result = nullptr };
  alc_stack_foreach(&storage->type_stack, _find_duplicate_fn, &data);

  return data.result;
}

b8 alc_type_is_same(const Alc_Type *t1, const Alc_Type *t2)
{
  if (t1->kind != t2->kind)
    return false;

  Alc_Type_Kind type_kind = t1->kind;
  switch (type_kind) {
  case ALC_TYPE_KIND_GENERIC_ERROR_INSTANCE: {
    ALC_TODO("Compare ALC_TYPE_KIND_GENERIC_ERROR_INSTANCE");
  }

  case ALC_TYPE_KIND_STRUCT: {
    ALC_TODO("Compare ALC_TYPE_KIND_STRUCT");
  }

  case ALC_TYPE_KIND_GENERIC_STRUCT: {
    ALC_TODO("Compare ALC_TYPE_KIND_GENERIC_STRUCT");
  }

  case ALC_TYPE_KIND_GENERIC_STRUCT_INSTANCE: {
    ALC_TODO("Compare ALC_TYPE_KIND_GENERIC_STRUCT_INSTANCE");
  }

  case ALC_TYPE_KIND_UNION: {
    ALC_TODO("Compare ALC_TYPE_KIND_UNION");
  }

  case ALC_TYPE_KIND_ENUM: {
    ALC_TODO("Compare ALC_TYPE_KIND_ENUM");
  }

  case ALC_TYPE_KIND_FUNCTION: {
    ALC_TODO("Compare ALC_TYPE_KIND_FUNCTION");
  }

  case ALC_TYPE_KIND_ALIAS: {
    Alc_Type *t1_aliased = t1->ALIAS.aliased_type;
    Alc_Type *t2_aliased = t2->ALIAS.aliased_type;

    // TODO: Add an option to also compare names for stronger types.

    return t1_aliased == t2_aliased;
  }

  case ALC_TYPE_KIND_POINTER: {
    Alc_Type *t1_indirected_type = t1->POINTER.indirected_type;
    Alc_Type *t2_indirected_type = t2->POINTER.indirected_type;

    return t1_indirected_type == t2_indirected_type;
  }

  case ALC_TYPE_KIND_CARRAY: {
    Alc_Type *t1_stored_type = t1->CARRAY.stored_type;
    usize t1_length = t1->CARRAY.length;

    Alc_Type *t2_stored_type = t2->CARRAY.stored_type;
    usize t2_length = t2->CARRAY.length;

    return t1_stored_type == t2_stored_type && t1_length == t2_length;
  }

  case ALC_TYPE_KIND_SLICE: {
    Alc_Type *t1_stored_type = t1->SLICE.stored_type;
    usize t1_length = t1->SLICE.length;

    Alc_Type *t2_stored_type = t2->SLICE.stored_type;
    usize t2_length = t2->SLICE.length;

    return t1_stored_type == t2_stored_type && t1_length == t2_length;
  }

  default:
    return true;
  }
}

b8 alc_type_is_integer(const Alc_Type *t)
{
  t = unwrap_alias(t);

  return alc_type_is_integer_explicit(t) || alc_type_is_pointer(t) || alc_type_is_bool(t);
}

b8 alc_type_is_integer_explicit(const Alc_Type *t)
{
  t = unwrap_alias(t);

  switch (t->kind) {
  case ALC_TYPE_KIND_INT_8:
  case ALC_TYPE_KIND_INT_16:
  case ALC_TYPE_KIND_INT_32:
  case ALC_TYPE_KIND_INT_64:
  case ALC_TYPE_KIND_UINT_8:
  case ALC_TYPE_KIND_UINT_16:
  case ALC_TYPE_KIND_UINT_32:
  case ALC_TYPE_KIND_UINT_64:
  case ALC_TYPE_KIND_ENUM:
    return true;

  default:
    return false;
  }
}

b8 alc_type_is_bool(const Alc_Type *t)
{
  t = unwrap_alias(t);

  return t->kind == ALC_TYPE_KIND_BOOL;
}

b8 alc_type_is_pointer(const Alc_Type *t)
{
  t = unwrap_alias(t);

  return t->kind == ALC_TYPE_KIND_POINTER || t->kind == ALC_TYPE_KIND_FUNCTION;
}

b8 alc_type_is_signed(const Alc_Type *t)
{
  t = unwrap_alias(t);

  switch (t->kind) {
  case ALC_TYPE_KIND_INT_8:
  case ALC_TYPE_KIND_INT_16:
  case ALC_TYPE_KIND_INT_32:
  case ALC_TYPE_KIND_INT_64:
  case ALC_TYPE_KIND_FLOAT_32:
  case ALC_TYPE_KIND_FLOAT_64:
    return true;

  case ALC_TYPE_KIND_ENUM:
    return !t->ENUM.is_unsigned;

  default:
    return false;
  }
}

b8 alc_type_is_unsigned(const Alc_Type *t)
{
  t = unwrap_alias(t);

  switch (t->kind) {
  case ALC_TYPE_KIND_UINT_8:
  case ALC_TYPE_KIND_UINT_16:
  case ALC_TYPE_KIND_UINT_32:
  case ALC_TYPE_KIND_UINT_64:
    return true;

  case ALC_TYPE_KIND_ENUM:
    return t->ENUM.is_unsigned;

  default:
    return false;
  }
}

b8 alc_type_is_enum(const Alc_Type *t)
{
  t = unwrap_alias(t);

  return t->kind == ALC_TYPE_KIND_ENUM;
}

b8 alc_type_is_float(const Alc_Type *t)
{
  t = unwrap_alias(t);

  switch (t->kind) {
  case ALC_TYPE_KIND_FLOAT_32:
  case ALC_TYPE_KIND_FLOAT_64:
    return true;

  default:
    return false;
  }
}

b8 alc_type_is_complete(const Alc_Type *t)
{
  t = unwrap_alias(t);

  // HACK: I don't know if there's a need to make it more sophisticated.
  switch (t->kind) {
  case ALC_TYPE_KIND_CARRAY: {
    return t->CARRAY.length != -1ULL && alc_type_is_complete(t->CARRAY.stored_type);
  }

  case ALC_TYPE_KIND_SLICE: {
    return t->SLICE.length != -1ULL && alc_type_is_complete(t->SLICE.stored_type);
  }

  default:
    return true;
  }
}

b8 alc_type_is_error(const Alc_Type *t)
{
  return t->kind == ALC_TYPE_KIND_ERROR ||
         (t->kind == ALC_TYPE_KIND_GENERIC_STRUCT_INSTANCE &&
          alc_type_is_error(t->GENERIC_STRUCT_INSTANCE.bound_generic_struct));
}

Alc_Type *alc_type_propagate(Alc_Type_Storage *storage, Alc_Type *a, Alc_Type *b)
{
  Alc_Type *type_error = storage->builtins.type_error;

  if (a->kind == ALC_TYPE_KIND_ERROR || b->kind == ALC_TYPE_KIND_ERROR ||
      a->kind == ALC_TYPE_KIND_GENERIC_ERROR_INSTANCE ||
      b->kind == ALC_TYPE_KIND_GENERIC_ERROR_INSTANCE)
    return type_error;

  if (a == b)
    return a;

  if ((a->kind == ALC_TYPE_KIND_STRUCT || b->kind == ALC_TYPE_KIND_STRUCT) ||
      (a->kind == ALC_TYPE_KIND_UNION || b->kind == ALC_TYPE_KIND_UNION) ||
      (a->kind == ALC_TYPE_KIND_GENERIC_STRUCT_INSTANCE ||
       b->kind == ALC_TYPE_KIND_GENERIC_STRUCT_INSTANCE))
    return alc_type_is_same(a, b) ? a : type_error;

  usize a_size = alc_type_get_size(a);
  usize b_size = alc_type_get_size(b);
  b8 a_is_enum = alc_type_is_enum(a);
  b8 b_is_enum = alc_type_is_enum(b);
  b8 a_is_integer = alc_type_is_integer(a);
  b8 b_is_integer = alc_type_is_integer(b);
  b8 a_is_pointer = alc_type_is_pointer(a);
  b8 b_is_pointer = alc_type_is_pointer(b);
  b8 a_is_float = alc_type_is_float(a);
  b8 b_is_float = alc_type_is_float(b);

  if (a_is_pointer || b_is_pointer) {
    if (a_is_pointer && b_is_pointer)
      return alc_type_is_same(a, b) ? a : type_error;

    // TODO: Maybe report an explicit error somehow
    if (a_is_pointer && b_is_integer)
      return a_size >= b_size ? a : type_error;

    if (b_is_pointer && a_is_integer)
      return b_size >= a_size ? b : type_error;

    return type_error;
  }

  if (a_is_float || b_is_float) {
    if (a_is_float && b_is_float)
      return a_size > b_size ? a : b;

    if (a_is_float)
      return b_is_integer ? a : type_error;

    return a_is_integer ? b : type_error;
  }

  if (a_is_enum || b_is_enum) {
    if (a_is_enum && b_is_enum)
      return alc_type_is_same(a, b) ? a : type_error;

    return a_is_enum ? b : a;
  }

  Alc_Type *int_propagation_table[4][2] = {
    { storage->builtins.type_int_8, storage->builtins.type_uint_8 },
    { storage->builtins.type_int_16, storage->builtins.type_uint_16 },
    { storage->builtins.type_int_32, storage->builtins.type_uint_32 },
    { storage->builtins.type_int_64, storage->builtins.type_uint_64 },
  };

  b8 is_unsigned = alc_type_is_unsigned(a) || alc_type_is_unsigned(b);
  usize biggest_size = ALC_MAX(a_size, b_size);

  // 0 - 8 bits
  // l - 16 bits
  // 2 - 32 bits
  // 3 - 64 bits
  u8 selected_size = biggest_size == 1                     ? 0 :
                     biggest_size > 1 && biggest_size <= 2 ? 1 :
                     biggest_size > 2 && biggest_size <= 4 ? 2 :
                                                             3;

  return int_propagation_table[selected_size][is_unsigned];
}

u64 alc_type_get_size(const Alc_Type *t)
{
  t = unwrap_alias(t);

  switch (t->kind) {
  case ALC_TYPE_KIND_ERROR:
  case ALC_TYPE_KIND_GENERIC_ERROR_INSTANCE:
  case ALC_TYPE_KIND_VOID:
    return 0;

  case ALC_TYPE_KIND_INT_8:
  case ALC_TYPE_KIND_UINT_8:
  case ALC_TYPE_KIND_BOOL:
    return 1;

  case ALC_TYPE_KIND_INT_16:
  case ALC_TYPE_KIND_UINT_16:
    return 2;

  case ALC_TYPE_KIND_INT_32:
  case ALC_TYPE_KIND_UINT_32:
  case ALC_TYPE_KIND_FLOAT_32:
    return 4;

  case ALC_TYPE_KIND_INT_64:
  case ALC_TYPE_KIND_UINT_64:
  case ALC_TYPE_KIND_FLOAT_64:
    return 8;

  case ALC_TYPE_KIND_POINTER:
  case ALC_TYPE_KIND_FUNCTION:
    return 8; // TODO: Should depend on the target architecture.

  case ALC_TYPE_KIND_ENUM:
    return t->ENUM.required_size;

  case ALC_TYPE_KIND_CARRAY:
    return t->CARRAY.length * alc_type_get_size(t->CARRAY.stored_type);

  case ALC_TYPE_KIND_ALIAS:
    ALC_NOREACH();

  default:
    ALC_TODO("Get size of other types");
  }
}

usize __alc_type_to_string_impl(char *buf, usize n, const Alc_Type *t,
                                struct __alc_type_to_string_opts opts)
{
  ALC_ASSERT((opts.namespace.relative && opts.namespace.relative_module != nullptr) ||
             !opts.namespace.relative);

  if (opts.namespace.include && t->source_file != nullptr) {
    usize written = alc_module_to_namespace_string(
      buf, n, t->source_file->module,
      opts.namespace.relative ? opts.namespace.relative_module : nullptr);
    buf += written;
    n -= written;
  }

  switch (t->kind) {
  case ALC_TYPE_KIND_ERROR:
    return snprintf(buf, n, "<error-type>");

  case ALC_TYPE_KIND_GENERIC_ERROR_INSTANCE:
    ALC_TODO("GENERIC_ERROR_INSTANCE to string");

  case ALC_TYPE_KIND_VOID:
    return snprintf(buf, n, "void");

  case ALC_TYPE_KIND_BOOL:
    return snprintf(buf, n, "bool");

  case ALC_TYPE_KIND_INT_8:
    return snprintf(buf, n, "s8");

  case ALC_TYPE_KIND_INT_16:
    return snprintf(buf, n, "s16");

  case ALC_TYPE_KIND_INT_32:
    return snprintf(buf, n, "s32");

  case ALC_TYPE_KIND_INT_64:
    return snprintf(buf, n, "s64");

  case ALC_TYPE_KIND_UINT_8:
    return snprintf(buf, n, "u8");

  case ALC_TYPE_KIND_UINT_16:
    return snprintf(buf, n, "u16");

  case ALC_TYPE_KIND_UINT_32:
    return snprintf(buf, n, "u32");

  case ALC_TYPE_KIND_UINT_64:
    return snprintf(buf, n, "u64");

  case ALC_TYPE_KIND_FLOAT_32:
    return snprintf(buf, n, "f32");

  case ALC_TYPE_KIND_FLOAT_64:
    return snprintf(buf, n, "f64");

  case ALC_TYPE_KIND_STRUCT:
    return snprintf(buf, n, "%s", t->STRUCT.name);

  case ALC_TYPE_KIND_GENERIC_STRUCT:
    ALC_TODO("GENERIC_STRUCT to string");

  case ALC_TYPE_KIND_GENERIC_STRUCT_INSTANCE:
    ALC_TODO("GENERIC_STRUCT_INSTANCE to string");

  case ALC_TYPE_KIND_UNION:
    return snprintf(buf, n, "%s", t->UNION.name);

  case ALC_TYPE_KIND_ENUM:
    return snprintf(buf, n, "%s", t->ENUM.name);

  case ALC_TYPE_KIND_FUNCTION:
    ALC_TODO("FUNCTION to string");

  case ALC_TYPE_KIND_ALIAS: {
    usize written = snprintf(buf, n, "%s", t->ALIAS.name);
    buf += written;
    n -= written;

    if (opts.alias.expand) {
      usize written2 = snprintf(buf, n, " (aka ");
      buf += written2;
      n -= written2;

      usize written3 = __alc_type_to_string_impl(buf, n, t->ALIAS.aliased_type, opts);
      buf += written3;
      n -= written3;

      usize written4 = snprintf(buf, n, ")");

      written += written2 + written3 + written4;
    }

    return written;
  }

  case ALC_TYPE_KIND_POINTER: {
    usize written = snprintf(buf, n, "*");
    buf += written;
    n -= written;

    return written + __alc_type_to_string_impl(buf, n, t->POINTER.indirected_type, opts);
  }

  case ALC_TYPE_KIND_CARRAY: {
    usize written = snprintf(buf, n, ".[%zu]", t->CARRAY.length);
    buf += written;
    n -= written;

    return written + __alc_type_to_string_impl(buf, n, t->CARRAY.stored_type, opts);
  }

  case ALC_TYPE_KIND_SLICE: {
    usize written = snprintf(buf, n, "[%zu]", t->SLICE.length);
    buf += written;
    n -= written;

    return written + __alc_type_to_string_impl(buf, n, t->SLICE.stored_type, opts);
  }

  case ALC_TYPE_KIND_TUPLE: {
    usize written = snprintf(buf, n, "|");
    buf += written;
    n -= written;

    for (usize i = 0; i < t->TUPLE.types_num; i++) {
      if (i > 0) {
        usize written_comma = snprintf(buf, n, ", ");
        buf += written_comma;
        n -= written_comma;

        written += written_comma;
      }

      usize written_type = __alc_type_to_string_impl(buf, n, t->TUPLE.types[i], opts);
      buf += written_type;
      n -= written_type;

      written += written_type;
    }

    return written + snprintf(buf, n, "|");
  }
  }

  ALC_NOREACH();
}

Alc_Type *alc_type_get_builtin(Alc_Type_Storage *storage, const char *name)
{
  if (strcmp(name, "void") == 0)
    return storage->builtins.type_void;
  else if (strcmp(name, "bool") == 0)
    return storage->builtins.type_bool;
  else if (strcmp(name, "s8") == 0)
    return storage->builtins.type_int_8;
  else if (strcmp(name, "s16") == 0)
    return storage->builtins.type_int_16;
  else if (strcmp(name, "s32") == 0)
    return storage->builtins.type_int_32;
  else if (strcmp(name, "s64") == 0)
    return storage->builtins.type_int_64;
  else if (strcmp(name, "u8") == 0)
    return storage->builtins.type_uint_8;
  else if (strcmp(name, "u16") == 0)
    return storage->builtins.type_uint_16;
  else if (strcmp(name, "u32") == 0)
    return storage->builtins.type_uint_32;
  else if (strcmp(name, "u64") == 0)
    return storage->builtins.type_uint_64;
  else if (strcmp(name, "f32") == 0)
    return storage->builtins.type_float_32;
  else if (strcmp(name, "f64") == 0)
    return storage->builtins.type_float_64;
  else if (strcmp(name, "usize") == 0)
    return storage->builtins.type_usize;
  else if (strcmp(name, "ssize") == 0)
    return storage->builtins.type_ssize;
  else if (strcmp(name, "uptr") == 0)
    return storage->builtins.type_uptr;
  else if (strcmp(name, "sptr") == 0)
    return storage->builtins.type_sptr;
  else if (strcmp(name, "off32") == 0)
    return storage->builtins.type_off_32;
  else if (strcmp(name, "off64") == 0)
    return storage->builtins.type_off_64;
  return nullptr;
}

b8 alc_type_is_builtin(Alc_Type_Storage *storage, const char *name)
{
  return alc_type_get_builtin(storage, name) != nullptr;
}

Alc_Type *alc_type_resolve_from_ast(Alc_Program *program, Alc_Source_File *sourcefile, Alc_Ast *ast)
{
  // TODO: Also resolve namespaces.

  switch (ast->kind) {
  case ALC_AST_KIND_TYPE_PLAIN: {
    const char *name = ast->TYPE_PLAIN.name;
    Alc_Type *type = alc_type_get_builtin(&program->type_storage, name);
    if (type != nullptr)
      return type;

    type = alc_source_file_find_type(sourcefile, name);
    if (type != nullptr)
      return type;

    type = alc_module_find_type(sourcefile->module, name);
    if (type != nullptr)
      return type;

    // TODO: Error: Undefined type.

    return program->type_storage.builtins.type_error;
  }

  case ALC_AST_KIND_TYPE_POINTER: {
    Alc_Type *indirected_type =
      alc_type_resolve_from_ast(program, sourcefile, ast->TYPE_POINTER.type);
    if ALC_UNLIKELY (alc_type_is_error(indirected_type))
      return program->type_storage.builtins.type_error;

    Alc_Type pointer_type = {
      .POINTER.indirected_type = indirected_type,
      .kind = ALC_TYPE_KIND_POINTER,
    };
    Alc_Type *out = alc_type_storage_add_type(&program->type_storage, &pointer_type);
    return out;
  }

  case ALC_AST_KIND_TYPE_OPTIONAL: {
    ALC_TODO("Construct optional.");
  }

  case ALC_AST_KIND_TYPE_SLICE: {
    Alc_Type *stored_type = alc_type_resolve_from_ast(program, sourcefile, ast->TYPE_SLICE.type);
    if ALC_UNLIKELY (alc_type_is_error(stored_type))
      return program->type_storage.builtins.type_error;

    u64 length = -1ULL;
    if (ast->TYPE_SLICE.size_expression != nullptr) {
      // TODO: Get compile-time value from it and verify that it is a positive
      // integer.
    }

    Alc_Type slice_type = {
      .SLICE = {
        .length = length,
        .stored_type = stored_type,
      },
      .kind = ALC_TYPE_KIND_SLICE,
    };
    Alc_Type *out = alc_type_storage_add_type(&program->type_storage, &slice_type);
    return out;
  }

  case ALC_AST_KIND_TYPE_ARRAY: {
    Alc_Type *stored_type = alc_type_resolve_from_ast(program, sourcefile, ast->TYPE_ARRAY.type);
    if ALC_UNLIKELY (alc_type_is_error(stored_type))
      return program->type_storage.builtins.type_error;

    u64 length = -1ULL;
    if (ast->TYPE_ARRAY.size_expression != nullptr) {
      // TODO: Get compile-time value from it and verify that it is a positive
      // integer.
    }

    Alc_Type array_type = {
      .CARRAY = {
        .length = length,
        .stored_type = stored_type,
      },
      .kind = ALC_TYPE_KIND_CARRAY,
    };
    Alc_Type *out = alc_type_storage_add_type(&program->type_storage, &array_type);
    return out;
  }

  case ALC_AST_KIND_TYPE_FUNCTION_POINTER: {
    Alc_Type *return_type;
    if (ast->TYPE_FUNCTION_POINTER.return_type != nullptr) {
      return_type =
        alc_type_resolve_from_ast(program, sourcefile, ast->TYPE_FUNCTION_POINTER.return_type);
      if ALC_UNLIKELY (alc_type_is_error(return_type))
        return program->type_storage.builtins.type_error;
    } else {
      return_type = program->type_storage.builtins.type_void;
    }

    Alc_Ast *argument_list = ast->TYPE_FUNCTION_POINTER.argument_list;
    b8 has_defaults = false;
    for (usize i = 0; i < argument_list->ARGUMENT_LIST.arguments_num; i++) {
      Alc_Ast *argument = argument_list->ARGUMENT_LIST.arguments[i];
      switch (argument->kind) {
      case ALC_AST_KIND_VAR_DECL: {
        if ALC_UNLIKELY (has_defaults) {
          // TODO: Error: Argument without default value that
          // comes after an argument with default value is not allowed.
          return program->type_storage.builtins.type_error;
        }
        // TODO: Implementation.
      } break;

      case ALC_AST_KIND_VAR_DEF: {
        // TODO: Implementation.
      } break;

      default:
        ALC_NOREACH();
      }
    }

    Alc_Type func_ptr_type = {
      .FUNCTION = {
        // TODO: Fill it when the time comes.
        .is_variadic = argument_list->ARGUMENT_LIST.is_variadic,
      },
      .kind = ALC_TYPE_KIND_FUNCTION,
    };
    Alc_Type *out = alc_type_storage_add_type(&program->type_storage, &func_ptr_type);
    return out;
  }

  case ALC_AST_KIND_TYPE_TUPLE: {
    Alc_Vector(Alc_Type *) types = alc_vector_reserve(Alc_Type *, ast->TYPE_TUPLE.types_num);
    for (usize i = 0; i < ast->TYPE_TUPLE.types_num; i++) {
      Alc_Ast *type_ast = ast->TYPE_TUPLE.types[i];
      Alc_Type *type = alc_type_resolve_from_ast(program, sourcefile, type_ast);
      if ALC_UNLIKELY (alc_type_is_error(type)) {
        alc_vector_destroy(types);
        return program->type_storage.builtins.type_error;
      }

      alc_vector_push(types, type);
    }

    // NOTE: Do not copy vector to the array, as the tuple type may already
    // exist, and array stored in the arena will stay, and we lose memory.
    Alc_Type tuple = {
      .TUPLE = {
        .types = types,
        .types_num = alc_vector_get_length(types),
      },
      .kind = ALC_TYPE_KIND_TUPLE,
    };
    Alc_Type *tuple_duplicate = alc_type_storage_find_duplicate(&program->type_storage, &tuple);
    if (tuple_duplicate != nullptr)
      return tuple_duplicate;

    tuple.TUPLE.types = alc_vector_to_array(types, &tuple.TUPLE.types_num);
    alc_vector_destroy(types);

    Alc_Type *out = alc_type_storage_add_type(&program->type_storage, &tuple);
    return out;
  }

  case ALC_AST_KIND_TYPE_TYPE_OF: {
    ALC_TODO("Resolve 'typeof' type based on provided scope.");
  }

  case ALC_AST_KIND_GENERIC_TYPE: {
    // NOTE: This should be used in instantiation context.
    const char *name = ast->GENERIC_TYPE.name;
    Alc_Type *bound_generic_struct;

    b8 validate_types = true;
    Alc_Ast *generic_type_list = ast->GENERIC_TYPE.generic_type_list;

    Alc_Vector(Alc_Type *) types =
      alc_vector_reserve(Alc_Type *, generic_type_list->GENERIC_TYPE_LIST.generic_types_num);

    bound_generic_struct = alc_type_get_builtin(&program->type_storage, name);
    if (bound_generic_struct != nullptr)
      goto __check_bound_generic_struct;

    bound_generic_struct = alc_source_file_find_type(sourcefile, name);
    if (bound_generic_struct != nullptr)
      goto __check_bound_generic_struct;

    bound_generic_struct = alc_module_find_type(sourcefile->module, name);
    if (bound_generic_struct != nullptr)
      goto __check_bound_generic_struct;

    // TODO: Error: Undefined type.

    bound_generic_struct = program->type_storage.builtins.type_error;
    goto __after_bound_struct;

__check_bound_generic_struct:
    if (bound_generic_struct->kind != ALC_TYPE_KIND_GENERIC_STRUCT) {
      bound_generic_struct = program->type_storage.builtins.type_error;
      validate_types = false;
    }

__after_bound_struct:
    for (usize i = 0; i < generic_type_list->GENERIC_TYPE_LIST.generic_types_num; i++) {
      Alc_Ast *type_ast = generic_type_list->GENERIC_TYPE_LIST.generic_types[i];
      Alc_Type *type = alc_type_resolve_from_ast(program, sourcefile, type_ast);
      if ALC_UNLIKELY (alc_type_is_error(type)) {
        alc_vector_destroy(types);
        return program->type_storage.builtins.type_error;
      }

      if ALC_LIKELY (validate_types) {
        // TODO: validate types.
      }

      alc_vector_push(types, type);
    }

    // NOTE: Do not copy anything for now.
    Alc_Type generic_struct_instance = {
      .GENERIC_STRUCT_INSTANCE = {
        .name = (char*)name,
        .bound_generic_struct = bound_generic_struct,
        .types = types,
        .types_num = alc_vector_get_length(types),
      },
      .kind = ALC_TYPE_KIND_GENERIC_STRUCT_INSTANCE,
    };
    Alc_Type *duplicate =
      alc_type_storage_find_duplicate(&program->type_storage, &generic_struct_instance);
    if (duplicate != nullptr)
      return duplicate;

    usize name_len = strlen(name) + 1;

    generic_struct_instance.GENERIC_STRUCT_INSTANCE.name =
      alc_alloc_arena_allocate_aligned(&ctx()->arena, sizeof(char) * name_len, 1);
    generic_struct_instance.GENERIC_STRUCT_INSTANCE.types =
      alc_vector_to_array(types, &generic_struct_instance.GENERIC_STRUCT_INSTANCE.types_num);
    alc_vector_destroy(types);
    memcpy(generic_struct_instance.GENERIC_STRUCT_INSTANCE.name, name, sizeof(char) * name_len);

    Alc_Type *out = alc_type_storage_add_type(&program->type_storage, &generic_struct_instance);
    return out;
  }

  case ALC_AST_KIND_TYPE_NONNULL: {
    // Alc_Type *pointer_type = alc_type_resolve_from_ast(program, sourcefile, ast->TYPE_NONNULL.type);
    // if ALC_UNLIKELY (alc_type_is_error(pointer_type))
    //   return program->type_storage.builtins.type_error;
    // else if ALC_UNLIKELY (!alc_type_is_pointer(pointer_type)) {
    //   // TODO: Error. Only pointers are allowed to be non-null.
    //   return program->type_storage.builtins.type_error;
    // }
    //
    // // TODO: Nonnull pointer must be contructed here.
    // return pointer_type;
    ALC_TODO("Construct Non-null pointer");
  }

  default:
    ALC_NOREACH();
  }
}

static inline Alc_Type *allocate_type_zero_init(Alc_Type_Storage *storage)
{
  Alc_Type *type = alc_type_storage_allocate_type(storage);
  memset(type, 0, sizeof(Alc_Type));
  return type;
}

static inline const Alc_Type *unwrap_alias(const Alc_Type *t)
{
  while (t->kind == ALC_TYPE_KIND_ALIAS)
    t = t->ALIAS.aliased_type;

  return t;
}

static Alc_Foreach_Result _find_duplicate_fn(usize index, void *value, void *user_data)
{
  ALC_UNUSED_PERMIT(index);

  struct {
    const Alc_Type *type;
    Alc_Type *result;
  } *data = user_data;

  Alc_Type *type = value;

  if (alc_type_is_same(data->type, type)) {
    data->result = type;
    return ALC_FOREACH_BREAK;
  }

  return ALC_FOREACH_CONTINUE;
}
