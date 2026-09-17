#include "alc/entry.h"
#include "alc/program.h"
#include "alc/sourcefile.h"
#include "alc/type.h"
#include "alc/vector.h"
#include "alc/analyzer.h"

b8 alc_analyzer_validate_and_emplace_type_entries(Alc_Program *program,
                                                  Alc_Vector(Alc_Entry) entries)
{
  b8 result = true;
  for (usize i = 0, entries_num = alc_vector_get_length(entries); i < entries_num; i++) {
    Alc_Entry *entry = &entries[i];
    const char *name;
    Alc_Type_Kind type_kind;
    switch (entry->ast->kind) {
    case ALC_AST_KIND_STRUCT: {
      name = entry->ast->STRUCT.name;
      type_kind = ALC_TYPE_KIND_STRUCT;
    } break;

    case ALC_AST_KIND_GENERIC_STRUCT: {
      name = entry->ast->GENERIC_STRUCT.name;
      type_kind = ALC_TYPE_KIND_GENERIC_STRUCT;
    } break;

    case ALC_AST_KIND_UNION: {
      name = entry->ast->UNION.name;
      type_kind = ALC_TYPE_KIND_UNION;
    } break;

    case ALC_AST_KIND_ENUM: {
      name = entry->ast->ENUM.name;
      type_kind = ALC_TYPE_KIND_ENUM;
    } break;

    case ALC_AST_KIND_TYPEDEF: {
      name = entry->ast->TYPEDEF.name;
      type_kind = ALC_TYPE_KIND_ALIAS;
    } break;

    default:
      ALC_NOREACH();
    }

    Alc_Type *found_type = alc_type_get_builtin(&program->type_storage, name);
    b8 is_builtin = true;
    if ALC_LIKELY (found_type == nullptr) {
      found_type = alc_module_find_type(entry->file->module, name);
      is_builtin = false;
    }

    if ALC_LIKELY (found_type == nullptr)
      found_type = alc_source_file_find_type(entry->file, name);

    if ALC_UNLIKELY (found_type != nullptr) {
      Alc_Source_File *defined_in_file = is_builtin ? nullptr : found_type->source_file;
      Alc_Ast *defined_in_ast = is_builtin ? nullptr : found_type->bound_ast;

      alc_program_add_error(program,
                            {
                              .ANALYSIS.error_data = {
                                .sourcefile = entry->file,
                                .TYPE_REDEF = {
                                  .name = name,
                                  .ast = entry->ast,
                                  .where_defined = {
                                    .sourcefile = defined_in_file,
                                    .ast = defined_in_ast,
                                  },
                                },
                                .kind = ALC_ANALYSIS_ERROR_TYPE_REDEF,
                              },
                              .kind = ALC_ERROR_KIND_ANALYSIS,
                            });

      result = false;
      continue;
    }

    Alc_Type *allocated_type = alc_type_storage_allocate_type(&program->type_storage);
    allocated_type->scope = entry->scope;
    allocated_type->bound_ast = entry->ast;
    allocated_type->source_file = entry->file;
    allocated_type->kind = type_kind;
    alc_source_file_put_type(entry->file, allocated_type, name);

    entry->data = allocated_type;
  }
  return result;
}
