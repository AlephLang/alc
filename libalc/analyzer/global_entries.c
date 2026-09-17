#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/analyzer.h"
#include "alc/entry.h"
#include "alc/global_variable.h"
#include "alc/module.h"
#include "alc/sourcefile.h"
#include "alc/type.h"
#include "alc/variable.h"
#include "alc/vector.h"

b8 alc_analyzer_validate_and_emplace_global_entries(Alc_Program *program,
                                                    Alc_Vector(Alc_Entry) entries)
{
  ALC_UNUSED_DEBUG(program);
  ALC_UNUSED_DEBUG(entries);

  b8 result = true;

  for (usize i = 0, entries_len = alc_vector_get_length(entries); i < entries_len; i++) {
    Alc_Entry *entry = &entries[i];

    Alc_Ast *ast = entry->ast;

    const char *name;
    Alc_Ast *type_ast;
    switch (ast->kind) {
    case ALC_AST_KIND_VAR_DECL: {
      name = ast->VAR_DECL.name;
      type_ast = ast->VAR_DECL.type;
    } break;

    case ALC_AST_KIND_VAR_DEF: {
      name = ast->VAR_DEF.name;
      type_ast = ast->VAR_DEF.type;
    } break;

    case ALC_AST_KIND_EXTERN_VARDECL: {
      name = ast->EXTERN_VARDECL.name;
      type_ast = ast->EXTERN_VARDECL.type;
    } break;

    default:
      ALC_NOREACH();
    }

    // TODO: Maybe also test for functions
    Alc_Global_Variable *found_gvar = alc_module_find_global(entry->file->module, name);
    if ALC_LIKELY (found_gvar == nullptr)
      found_gvar = alc_source_file_find_global(entry->file, name);

    if ALC_UNLIKELY (found_gvar != nullptr) {
      fprintf(stderr, "Variable '%s' was already declared\n", name);
      // TODO: Error: Redeclaration of the variable.
      result = false;
      continue;
    }

    Alc_Type *type = nullptr;
    if (type_ast != nullptr) {
      type = alc_type_resolve_from_ast(program, entry->file, type_ast);
      if ALC_UNLIKELY (alc_type_is_error(type))
        result = false;
    }

    Alc_Variable *var = ast->kind == ALC_AST_KIND_EXTERN_VARDECL ?
                          alc_variable_extern_create(name, type) :
                          alc_variable_create(name, type);
    Alc_Global_Variable gvar = { .var_data = var, .scope = entry->scope };
    alc_source_file_put_global(entry->file, &gvar, name);
  }

  return result;
}
