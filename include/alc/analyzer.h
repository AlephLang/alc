#ifndef __ALC_ANALYZER_H__
#define __ALC_ANALYZER_H__

#include <alc/vector.h>
#include <alc/module.h>
#include <alc/entry.h>
#include <alc/ast.h>

typedef struct __Alc_Program Alc_Program;

typedef struct {
  Alc_Source_File *sourcefile;

  union {
    struct {
      const char *module;
      Alc_Ast *ast;
    } UNRESOLVABLE_IMPORT;
    struct {
      const char *name;
      Alc_Ast *ast;
      struct {
        Alc_Source_File *sourcefile;
        Alc_Ast *ast;
      } where_defined;
    } TYPE_REDEF;
    struct {
      const char *name;
      Alc_Ast *ast;
      struct {
        Alc_Source_File *sourcefile;
        Alc_Ast *ast;
      } where_declared;
    } VARIABLE_REDECL;
    struct {
      const char *name;
      Alc_Ast *ast;
      struct {
        Alc_Source_File *sourcefile;
        Alc_Ast *ast;
      } where_defined;
    } FUNCTION_REDEF;
  };

  enum {
    ALC_ANALYSIS_ERROR_UNRESOLVABLE_IMPORT,
    ALC_ANALYSIS_ERROR_TYPE_REDEF,
    ALC_ANALYSIS_ERROR_VARIABLE_REDECL,
    ALC_ANALYSIS_ERROR_FUNCTION_REDEF,
  } kind;
} Alc_Analysis_Error;

ALC_API b8 alc_analyzer_validate_and_emplace_import_entries(Alc_Program *program,
                                                            Alc_Vector(Alc_Entry) entries);
ALC_API b8 alc_analyzer_validate_and_emplace_type_entries(Alc_Program *program,
                                                          Alc_Vector(Alc_Entry) entries);
ALC_API b8 alc_analyzer_validate_and_emplace_global_entries(Alc_Program *program,
                                                            Alc_Vector(Alc_Entry) entries);
ALC_API b8 alc_analyzer_validate_and_emplace_function_entries(Alc_Program *program,
                                                              Alc_Vector(Alc_Entry) entries);

#endif // __ALC_ANALYZER_H__
