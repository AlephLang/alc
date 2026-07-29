#ifndef __ALC_ANALYZER_H__
#define __ALC_ANALYZER_H__

#include <alc/scope.h>
#include <alc/context.h>
#include <alc/defs.h>
#include <alc/ast.h>
#include <alc/vector.h>

typedef struct {
  Alc_Ast *bound_ast;
  enum {
    ALC_SEMANTIC_ERROR_REDEFINITION,
  } type;
} Alc_Semantic_Error;

typedef struct {
  Alc_Context *context;
  Alc_Scope *current_scope;

  Alc_Vector(Alc_Semantic_Error) semantic_errors;
} Alc_Analyzer;

ALC_API Alc_Analyzer alc_analyzer_create(Alc_Context *context);
ALC_API void alc_analyzer_destroy(Alc_Analyzer *analyzer);

ALC_API b8 alc_analyzer_analyse(Alc_Analyzer *analyzer, Alc_Ast *root);

#endif // __ALC_ANALYZER_H__
