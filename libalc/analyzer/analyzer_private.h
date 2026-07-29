#ifndef __ALC_ANALYZER_PRIVATE_H__
#define __ALC_ANALYZER_PRIVATE_H__

#include "alc/analyzer.h"
#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/scope.h"
#include "alc/vector.h"

typedef struct {
  Alc_Ast *ast;
  Alc_Scope *scope;
} Alc_Gathered_Ast;

static inline Alc_Gathered_Ast gathered_ast_create(Alc_Ast *ast, Alc_Scope *scope)
{
  return (Alc_Gathered_Ast){
    .ast = ast,
    .scope = scope,
  };
}

typedef struct {
  Alc_Vector(Alc_Gathered_Ast) imports;
  Alc_Vector(Alc_Gathered_Ast) types;
  Alc_Vector(Alc_Gathered_Ast) globals;
  Alc_Vector(Alc_Gathered_Ast) functions;
} Alc_Gathered_Info;

#define alc_analyzer_add_error(_analyzer, ...)                            \
  {                                                                       \
    Alc_Semantic_Error __alc__semantic_error = { __VA_ARGS__ };           \
    alc_vector_push((_analyzer)->semantic_errors, __alc__semantic_error); \
  }

b8 gather_info(Alc_Analyzer *a, Alc_Ast *ast, Alc_Gathered_Info *info);

#endif // __ALC_ANALYZER_PRIVATE_H__
