#include "alc/analyzer.h"
#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/scope.h"
#include "alc/vector.h"
#include "analyzer/analyzer_private.h"
#include <stdio.h>
#include <string.h>

static void print_scope_name(Alc_Scope *scope);
static void print_gathered_ast(Alc_Gathered_Ast *gathered_ast);
static void print_gathered_info(Alc_Gathered_Info *info);

Alc_Analyzer alc_analyzer_create(Alc_Context *context)
{
  return (Alc_Analyzer){
    .context = context,
    .current_scope = &context->global_scope,
    .semantic_errors = alc_vector_create(Alc_Semantic_Error),
  };
}

void alc_analyzer_destroy(Alc_Analyzer *analyzer)
{
  alc_vector_destroy(analyzer->semantic_errors);

  memset(analyzer, 0, sizeof(Alc_Analyzer));
}

b8 alc_analyzer_analyse(Alc_Analyzer *analyzer, Alc_Ast *root)
{
  ALC_ASSERT(root->kind == ALC_AST_KIND_ROOT)

  Alc_Gathered_Info gathered_info = {
    .imports = alc_vector_create(Alc_Gathered_Ast),
    .types = alc_vector_create(Alc_Gathered_Ast),
    .globals = alc_vector_create(Alc_Gathered_Ast),
    .functions = alc_vector_create(Alc_Gathered_Ast),
  };
  if ALC_UNLIKELY (!gather_info(analyzer, root, &gathered_info))
    return false;

  print_gathered_info(&gathered_info);

  alc_vector_destroy(gathered_info.imports);
  alc_vector_destroy(gathered_info.types);
  alc_vector_destroy(gathered_info.globals);
  alc_vector_destroy(gathered_info.functions);

  return true;
}

static void print_scope_name(Alc_Scope *scope)
{
  if (scope->parent != nullptr)
    print_scope_name(scope->parent);

  switch (scope->kind) {
  case ALC_SCOPE_KIND_GLOBAL:
    printf("::");
    break;

  case ALC_SCOPE_KIND_NAMELESS:
    printf("<?>::");
    break;

  default:
    printf("%s::", scope->name);
    break;
  }
}

static void print_gathered_ast(Alc_Gathered_Ast *gathered_ast)
{
  print_scope_name(gathered_ast->scope);
  printf("%p", gathered_ast->ast);
}

static void print_gathered_info(Alc_Gathered_Info *info)
{
  usize imports_num = alc_vector_get_length(info->imports);
  usize types_num = alc_vector_get_length(info->types);
  usize globals_num = alc_vector_get_length(info->globals);
  usize functions_num = alc_vector_get_length(info->functions);

  printf("##### IMPORTS (%zu):\n", imports_num);
  for (usize i = 0; i < imports_num; i++) {
    print_gathered_ast(&info->imports[i]);
    putchar('\n');
  }

  printf("##### TYPES (%zu):\n", types_num);
  for (usize i = 0; i < types_num; i++) {
    print_gathered_ast(&info->types[i]);
    putchar('\n');
  }

  printf("##### GLOBALS (%zu):\n", globals_num);
  for (usize i = 0; i < globals_num; i++) {
    print_gathered_ast(&info->globals[i]);
    putchar('\n');
  }

  printf("##### FUNCTIONS (%zu):\n", functions_num);
  for (usize i = 0; i < functions_num; i++) {
    print_gathered_ast(&info->functions[i]);
    putchar('\n');
  }
}
