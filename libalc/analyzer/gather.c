#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/hashtable.h"
#include "alc/scope.h"
#include "alc/vector.h"
#include "analyzer/analyzer_private.h"

b8 gather_info(Alc_Analyzer *a, Alc_Ast *ast, Alc_Gathered_Info *info)
{
  Alc_Ast **children;
  usize children_num;

#define _CHILDREN(_ast_kind, _field)            \
  case ALC_AST_KIND_FULL_NAME(_ast_kind):       \
    children = ast->_ast_kind._field;           \
    children_num = ast->_ast_kind._field##_num; \
    break;

  switch (ast->kind) {
    _CHILDREN(ROOT, toplevel_statements);
    _CHILDREN(STRUCT, children);
    _CHILDREN(GENERIC_STRUCT, children);
    _CHILDREN(UNION, children);
    _CHILDREN(STMT_BLOCK, statements);

  default:
    ALC_NOREACH();
  }

  b8 result = true;
  for (usize i = 0; i < children_num; i++) {
    Alc_Ast *child = children[i];
    switch (child->kind) {
    case ALC_AST_KIND_VAR_DECL:
    case ALC_AST_KIND_VAR_DEF:
    case ALC_AST_KIND_EXTERN_VARDECL: {
      if ALC_LIKELY (a->current_scope->kind != ALC_SCOPE_KIND_GLOBAL)
        break;

      alc_vector_push(info->globals, gathered_ast_create(child, a->current_scope));
    } break;

    case ALC_AST_KIND_FUNC:
    case ALC_AST_KIND_GENERIC_FUNC: {
      const char *name = child->kind == ALC_AST_KIND_FUNC ? child->FUNC.name :
                                                            child->GENERIC_FUNC.name;
      if ALC_UNLIKELY (alc_scope_get_named_scope(a->current_scope, name) != nullptr) {
        alc_analyzer_add_error(a, .bound_ast = child, .type = ALC_SEMANTIC_ERROR_REDEFINITION);
        result = false;
        break;
      }

      alc_vector_push(info->functions, gathered_ast_create(child, a->current_scope));
      Alc_Scope new_scope = alc_scope_create(child->kind == ALC_AST_KIND_FUNC ?
                                               ALC_SCOPE_KIND_FUNCTION :
                                               ALC_SCOPE_KIND_GENERIC_FUNCTION,
                                             a->current_scope, name, child);
      alc_hashtable_put(&a->current_scope->named_scopes, name, &new_scope);
      Alc_Scope *current = alc_hashtable_get(&a->current_scope->named_scopes, name);
      Alc_Scope *saved_scope = a->current_scope;
      a->current_scope = current;

      Alc_Ast *continue_ast = child->kind == ALC_AST_KIND_FUNC ? child->FUNC.body :
                                                                 child->GENERIC_FUNC.body;

      if (continue_ast->kind == ALC_AST_KIND_STMT_BLOCK)
        result = result && gather_info(a, continue_ast, info);

      a->current_scope = saved_scope;
    } break;

    case ALC_AST_KIND_EXTERN_FUNC: {
      alc_vector_push(info->functions, gathered_ast_create(child, a->current_scope));
    } break;

    case ALC_AST_KIND_STRUCT:
    case ALC_AST_KIND_GENERIC_STRUCT:
    case ALC_AST_KIND_UNION: {
      const char *name = child->kind == ALC_AST_KIND_STRUCT         ? child->STRUCT.name :
                         child->kind == ALC_AST_KIND_GENERIC_STRUCT ? child->GENERIC_STRUCT.name :
                                                                      child->UNION.name;
      if ALC_UNLIKELY (alc_scope_get_named_scope(a->current_scope, name) != nullptr) {
        alc_analyzer_add_error(a, .bound_ast = child, .type = ALC_SEMANTIC_ERROR_REDEFINITION);
        result = false;
        break;
      }

      alc_vector_push(info->types, gathered_ast_create(child, a->current_scope));
      Alc_Scope new_scope = alc_scope_create(
        child->kind == ALC_AST_KIND_STRUCT         ? ALC_SCOPE_KIND_STRUCT :
        child->kind == ALC_AST_KIND_GENERIC_STRUCT ? ALC_SCOPE_KIND_GENERIC_STRUCT :
                                                     ALC_SCOPE_KIND_UNION,
        a->current_scope, name, child);
      alc_hashtable_put(&a->current_scope->named_scopes, name, &new_scope);
      Alc_Scope *current = alc_hashtable_get(&a->current_scope->named_scopes, name);
      Alc_Scope *saved_scope = a->current_scope;
      a->current_scope = current;

      result = result && gather_info(a, child, info);

      a->current_scope = saved_scope;
    } break;

    case ALC_AST_KIND_ENUM:
    case ALC_AST_KIND_TYPEDEF: {
      alc_vector_push(info->types, gathered_ast_create(child, a->current_scope));
    } break;

    case ALC_AST_KIND_IMPORT: {
      alc_vector_push(info->imports, gathered_ast_create(child, a->current_scope));
    } break;

    case ALC_AST_KIND_STMT_BLOCK: {
      Alc_Scope new_scope =
        alc_scope_create(ALC_SCOPE_KIND_NAMELESS, a->current_scope, nullptr, child);
      alc_vector_push(a->current_scope->nameless_scopes, new_scope);
      Alc_Scope *current =
        &a->current_scope
           ->nameless_scopes[alc_vector_get_length(a->current_scope->nameless_scopes) - 1];
      Alc_Scope *saved_scope = a->current_scope;
      a->current_scope = current;

      result = result && gather_info(a, child, info);

      a->current_scope = saved_scope;
    } break;

    default:
      break;
    }
  }

  return result;
}
