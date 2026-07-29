#ifndef __ALC_SCOPE_H__
#define __ALC_SCOPE_H__

#include <alc/defs.h>
#include <alc/vector.h>
#include <alc/hashtable.h>
#include <alc/ast.h>

typedef enum {
  ALC_SCOPE_KIND_NAMELESS,
  ALC_SCOPE_KIND_FUNCTION,
  ALC_SCOPE_KIND_STRUCT,
  ALC_SCOPE_KIND_UNION,
  ALC_SCOPE_KIND_GENERIC_FUNCTION,
  ALC_SCOPE_KIND_GENERIC_STRUCT,
  ALC_SCOPE_KIND_GLOBAL,
} Alc_Scope_Kind;

typedef struct __Alc_Scope {
  Alc_Hashtable named_scopes;
  Alc_Vector(struct __Alc_Scope) nameless_scopes;

  const char *name;
  Alc_Ast *bound_ast;

  struct __Alc_Scope *parent;

  Alc_Scope_Kind kind;
} Alc_Scope;

ALC_API Alc_Scope alc_scope_create(Alc_Scope_Kind kind, Alc_Scope *parent, const char *name,
                                   Alc_Ast *bound_ast);
ALC_API void alc_scope_destroy(Alc_Scope *scope);

ALC_API Alc_Scope *alc_scope_get_named_scope(Alc_Scope *scope, const char *name);
ALC_API Alc_Scope *alc_scope_get_nameless_scope(Alc_Scope *scope, u64 scope_id);

#endif //  __ALC_SCOPE_H__
