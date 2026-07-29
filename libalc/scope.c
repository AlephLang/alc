#include "alc/scope.h"
#include "alc/defs.h"
#include "alc/hashtable.h"
#include "alc/vector.h"
#include <string.h>

static void foreach_scope_destroy(usize index, void *value, void *user_data);

Alc_Scope alc_scope_create(Alc_Scope_Kind kind, Alc_Scope *parent, const char *name,
                           Alc_Ast *bound_ast)
{
  ALC_ASSERT(kind == ALC_SCOPE_KIND_GLOBAL || (parent != nullptr && bound_ast != nullptr));
  ALC_ASSERT(kind == ALC_SCOPE_KIND_NAMELESS || kind == ALC_SCOPE_KIND_GLOBAL || name != nullptr);

  return (Alc_Scope){
    .named_scopes = alc_hashtable_create(sizeof(Alc_Scope), false),
    .nameless_scopes = alc_vector_create(Alc_Scope),
    .name = name,
    .bound_ast = bound_ast,
    .parent = parent,
    .kind = kind,
  };
}

void alc_scope_destroy(Alc_Scope *scope)
{
  alc_hashtable_foreach(&scope->named_scopes, foreach_scope_destroy, nullptr);
  alc_hashtable_destroy(&scope->named_scopes);

  for (usize i = 0, nameless_scopes_len = alc_vector_get_length(scope->nameless_scopes);
       i < nameless_scopes_len; i++) {
    alc_scope_destroy(&scope->nameless_scopes[i]);
  }
  alc_vector_destroy(scope->nameless_scopes);

  memset(scope, 0, sizeof(Alc_Scope));
}

Alc_Scope *alc_scope_get_named_scope(Alc_Scope *scope, const char *name)
{
  return alc_hashtable_get(&scope->named_scopes, name);
}

Alc_Scope *alc_scope_get_nameless_scope(Alc_Scope *scope, u64 scope_id)
{
  return &scope->nameless_scopes[scope_id];
}

static void foreach_scope_destroy(usize index, void *value, void *user_data)
{
  ALC_UNUSED_PERMIT(index);
  ALC_UNUSED_PERMIT(user_data);

  alc_scope_destroy(value);
}
