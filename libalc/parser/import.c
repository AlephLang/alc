#include "alc/ast.h"
#include "alc/token.h"
#include "alc/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

Alc_Ast *parse_import(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);
  Alc_Ast *package_or_module = p->pos + 1 < p->tokens_num &&
                                   p->tokens[p->pos + 1].type == ALC_TOKEN_TYPE_COLON ?
                                 parse_package(p) :
                                 parse_module(p);
  _VERIFY_AST(package_or_module);

  const char *import_as = nullptr;
  usize import_as_len = 0;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_ID) {
    _VERIFY_VALUE(p, p->pos, "as");
    p->pos++;

    _VERIFY_POS(p, p->pos);
    _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);
    import_as = p->tokens[p->pos].value;

    p->pos++;

    import_as_len = strlen(import_as) + 1;
  }

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_SEMICOLON);

  p->pos++;

  usize import_ast_size = sizeof(Alc_Ast) + (sizeof(char) * import_as_len);
  Alc_Ast *import_ast = alc_alloc_arena_allocate(&ctx()->arena, import_ast_size);
  import_ast->IMPORT.package_or_module = package_or_module;
  import_ast->IMPORT.import_as = import_as != nullptr ? (char *)import_ast + sizeof(Alc_Ast) :
                                                        nullptr;
  import_ast->pos = pos;
  import_ast->kind = ALC_AST_KIND_IMPORT;

  if (import_as != nullptr)
    memcpy(import_ast->IMPORT.import_as, import_as, sizeof(char) * import_as_len);

  return import_ast;
}
