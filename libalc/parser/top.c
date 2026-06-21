#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"

Alc_Ast *parse_top(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);

  switch (p->tokens[p->pos].type) {
  case ALC_TOKEN_TYPE_SEMICOLON: {
    Alc_Ast *none_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
    none_ast->pos = p->pos++;
    none_ast->kind = ALC_AST_KIND_NONE;
    return none_ast;
  }

  case ALC_TOKEN_TYPE_AT: {
    return parse_stmt_label(p);
  }

  case ALC_TOKEN_TYPE_ID: {
    Alc_Ast *ids = parse_ids(p);
    return ids == (void *)-1 ? parse_decldef(p, nullptr) : ids;
  }

  case ALC_TOKEN_TYPE_LBRACK: {
    Alc_Ast *attribute_list = parse_attribute_list(p);
    _VERIFY_AST(attribute_list);

    return parse_decldef(p, attribute_list);
  }

  default:
    add_error_unexpected_token_v(p, p->pos++, nullptr);
    return nullptr;
  }
}
