#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "alc/vector.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

Alc_Ast *parse_decldef(Alc_Parser *p, Alc_Ast *attribute_list)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  if (is_qualifier(p->tokens[p->pos].value)) {
    // 2 qualifier names are the maximum you can really have.
    // If you have more, you probably did something wrong.
    Alc_Vector(const char *) names = alc_vector_reserve(const char *, 2);

    while (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_ID &&
           is_qualifier(p->tokens[p->pos].value)) {
      alc_vector_push(names, p->tokens[p->pos].value);
      p->pos++;
    }

    usize last_pos = p->pos;

    Alc_Ast *qualified = parse_decldef(p, attribute_list);
    _VERIFY_AST(qualified, { alc_vector_destroy(names); });

    Alc_Ast *qualifier_ast;
    for (sptr i = alc_vector_get_length(names) - 1; i >= 0; i--) {
      usize name_len = strlen(names[i]) + 1;
      qualifier_ast =
        alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + sizeof(char) * name_len);
      qualifier_ast->data.QUALIFIER.name = (char *)qualifier_ast + sizeof(Alc_Ast);
      qualifier_ast->data.QUALIFIER.qualified = qualified;
      qualifier_ast->pos = --last_pos;
      qualifier_ast->kind = ALC_AST_KIND_QUALIFIER;
      memcpy(qualifier_ast->data.QUALIFIER.name, names[i], sizeof(char) * name_len);
      qualified = qualifier_ast;
    }
    alc_vector_destroy(names);

    return qualifier_ast;
  } else if (strcmp(p->tokens[p->pos].value, "func") == 0) {
    p->pos++;
    return parse_function(p, attribute_list, ALC_AST_FUNCTION_KIND_EXPLICIT);
  }

  Alc_Token *tok2 = peek(p, 1);
  if ALC_UNLIKELY (tok2 == nullptr) {
    p->pos++;
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (tok2->type != ALC_TOKEN_TYPE_COLON) {
    p->pos++;
    add_error_unexpected_token(p, p->pos++, ALC_TOKEN_TYPE_COLON);
    return nullptr;
  }

  Alc_Token *tok3 = peek(p, 2);
  if (tok3 != nullptr && !tok2->has_whitespace_after && tok3->type == ALC_TOKEN_TYPE_COLON)
    return parse_function(p, attribute_list, ALC_AST_FUNCTION_KIND_DEFAULT);

  Alc_Ast *decldef = parse_decldef_var(p, attribute_list);
  _VERIFY_AST(decldef);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_SEMICOLON);

  p->pos++;

  return decldef;
}

Alc_Ast *parse_decldef_var(Alc_Parser *p, Alc_Ast *attribute_list)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  usize pos = p->pos;

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COLON);

  Alc_Ast *type = nullptr;
  if (!p->tokens[p->pos].has_whitespace_after && p->pos + 1 < p->tokens_num &&
      p->tokens[p->pos + 1].type == ALC_TOKEN_TYPE_EQ) {
    p->pos += 2;

    goto __vardef;
  }

  p->pos++;

  type = parse_type(p);
  _VERIFY_AST(type);

  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_EQ) {
    p->pos++;

__vardef:
    _VERIFY_POS(p, p->pos);
    Alc_Ast *expr = p->tokens[p->pos].type == ALC_TOKEN_TYPE_LCBRACK ? parse_initlist(p) :
                                                                       parse_expr(p, false);
    _VERIFY_AST(expr);

    Alc_Ast *vardef_ast =
      alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + sizeof(char) * name_len);
    vardef_ast->data.VAR_DEF.name = (char *)vardef_ast + sizeof(Alc_Ast);
    vardef_ast->data.VAR_DEF.type = type;
    vardef_ast->data.VAR_DEF.expression = expr;
    vardef_ast->data.VAR_DEF.attribute_list = attribute_list;
    vardef_ast->pos = pos;
    vardef_ast->kind = ALC_AST_KIND_VAR_DEF;
    memcpy(vardef_ast->data.VAR_DEF.name, name, name_len);
    return vardef_ast;
  }

  Alc_Ast *vardecl_ast =
    alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + sizeof(char) * name_len);
  vardecl_ast->data.VAR_DECL.name = (char *)vardecl_ast + sizeof(Alc_Ast);
  vardecl_ast->data.VAR_DECL.type = type;
  vardecl_ast->data.VAR_DECL.attribute_list = attribute_list;
  vardecl_ast->pos = pos;
  vardecl_ast->kind = ALC_AST_KIND_VAR_DECL;
  memcpy(vardecl_ast->data.VAR_DECL.name, name, name_len);
  return vardecl_ast;
}
