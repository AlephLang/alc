#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "alc/vector.h"
#include "alc/alloc_arena.h"
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

    // There's no real need to initialize it to nullptr but Mr. Compiler said that he wouldn't
    // compile library if I don't initialize it.
    Alc_Ast *qualifier_ast = nullptr;
    for (usize i = 0, names_len = alc_vector_get_length(names); i < names_len; i++) {
      usize name_len = strlen(names[names_len - i - 1]) + 1;
      qualifier_ast =
        alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + sizeof(char) * name_len);
      qualifier_ast->QUALIFIER.name = (char *)qualifier_ast + sizeof(Alc_Ast);
      qualifier_ast->QUALIFIER.qualified = qualified;
      qualifier_ast->pos = --last_pos;
      qualifier_ast->kind = ALC_AST_KIND_QUALIFIER;
      memcpy(qualifier_ast->QUALIFIER.name, names[i], sizeof(char) * name_len);
      qualified = qualifier_ast;
    }
    alc_vector_destroy(names);

    return qualifier_ast;
  }

  // TODO: rewrite this, it looks awful

  Alc_Token *tok2 = peek(p, 1);
  if ALC_UNLIKELY (tok2 == nullptr) {
    p->pos++;
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if (tok2->type == ALC_TOKEN_TYPE_EQ && !tok2->has_whitespace_after) {
    Alc_Token *tok3 = peek(p, 2);
    if (tok3 != nullptr && tok3->type == ALC_TOKEN_TYPE_RARROW)
      return parse_function_alias(p, attribute_list);
  } else if (tok2->type == ALC_TOKEN_TYPE_LPAREN) {
    return parse_function(p, attribute_list, ALC_AST_FUNCTION_KIND_DEFAULT);
  } else if (tok2->type == ALC_TOKEN_TYPE_COLON) {
    Alc_Token *tok3 = peek(p, 2);
    if (tok3 != nullptr && !tok2->has_whitespace_after && tok3->type == ALC_TOKEN_TYPE_COLON)
      return parse_function(p, attribute_list, ALC_AST_FUNCTION_KIND_DEFAULT);
  }

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

  usize pos = p->pos;

  Alc_Vector(Alc_Ast *) names_v = alc_vector_create(Alc_Ast *);
  while (p->pos < p->tokens_num) {
    Alc_Ast *name_ast = parse_name(p);
    _VERIFY_AST(name_ast, { alc_vector_destroy(names_v); });

    alc_vector_push(names_v, name_ast);

    if (p->pos >= p->tokens_num || p->tokens[p->pos].type != ALC_TOKEN_TYPE_COMMA)
      break;

    _VERIFY_POS(p, p->pos, { alc_vector_destroy(names_v); });
    _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COMMA, { alc_vector_destroy(names_v); });

    p->pos++;
  }

  _VERIFY_POS(p, p->pos, { alc_vector_destroy(names_v); });
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COLON, { alc_vector_destroy(names_v); });

  Alc_Ast *type = nullptr;
  if (!p->tokens[p->pos].has_whitespace_after && p->pos + 1 < p->tokens_num &&
      p->tokens[p->pos + 1].type == ALC_TOKEN_TYPE_EQ) {
    p->pos += 2;

    goto __vardef;
  }

  p->pos++;

  type = parse_type(p);
  _VERIFY_AST(type, { alc_vector_destroy(names_v); });

  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_EQ) {
    p->pos++;

__vardef:
    _VERIFY_POS(p, p->pos);
    Alc_Ast *expr;
    switch (p->tokens[p->pos].type) {
    case ALC_TOKEN_TYPE_LCBRACK: {
      expr = parse_initlist(p);
    } break;

    case ALC_TOKEN_TYPE_LBRACK: {
      expr = parse_lambda(p);
    } break;

    default: {
      expr = parse_expr(p, false);
    } break;
    }
    _VERIFY_AST(expr, { alc_vector_destroy(names_v); });

    Alc_Ast *vardef_ast = alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
    vardef_ast->VAR_DEF.names = alc_vector_to_array(names_v, &vardef_ast->VAR_DEF.names_num);
    vardef_ast->VAR_DEF.type = type;
    vardef_ast->VAR_DEF.expression = expr;
    vardef_ast->VAR_DEF.attribute_list = attribute_list;
    vardef_ast->pos = pos;
    vardef_ast->kind = ALC_AST_KIND_VAR_DEF;
    return vardef_ast;
  }

  Alc_Ast *vardecl_ast = alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  vardecl_ast->VAR_DECL.names = alc_vector_to_array(names_v, &vardecl_ast->VAR_DECL.names_num);
  vardecl_ast->VAR_DECL.type = type;
  vardecl_ast->VAR_DECL.attribute_list = attribute_list;
  vardecl_ast->pos = pos;
  vardecl_ast->kind = ALC_AST_KIND_VAR_DECL;
  return vardecl_ast;
}
