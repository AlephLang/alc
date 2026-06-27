#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "containers/vector.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

static Alc_Ast *parse_case_chain(Alc_Parser *p);
static Alc_Ast *parse_case(Alc_Parser *p);
static Alc_Ast *parse_default(Alc_Parser *p);

Alc_Ast *parse_stmt_switch(Alc_Parser *p)
{
  usize pos = p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_LPAREN);

  p->pos++;

  Alc_Ast *expression = parse_expr(p, false);
  _VERIFY_AST(expression);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RPAREN);

  p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_LCBRACK);

  p->pos++;

  Vector(Alc_Ast *) case_chains = vector_create(Alc_Ast *);
  while (p->pos < p->tokens_num && p->tokens[p->pos].type != ALC_TOKEN_TYPE_RCBRACK) {
    Alc_Ast *case_chain = parse_case_chain(p);
    _VERIFY_AST(case_chain, { vector_destroy(case_chains); });
    vector_push(case_chains, case_chain);
  }

  _VERIFY_POS(p, p->pos, { vector_destroy(case_chains); });
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RCBRACK, { vector_destroy(case_chains); });

  p->pos++;

  Alc_Ast *stmt_switch_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  stmt_switch_ast->data.STMT_SWITCH.expression = expression;
  stmt_switch_ast->data.STMT_SWITCH.case_chains =
    vector_to_array(case_chains, &stmt_switch_ast->data.STMT_SWITCH.case_chains_num);
  stmt_switch_ast->pos = pos;
  stmt_switch_ast->kind = ALC_AST_KIND_STMT_SWITCH;
  vector_destroy(case_chains);
  return stmt_switch_ast;
}

static Alc_Ast *parse_case_chain(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);

  usize pos = p->pos;

  Vector(Alc_Ast *) cases = vector_create(Alc_Ast *);
  while (p->pos < p->tokens_num && p->tokens[p->pos].type != ALC_TOKEN_TYPE_LCBRACK) {
    _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID, { vector_destroy(cases); });

    Alc_Ast *case_ast;
    if (strcmp(p->tokens[p->pos].value, "case") == 0)
      case_ast = parse_case(p);
    else if (strcmp(p->tokens[p->pos].value, "default") == 0)
      case_ast = parse_default(p);
    else {
      Vector(const char *) expected_values = vector_reserve(const char *, 2);
      vector_push(expected_values, "case");
      vector_push(expected_values, "default");
      _VERIFY_VALUE_V(p, p->pos, expected_values, { vector_destroy(cases); });
      ALC_NOREACH();
    }

    vector_push(cases, case_ast);
  }

  Alc_Ast *body = parse_stmt_block(p);
  _VERIFY_AST(body, { vector_destroy(cases); });

  Alc_Ast *case_chain_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  case_chain_ast->data.CASE_CHAIN.cases =
    vector_to_array(cases, &case_chain_ast->data.CASE_CHAIN.cases_num);
  case_chain_ast->data.CASE_CHAIN.body = body;
  case_chain_ast->pos = pos;
  case_chain_ast->kind = ALC_AST_KIND_CASE_CHAIN;
  vector_destroy(cases);
  return case_chain_ast;
}

static Alc_Ast *parse_case(Alc_Parser *p)
{
  usize pos = p->pos++;

  Alc_Ast *expression = parse_expr(p, false);
  _VERIFY_AST(expression);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COLON);

  p->pos++;

  Alc_Ast *case_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  case_ast->data.CASE.expression = expression;
  case_ast->pos = pos;
  case_ast->kind = ALC_AST_KIND_CASE;
  return case_ast;
}

static Alc_Ast *parse_default(Alc_Parser *p)
{
  usize pos = p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COLON);

  p->pos++;

  Alc_Ast *default_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  default_ast->pos = pos;
  default_ast->kind = ALC_AST_KIND_DEFAULT;
  return default_ast;
}
