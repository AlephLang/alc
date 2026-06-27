#include "alc/ast.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "containers/vector.h"
#include "global.h"
#include "parser/parser_private.h"

Alc_Ast *parse_stmt_block(Alc_Parser *p)
{
  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_LCBRACK);

  usize pos = p->pos++;

  Alc_Ast **statements = vector_create(Alc_Ast *);
  while (p->pos < p->tokens_num && p->tokens[p->pos].type != ALC_TOKEN_TYPE_RCBRACK) {
    Alc_Ast *statement = parse_stmt(p);
    _VERIFY_AST(statement, { vector_destroy(statements); });

    vector_push(statements, statement);
  }

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RCBRACK);

  p->pos++;

  Alc_Ast *stmt_block = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  stmt_block->data.STMT_BLOCK.statements =
    vector_to_array(statements, &stmt_block->data.STMT_BLOCK.statements_num);
  stmt_block->pos = pos;
  stmt_block->kind = ALC_AST_KIND_STMT_BLOCK;
  vector_destroy(statements);
  return stmt_block;
}

Alc_Ast *parse_stmt(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);
  switch (p->tokens[p->pos].type) {
  case ALC_TOKEN_TYPE_SEMICOLON: {
    usize pos = p->pos++;
    Alc_Ast *none_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
    none_ast->pos = pos;
    none_ast->kind = ALC_AST_KIND_NONE;
    return none_ast;
  }

  case ALC_TOKEN_TYPE_LBRACK: {
    Alc_Ast *attribute_list = parse_attribute_list(p);
    _VERIFY_AST(attribute_list);

    return parse_decldef(p, attribute_list);
  }

  case ALC_TOKEN_TYPE_AT: {
    return parse_stmt_label(p);
  }

  case ALC_TOKEN_TYPE_LCBRACK: {
    return parse_stmt_block(p);
  }

  case ALC_TOKEN_TYPE_ID: {
    Alc_Ast *stmt = parse_ids(p);
    if (stmt != (void *)-1)
      return stmt;

    if (is_qualifier(p->tokens[p->pos].value)) {
      return parse_decldef(p, nullptr);
    }

    if (p->pos + 1 < p->tokens_num && p->tokens[p->pos + 1].type == ALC_TOKEN_TYPE_COLON) {
      if (p->pos + 2 < p->tokens_num && p->tokens[p->pos + 2].type == ALC_TOKEN_TYPE_COLON) {
        if ((p->pos + 3 < p->tokens_num && p->tokens[p->pos + 3].type == ALC_TOKEN_TYPE_LPAREN) ||
            (p->pos + 3 < p->tokens_num && p->tokens[p->pos + 3].type == ALC_TOKEN_TYPE_LARROW))
          return parse_function(p, nullptr, ALC_AST_FUNCTION_KIND_DEFAULT);
        else
          return parse_stmt_expr(p);
      }
      Alc_Ast *decldef_var = parse_decldef_var(p, nullptr);
      _VERIFY_AST(decldef_var);
      _VERIFY_POS(p, p->pos);
      _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_SEMICOLON);
      p->pos++;
      return decldef_var;
    }
  } break;

  default:
    break;
  }

  return parse_stmt_expr(p);
}
