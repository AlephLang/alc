#include "alc/ast.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "containers/vector.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

static Alc_Ast *parse_entry(Alc_Parser *p);
static Alc_Ast *parse_entry_default(Alc_Parser *p);
static Alc_Ast *parse_entry_explicit(Alc_Parser *p);
static Alc_Ast *parse_entry_explicit_array_elem(Alc_Parser *p);

Alc_Ast *parse_initlist(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_LCBRACK);

  usize pos = p->pos++;

  Alc_Ast **entries = vector_create(Alc_Ast *);
  while (p->pos < p->tokens_num && p->tokens[p->pos].type != ALC_TOKEN_TYPE_RCBRACK) {
    Alc_Ast *entry = parse_entry(p);
    _VERIFY_AST(entry, { vector_destroy(entries); });

    _VERIFY_POS(p, p->pos, { vector_destroy(entries); });

    vector_push(entries, entry);

    if (p->tokens[p->pos].type != ALC_TOKEN_TYPE_COMMA)
      break;

    p->pos++;
  }

  _VERIFY_POS(p, p->pos, { vector_destroy(entries); });
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RCBRACK, { vector_destroy(entries); });

  p->pos++;

  Alc_Ast *initlist_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  initlist_ast->data.INITLIST.entries =
    vector_to_array(entries, &initlist_ast->data.INITLIST.entries_num);
  initlist_ast->pos = pos;
  initlist_ast->kind = ALC_AST_KIND_INITLIST;
  vector_destroy(entries);
  return initlist_ast;
}

static Alc_Ast *parse_entry(Alc_Parser *p)
{
  _VERIFY_POS(p, p->pos);
  switch (p->tokens[p->pos].type) {
  case ALC_TOKEN_TYPE_PERIOD:
    return parse_entry_explicit(p);
  case ALC_TOKEN_TYPE_LBRACK:
    return parse_entry_explicit_array_elem(p);
  default:
    return parse_entry_default(p);
  }
}

static Alc_Ast *parse_entry_default(Alc_Parser *p)
{
  _VERIFY_POS(p, p->pos);
  Alc_Ast *expr = p->tokens[p->pos].type == ALC_TOKEN_TYPE_LCBRACK ? parse_initlist(p) :
                                                                     parse_expr(p, false);
  _VERIFY_AST(expr);

  Alc_Ast *entry_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  entry_ast->data.INITLIST_ENTRY.expression = expr;
  entry_ast->pos = expr->pos;
  entry_ast->kind = ALC_AST_KIND_INITLIST_ENTRY;
  return entry_ast;
}

static Alc_Ast *parse_entry_explicit(Alc_Parser *p)
{
  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_PERIOD);
  _VERIFY_NO_WS(p, p->pos, ALC_TOKEN_TYPE_ID);

  p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  usize pos = p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_EQ);

  p->pos++;

  _VERIFY_POS(p, p->pos);
  Alc_Ast *expr = p->tokens[p->pos].type == ALC_TOKEN_TYPE_LCBRACK ? parse_initlist(p) :
                                                                     parse_expr(p, false);
  _VERIFY_AST(expr);

  Alc_Ast *entry_explicit =
    alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + sizeof(char) * name_len);
  entry_explicit->data.INITLIST_ENTRY_EXPLICIT.field_name =
    (char *)entry_explicit + sizeof(Alc_Ast);
  entry_explicit->data.INITLIST_ENTRY_EXPLICIT.expression = expr;
  entry_explicit->pos = pos;
  entry_explicit->kind = ALC_AST_KIND_INITLIST_ENTRY_EXPLICIT;
  memcpy(entry_explicit->data.INITLIST_ENTRY_EXPLICIT.field_name, name, sizeof(char) * name_len);
  return entry_explicit;
}

static Alc_Ast *parse_entry_explicit_array_elem(Alc_Parser *p)
{
  _VERIFY_POS(p, p->pos);

  usize pos = p->pos;

  Alc_Ast **index_expressions = vector_create(Alc_Ast *);
  while (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_LBRACK) {
    p->pos++;

    Alc_Ast *expr = parse_expr(p, false);
    _VERIFY_AST(expr, { vector_destroy(index_expressions); });

    _VERIFY_POS(p, p->pos, { vector_destroy(index_expressions); });
    _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RBRACK, { vector_destroy(index_expressions); });

    vector_push(index_expressions, expr);

    p->pos++;
  }

  _VERIFY_POS(p, p->pos, { vector_destroy(index_expressions); });
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_EQ, { vector_destroy(index_expressions); });

  p->pos++;

  _VERIFY_POS(p, p->pos, { vector_destroy(index_expressions); });
  Alc_Ast *expr = p->tokens[p->pos].type == ALC_TOKEN_TYPE_LCBRACK ? parse_initlist(p) :
                                                                     parse_expr(p, false);
  _VERIFY_AST(expr, { vector_destroy(expr); });

  Alc_Ast *entry_explicit_array_element = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  entry_explicit_array_element->data.INITLIST_ENTRY_EXPLICIT_ARRAY_ELEMENT.index_expressions =
    vector_to_array(index_expressions,
                    &entry_explicit_array_element->data.INITLIST_ENTRY_EXPLICIT_ARRAY_ELEMENT
                       .index_expressions_num);
  entry_explicit_array_element->data.INITLIST_ENTRY_EXPLICIT_ARRAY_ELEMENT.expression = expr;
  entry_explicit_array_element->pos = pos;
  entry_explicit_array_element->kind = ALC_AST_KIND_INITLIST_ENTRY_EXPLICIT_ARRAY_ELEMENT;
  vector_destroy(index_expressions);
  return entry_explicit_array_element;
}
