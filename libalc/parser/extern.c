#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "alc/alloc_arena.h"
#include "alc/vector.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

static Alc_Ast *extern_variable(Alc_Parser *p, usize pos);
static Alc_Ast *extern_function(Alc_Parser *p, usize pos);

Alc_Ast *parse_extern(Alc_Parser *p)
{
  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);
  _VERIFY_VALUE(p, p->pos, "extern");

  usize pos = p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  // Check what token goes after the ID.

  _VERIFY_POS(p, p->pos + 1);
  if (p->tokens[p->pos + 1].type == ALC_TOKEN_TYPE_COMMA) {
    return extern_variable(p, pos);
  }

  _VERIFY_TOKEN(p, p->pos + 1, ALC_TOKEN_TYPE_COLON);
  if (p->pos + 2 < p->tokens_num && p->tokens[p->pos + 2].type == ALC_TOKEN_TYPE_COLON)
    return extern_function(p, pos);

  return extern_variable(p, pos);
}

static Alc_Ast *extern_variable(Alc_Parser *p, usize pos)
{
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

  p->pos++;

  Alc_Ast *type = parse_type(p);
  _VERIFY_AST(type, { alc_vector_destroy(names_v); });

  _VERIFY_POS(p, p->pos, { alc_vector_destroy(names_v); });
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_SEMICOLON, { alc_vector_destroy(names_v); });

  p->pos++;

  Alc_Ast *extern_variable_ast = alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  extern_variable_ast->EXTERN_VARDECL.names =
    alc_vector_to_array(names_v, &extern_variable_ast->EXTERN_VARDECL.names_num);
  extern_variable_ast->EXTERN_VARDECL.type = type;
  extern_variable_ast->pos = pos;
  extern_variable_ast->kind = ALC_AST_KIND_EXTERN_VARDECL;
  alc_vector_destroy(names_v);

  return extern_variable_ast;
}

static Alc_Ast *extern_function(Alc_Parser *p, usize pos)
{
  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;
  pos = p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COLON);
  _VERIFY_NO_WS(p, p->pos, ALC_TOKEN_TYPE_COLON);

  p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COLON);

  p->pos++;

  Alc_Ast *arguments = parse_function_arguments(p);
  _VERIFY_AST(arguments);

  Alc_Ast *return_type = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_MINUS) {
    _VERIFY_NO_WS(p, p->pos, ALC_TOKEN_TYPE_RARROW);

    p->pos++;

    _VERIFY_POS(p, p->pos);
    _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RARROW);

    p->pos++;

    return_type = parse_type(p);
    _VERIFY_AST(return_type);
  }

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_SEMICOLON);

  p->pos++;

  Alc_Ast *extern_function =
    alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + (sizeof(char) * name_len));
  extern_function->EXTERN_FUNC.name = (char *)extern_function + sizeof(Alc_Ast);
  extern_function->EXTERN_FUNC.argument_list = arguments;
  extern_function->EXTERN_FUNC.return_type = return_type;
  extern_function->pos = pos;
  extern_function->kind = ALC_AST_KIND_EXTERN_FUNC;
  memcpy(extern_function->EXTERN_FUNC.name, name, sizeof(char) * name_len);

  return extern_function;
}

/*
static Alc_Ast *__var(Alc_Parser *p, usize pos, const char *name, usize name_len);
static Alc_Ast *__function(Alc_Parser *p, usize pos, const char *name, usize name_len);

Alc_Ast *parse_extern(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  usize pos = p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COLON);

  return !p->tokens[p->pos].has_whitespace_after && p->pos + 1 < p->tokens_num &&
             p->tokens[p->pos].type == ALC_TOKEN_TYPE_COLON ?
           __function(p, pos, name, name_len) :
           __var(p, pos, name, name_len);
}

static Alc_Ast *__var(Alc_Parser *p, usize pos, const char *name, usize name_len)
{
  p->pos++;

  Alc_Ast *var_type = parse_type(p);
  _VERIFY_AST(var_type);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_SEMICOLON);

  p->pos++;

  Alc_Ast *extern_vardecl =
    alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + sizeof(char) * name_len);
  extern_vardecl->EXTERN_VARDECL.name = (char *)extern_vardecl + sizeof(Alc_Ast);
  extern_vardecl->EXTERN_VARDECL.type = var_type;
  extern_vardecl->pos = pos;
  extern_vardecl->kind = ALC_AST_KIND_EXTERN_VARDECL;
  memcpy(extern_vardecl->EXTERN_VARDECL.name, name, name_len);
  return extern_vardecl;
}

static Alc_Ast *__function(Alc_Parser *p, usize pos, const char *name, usize name_len)
{
  p->pos += 2;

  Alc_Ast *argument_list = parse_function_arguments(p);
  _VERIFY_AST(argument_list);

  Alc_Ast *return_type = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_MINUS) {
    _VERIFY_NO_WS(p, p->pos, ALC_TOKEN_TYPE_RARROW);

    p->pos++;

    _VERIFY_POS(p, p->pos);
    _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RARROW);

    p->pos++;

    return_type = parse_type(p);
    _VERIFY_AST(return_type);
  }

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_SEMICOLON);

  p->pos++;

  Alc_Ast *extern_func =
    alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + sizeof(char) * name_len);
  extern_func->EXTERN_FUNC.name = (char *)extern_func + sizeof(Alc_Ast);
  extern_func->EXTERN_FUNC.argument_list = argument_list;
  extern_func->EXTERN_FUNC.return_type = return_type;
  extern_func->pos = pos;
  extern_func->kind = ALC_AST_KIND_EXTERN_FUNC;
  memcpy(extern_func->EXTERN_FUNC.name, name, name_len);
  return extern_func;
}
*/
