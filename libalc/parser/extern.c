#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

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
    alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + sizeof(char) * name_len);
  extern_vardecl->data.EXTERN_VARDECL.name = (char *)extern_vardecl + sizeof(Alc_Ast);
  extern_vardecl->data.EXTERN_VARDECL.type = var_type;
  extern_vardecl->pos = pos;
  extern_vardecl->kind = ALC_AST_KIND_EXTERN_VARDECL;
  memcpy(extern_vardecl->data.EXTERN_VARDECL.name, name, name_len);
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
    alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + sizeof(char) * name_len);
  extern_func->data.EXTERN_FUNC.name = (char *)extern_func + sizeof(Alc_Ast);
  extern_func->data.EXTERN_FUNC.argument_list = argument_list;
  extern_func->data.EXTERN_FUNC.return_type = return_type;
  extern_func->pos = pos;
  extern_func->kind = ALC_AST_KIND_EXTERN_FUNC;
  memcpy(extern_func->data.EXTERN_FUNC.name, name, name_len);
  return extern_func;
}
