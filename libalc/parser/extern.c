#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

static alc_ast_t *__var(alc_parser_t *p, usize pos, const char *name, usize name_len);
static alc_ast_t *__function(alc_parser_t *p, usize pos, const char *name, usize name_len);

alc_ast_t *parse_extern(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  p->pos++;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_ID) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_ID);
    return nullptr;
  }

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  usize pos = p->pos++;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_COLON) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_COLON);
    return nullptr;
  }

  return !p->tokens[p->pos].has_whitespace_after && p->pos + 1 < p->tokens_num &&
             p->tokens[p->pos].type == ALC_TOKEN_TYPE_COLON ?
           __function(p, pos, name, name_len) :
           __var(p, pos, name, name_len);
}

static alc_ast_t *__var(alc_parser_t *p, usize pos, const char *name, usize name_len)
{
  p->pos++;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  alc_ast_t *var_type = parse_type(p);
  if ALC_UNLIKELY (var_type == nullptr)
    return nullptr;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_SEMICOLON) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_SEMICOLON);
    return nullptr;
  }

  p->pos++;

  alc_ast_t *extern_vardecl =
    alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + sizeof(char) * name_len);
  extern_vardecl->data.EXTERN_VARDECL.name = (char *)extern_vardecl + sizeof(alc_ast_t);
  extern_vardecl->data.EXTERN_VARDECL.type = var_type;
  extern_vardecl->pos = pos;
  extern_vardecl->kind = ALC_AST_KIND_EXTERN_VARDECL;
  memcpy(extern_vardecl->data.EXTERN_VARDECL.name, name, name_len);
  return extern_vardecl;
}

static alc_ast_t *__function(alc_parser_t *p, usize pos, const char *name, usize name_len)
{
  p->pos += 2;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_LPAREN) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_LPAREN);
    return nullptr;
  }

  alc_ast_t *argument_list = parse_function_arguments(p);
  if ALC_UNLIKELY (argument_list == nullptr)
    return nullptr;

  alc_ast_t *return_type = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_MINUS) {
    if ALC_UNLIKELY (p->tokens[p->pos].has_whitespace_after) {
      add_error_unexpected_whitespace(p, p->pos++, ALC_TOKEN_TYPE_RARROW);
      return nullptr;
    }

    p->pos++;

    if ALC_UNLIKELY (p->pos >= p->tokens_num) {
      add_error_unexpected_eof(p, p->pos);
      return nullptr;
    }

    if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_RARROW) {
      add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_RARROW);
      return nullptr;
    }

    p->pos++;

    if ALC_UNLIKELY (p->pos >= p->tokens_num) {
      add_error_unexpected_eof(p, p->pos);
      return nullptr;
    }

    return_type = parse_type(p);
    if ALC_UNLIKELY (return_type == nullptr)
      return nullptr;
  }

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_SEMICOLON) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_SEMICOLON);
    return nullptr;
  }

  p->pos++;

  alc_ast_t *extern_func =
    alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + sizeof(char) * name_len);
  extern_func->data.EXTERN_FUNC.name = (char *)extern_func + sizeof(alc_ast_t);
  extern_func->data.EXTERN_FUNC.argument_list = argument_list;
  extern_func->data.EXTERN_FUNC.return_type = return_type;
  extern_func->pos = pos;
  extern_func->kind = ALC_AST_KIND_EXTERN_FUNC;
  memcpy(extern_func->data.EXTERN_FUNC.name, name, name_len);
  return extern_func;
}
