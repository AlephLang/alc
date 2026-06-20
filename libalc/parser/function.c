#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "containers/vector.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

static alc_ast_t *parse_variadic_args(alc_parser_t *p);

alc_ast_t *parse_function(alc_parser_t *p, alc_ast_t *attribute_list, alc_ast_function_kind_t kind)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  usize pos = p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COLON);
  _VERIFY_NO_WS(p, p->pos, ALC_TOKEN_TYPE_COLON);

  p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COLON);

  p->pos++;

  alc_ast_t *generic_placeholder_type_list = nullptr;
  if (kind != ALC_AST_FUNCTION_KIND_EXPORTED && p->pos < p->tokens_num &&
      p->tokens[p->pos].type == ALC_TOKEN_TYPE_LARROW) {
    generic_placeholder_type_list = parse_generic_placeholder_type_list(p);
    _VERIFY_AST(generic_placeholder_type_list);
  }

  alc_ast_t *argument_list = parse_function_arguments(p);
  _VERIFY_AST(argument_list);

  alc_ast_t *return_type = nullptr;
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

  alc_ast_t *body;
  if (p->tokens[p->pos].type == ALC_TOKEN_TYPE_ID && strcmp(p->tokens[p->pos].value, "$") == 0) {
    p->pos++;

    body = parse_expr(p, false);
    _VERIFY_AST(body);

    _VERIFY_POS(p, p->pos);
    _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_SEMICOLON);

    p->pos++;
  } else if (p->tokens[p->pos].type == ALC_TOKEN_TYPE_LCBRACK) {
    body = parse_stmt_block(p);
    _VERIFY_AST(body);
  } else {
    add_error_unexpected_token(p, p->pos++, ALC_TOKEN_TYPE_LCBRACK);
    return nullptr;
  }

  if (generic_placeholder_type_list == nullptr) {
    alc_ast_t *function_ast =
      alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + sizeof(char) * name_len);
    function_ast->data.FUNC.name = (char *)function_ast + sizeof(alc_ast_t);
    function_ast->data.FUNC.argument_list = argument_list;
    function_ast->data.FUNC.return_type = return_type;
    function_ast->data.FUNC.body = body;
    function_ast->data.FUNC.attribute_list = attribute_list;
    function_ast->data.FUNC.kind = kind;
    function_ast->pos = pos;
    function_ast->kind = ALC_AST_KIND_FUNC;
    memcpy(function_ast->data.FUNC.name, name, name_len);
    return function_ast;
  } else {
    alc_ast_t *generic_function_ast =
      alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + sizeof(char) * name_len);
    generic_function_ast->data.GENERIC_FUNC.name = (char *)generic_function_ast + sizeof(alc_ast_t);
    generic_function_ast->data.GENERIC_FUNC.generic_placeholder_type_list =
      generic_placeholder_type_list;
    generic_function_ast->data.GENERIC_FUNC.argument_list = argument_list;
    generic_function_ast->data.GENERIC_FUNC.return_type = return_type;
    generic_function_ast->data.GENERIC_FUNC.body = body;
    generic_function_ast->data.GENERIC_FUNC.attribute_list = attribute_list;
    generic_function_ast->data.GENERIC_FUNC.kind = kind;
    generic_function_ast->pos = pos;
    generic_function_ast->kind = ALC_AST_KIND_GENERIC_FUNC;
    memcpy(generic_function_ast->data.GENERIC_FUNC.name, name, name_len);
    return generic_function_ast;
  }
}

alc_ast_t *parse_function_arguments(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_LPAREN);

  usize pos = p->pos++;

  alc_ast_t **args = vector_create(alc_ast_t *);
  b8 first = true;
  while (p->pos < p->tokens_num && p->tokens[p->pos].type != ALC_TOKEN_TYPE_RPAREN) {
    if (!first) {
      _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COMMA, { vector_destroy(args); });
      p->pos++;
    }

    alc_ast_t *arg = p->tokens[p->pos].type == ALC_TOKEN_TYPE_PERIOD ?
                       parse_variadic_args(p) :
                       parse_decldef_var(p, nullptr);
    _VERIFY_AST(arg, { vector_destroy(args); });

    vector_push(args, arg);

    first = false;
  }

  _VERIFY_POS(p, p->pos, { vector_destroy(args); });
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RPAREN, { vector_destroy(args); });

  p->pos++;

  alc_ast_t *argument_list = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  argument_list->data.ARGUMENT_LIST.arguments =
    vector_to_array(args, &argument_list->data.ARGUMENT_LIST.arguments_num);
  argument_list->pos = pos;
  argument_list->kind = ALC_AST_KIND_ARGUMENT_LIST;
  vector_destroy(args);
  return argument_list;
}

static alc_ast_t *parse_variadic_args(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_PERIOD);
  _VERIFY_NO_WS(p, p->pos, ALC_TOKEN_TYPE_PERIOD);

  usize pos = p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_PERIOD);
  _VERIFY_NO_WS(p, p->pos, ALC_TOKEN_TYPE_PERIOD);

  p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_PERIOD);

  p->pos++;

  alc_ast_t *variadic_arg_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  variadic_arg_ast->pos = pos;
  variadic_arg_ast->kind = ALC_AST_KIND_VARIADIC;
  return variadic_arg_ast;
}
