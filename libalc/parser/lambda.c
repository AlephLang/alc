#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/token.h"
#include "alc/vector.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"
#include <string.h>

static inline Alc_Ast *parse_capture_object(Alc_Parser *p);

Alc_Ast *parse_lambda(Alc_Parser *p)
{
  usize pos = p->pos++;
  Alc_Vector(Alc_Ast *) captured_objects = alc_vector_create(Alc_Ast *);

  b8 first = true;
  while (p->pos < p->tokens_num && p->tokens[p->pos].type != ALC_TOKEN_TYPE_RBRACK) {
    if (!first) {
      _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COMMA, { alc_vector_destroy(captured_objects); });
      p->pos++;

      _VERIFY_POS(p, p->pos, { alc_vector_destroy(captured_objects); });
    }

    Alc_Ast *captured_object = parse_capture_object(p);
    _VERIFY_AST(captured_object, { alc_vector_destroy(captured_objects); });

    alc_vector_push(captured_objects, captured_object);

    first = false;
  }

  _VERIFY_POS(p, p->pos, { alc_vector_destroy(captured_objects); });
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RBRACK, { alc_vector_destroy(captured_objects); });

  p->pos++;

  _VERIFY_POS(p, p->pos, { alc_vector_destroy(captured_objects); });

  Alc_Ast *argument_list = parse_function_arguments(p);
  _VERIFY_AST(argument_list, { alc_vector_destroy(captured_objects); });

  Alc_Ast *return_type = nullptr;
  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_MINUS) {
    _VERIFY_NO_WS(p, p->pos, ALC_TOKEN_TYPE_RARROW, { alc_vector_destroy(captured_objects); });

    p->pos++;

    _VERIFY_POS(p, p->pos, { alc_vector_destroy(captured_objects); });
    _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RARROW, { alc_vector_destroy(captured_objects); });

    p->pos++;

    return_type = parse_type(p);
    _VERIFY_AST(return_type, { alc_vector_destroy(captured_objects); });
  }

  _VERIFY_POS(p, p->pos, { alc_vector_destroy(captured_objects); });
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_LCBRACK, { alc_vector_destroy(captured_objects); });

  Alc_Ast *body = parse_stmt_block(p);
  _VERIFY_AST(body, { alc_vector_destroy(captured_objects); });

  Alc_Ast *lambda_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  lambda_ast->LAMBDA.captured_objects =
    alc_vector_to_array(captured_objects, &lambda_ast->LAMBDA.captured_num);
  lambda_ast->LAMBDA.argument_list = argument_list;
  lambda_ast->LAMBDA.return_type = return_type;
  lambda_ast->LAMBDA.body = body;
  lambda_ast->pos = pos;
  lambda_ast->kind = ALC_AST_KIND_LAMBDA;
  alc_vector_destroy(captured_objects);
  return lambda_ast;
}

static inline Alc_Ast *parse_capture_object(Alc_Parser *p)
{
  if (p->tokens[p->pos].type == ALC_TOKEN_TYPE_AMPERSAND) {
    Alc_Ast *lambda_capture_full_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
    lambda_capture_full_ast->pos = p->pos++;
    lambda_capture_full_ast->kind = ALC_AST_KIND_LAMBDA_CAPTURE_FULL;
    return lambda_capture_full_ast;
  }

  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  Alc_Ast *lambda_capture_object_ast =
    alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + sizeof(char) * name_len);
  lambda_capture_object_ast->LAMBDA_CAPTURE_OBJECT.name =
    (char *)lambda_capture_object_ast + sizeof(Alc_Ast);
  lambda_capture_object_ast->pos = p->pos++;
  lambda_capture_object_ast->kind = ALC_AST_KIND_LAMBDA_CAPTURE_OBJECT;
  memcpy(lambda_capture_object_ast->LAMBDA_CAPTURE_OBJECT.name, name, name_len);
  return lambda_capture_object_ast;
}
