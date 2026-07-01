#include "alc/parser.h"
#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "alc/vector.h"
#include "global.h"
#include "parser/parser_private.h"
#include <stdarg.h>
#include <string.h>

Alc_Parser *alc_parser_create(Alc_Token *tokens, usize tokens_num)
{
  ALC_ASSERT((tokens_num == 0 && tokens == nullptr) || (tokens_num > 0 && tokens != nullptr));

  Alc_Parser *parser = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Parser));
  parser->tokens = tokens;
  parser->tokens_num = tokens_num;
  parser->errors = alc_vector_create(Alc_Parser_Error);
  parser->pos = 0;
  return parser;
}

void alc_parser_destroy(Alc_Parser *parser)
{
  ALC_ASSERT(parser != nullptr);
  ALC_ASSERT(parser->errors != nullptr);
  alc_vector_destroy(parser->errors);
  parser->errors = nullptr;
}

Alc_Ast *alc_parser_parse(Alc_Parser *parser)
{
  ALC_ASSERT(parser != nullptr);

  Alc_Vector(Alc_Ast *) toplevels = alc_vector_create(Alc_Ast *);

  while (parser->pos < parser->tokens_num) {
    Alc_Ast *top = parse_top(parser);
    if ALC_UNLIKELY (top == nullptr)
      continue;

    alc_vector_push(toplevels, top);
  }

  Alc_Ast *root = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  root->pos = 0;
  root->kind = ALC_AST_KIND_ROOT;
  root->data.ROOT.toplevel_statements =
    alc_vector_get_length(toplevels) > 0 ?
      alc_vector_to_array(toplevels, &root->data.ROOT.toplevel_statements_num) :
      nullptr;
  alc_vector_destroy(toplevels);

  return root;
}

Alc_Ast *parse_ids(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  if (strcmp(p->tokens[p->pos].value, "struct") == 0)
    return parse_struct(p, ALC_AST_STRUCT_KIND_DEFAULT);
  else if (strcmp(p->tokens[p->pos].value, "partial") == 0)
    return parse_partial_struct(p);
  else if (strcmp(p->tokens[p->pos].value, "enum") == 0)
    return parse_enum(p);
  else if (strcmp(p->tokens[p->pos].value, "union") == 0)
    return parse_union(p);
  else if (strcmp(p->tokens[p->pos].value, "using") == 0)
    return parse_typedef(p);
  else if (strcmp(p->tokens[p->pos].value, "scope") == 0)
    return parse_scope(p);
  else if (strcmp(p->tokens[p->pos].value, "extern") == 0)
    return parse_extern(p);
  else if (strcmp(p->tokens[p->pos].value, "import") == 0)
    return parse_import(p);
  else if (strcmp(p->tokens[p->pos].value, "return") == 0)
    return parse_stmt_return(p);
  else if (strcmp(p->tokens[p->pos].value, "goto") == 0)
    return parse_stmt_goto(p);
  else if (strcmp(p->tokens[p->pos].value, "break") == 0)
    return parse_stmt_break(p);
  else if (strcmp(p->tokens[p->pos].value, "continue") == 0)
    return parse_stmt_continue(p);
  else if (strcmp(p->tokens[p->pos].value, "fallthrough") == 0)
    return parse_stmt_fallthrough(p);
  else if (strcmp(p->tokens[p->pos].value, "if") == 0)
    return parse_stmt_if(p);
  else if (strcmp(p->tokens[p->pos].value, "else") == 0)
    return parse_stmt_else(p);
  else if (strcmp(p->tokens[p->pos].value, "loop") == 0)
    return parse_stmt_loop(p);
  else if (strcmp(p->tokens[p->pos].value, "while") == 0)
    return parse_stmt_while(p);
  else if (strcmp(p->tokens[p->pos].value, "do") == 0)
    return parse_stmt_do_while(p);
  else if (strcmp(p->tokens[p->pos].value, "for") == 0)
    return parse_stmt_for(p);
  else if (strcmp(p->tokens[p->pos].value, "foreach") == 0)
    return parse_stmt_foreach(p);
  else if (strcmp(p->tokens[p->pos].value, "switch") == 0)
    return parse_stmt_switch(p);
  else if (strcmp(p->tokens[p->pos].value, "defer") == 0)
    return parse_stmt_defer(p);
  else if (strcmp(p->tokens[p->pos].value, "func") == 0)
    return parse_decldef(p, nullptr);
  else if (strcmp(p->tokens[p->pos].value, "export") == 0) {
    p->pos++;
    return parse_function(p, nullptr, ALC_AST_FUNCTION_KIND_EXPORTED);
  }

  return (void *)-1;
}

Alc_Vector(Alc_Parser_Error) alc_parser_get_errors(const Alc_Parser *parser)
{
  ALC_ASSERT(parser != nullptr);
  ALC_ASSERT(parser->errors != nullptr);
  return parser->errors;
}

Alc_Token *peek(const Alc_Parser *p, s32 adv)
{
  ALC_ASSUME(p != nullptr);
  return p->pos + adv < p->tokens_num ? &p->tokens[p->pos + adv] : nullptr;
}

b8 is_qualifier(const char *str)
{
  return strcmp(str, "const") == 0 || strcmp(str, "persist") == 0 || strcmp(str, "inline") == 0;
}
