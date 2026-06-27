#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"

Alc_Ast *parse_stmt_label(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  Alc_Ast *label = parse_label(p);
  _VERIFY_AST(label);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COLON);

  p->pos++;

  Alc_Ast *stmt_label_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  stmt_label_ast->data.STMT_LABEL.label = label;
  stmt_label_ast->pos = label->pos;
  stmt_label_ast->kind = ALC_AST_KIND_STMT_LABEL;
  return stmt_label_ast;
}
