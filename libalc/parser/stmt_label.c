#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"

alc_ast_t *parse_stmt_label(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  alc_ast_t *label = parse_label(p);
  _VERIFY_AST(label);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COLON);

  p->pos++;

  alc_ast_t *stmt_label_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  stmt_label_ast->data.STMT_LABEL.label = label;
  stmt_label_ast->pos = label->pos;
  stmt_label_ast->kind = ALC_AST_KIND_STMT_LABEL;
  return stmt_label_ast;
}
