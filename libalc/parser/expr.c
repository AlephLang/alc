#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "allocs/alloc_arena.h"
#include "containers/vector.h"
#include "global.h"
#include "parser/parser_private.h"
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

static alc_ast_t *pratt_parse(alc_parser_t *p, b8 is_toplevel, u8 min_prec, b8 has_assign);
static u8 get_precedence(alc_ast_kind_t op_kind);
static alc_ast_t *parse_operator(alc_parser_t *p);
static b8 is_namespace(const alc_parser_t *p);
static b8 is_generic_call_or_namespace(const alc_parser_t *p);
static inline b8 is_generic_call(const alc_parser_t *p)
{
  return is_generic_call_or_namespace(p);
}
static b8 is_call(const alc_parser_t *p);
static b8 parse_call_arguments(alc_parser_t *p, alc_ast_t ***out_arguments, usize *out_n);
static alc_ast_t *parse_operands(alc_parser_t *p);
static alc_ast_t *parse_namespaces_and_identifier_operands(alc_parser_t *p);
static alc_ast_t *parse_only_operands(alc_parser_t *p);
static alc_ast_t *parse_post(alc_parser_t *p, alc_ast_t *ast);
static alc_ast_t *parse_identifier(alc_parser_t *p);
static alc_ast_t *parse_call(alc_parser_t *p);
static alc_ast_t *parse_generic_call(alc_parser_t *p);
static alc_ast_t *parse_namespace(alc_parser_t *p);
static alc_ast_t *parse_generic_call_or_namespace(alc_parser_t *p);
static alc_ast_t *parse_sizeof(alc_parser_t *p);
static alc_ast_t *parse_alignof(alc_parser_t *p);
static alc_ast_t *parse_cast(alc_parser_t *p);
static alc_ast_t *parse_prefix_expr(alc_parser_t *p);
static alc_ast_t *parse_operands_or_prefix(alc_parser_t *p);
static char *parse_typespec(alc_parser_t *p, b8 prev_has_whitespace_after);
static inline u64 str_to_num(const char *str, alc_token_type_t numtype);
static inline u64 str_dec_to_num(const char *str);
static inline u64 str_hex_to_num(const char *str);
static inline u64 str_bin_to_num(const char *str);
static inline u64 str_oct_to_num(const char *str);

alc_ast_t *parse_expr(alc_parser_t *p, b8 is_toplevel)
{
  return pratt_parse(p, is_toplevel, 0, false);
}

alc_ast_t *parse_stmt_expr(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  alc_ast_t *expr = parse_expr(p, true);
  if ALC_UNLIKELY (expr == nullptr)
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

  alc_ast_t *stmt_expr = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  stmt_expr->data.STMT_EXPR.expression = expr;
  stmt_expr->pos = expr->pos;
  stmt_expr->kind = ALC_AST_KIND_STMT_EXPR;
  return stmt_expr;
}

static alc_ast_t *pratt_parse(alc_parser_t *p, b8 is_toplevel, u8 min_prec, b8 has_assign)
{
  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  alc_ast_t *lhs = parse_operands_or_prefix(p);
  if ALC_UNLIKELY (lhs == nullptr)
    return nullptr;

  while (p->pos < p->tokens_num) {
    usize saved_parser_pos = p->pos;

    alc_ast_t *operator = parse_operator(p);
    if (operator == nullptr)
      break;

    u8 prec = get_precedence(operator->kind);
    if (prec <= min_prec) {
      p->pos = saved_parser_pos;
      break;
    }

    b8 assign = false;
    switch (operator->kind) {
    case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_EQ:
    case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_ADDEQ:
    case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_SUBEQ:
    case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_MULEQ:
    case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_DIVEQ:
    case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_MODEQ:
    case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_SHLEQ:
    case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_SHREQ:
    case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_ANDEQ:
    case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_OREQ:
    case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_XOREQ: {
      assign = true;
      if ALC_UNLIKELY (has_assign) {
        alc_parser_error_t two_assign_operators_error = {
          .pos = operator->pos,
          .len = 1,
          .type = ALC_PARSER_ERROR_TYPE_TWO_ASSIGN_OPERATORS_IN_EXPRESSION,
        };
        add_error(p, two_assign_operators_error);
      } else if ALC_UNLIKELY (!is_toplevel) {
        alc_parser_error_t assign_in_non_toplevel_error = {
          .pos = operator->pos,
          .len = 1,
          .type = ALC_PARSER_ERROR_TYPE_ASSIGN_OPERATOR_IN_NON_TOPLEVEL_EXPRESSION,
        };
        add_error(p, assign_in_non_toplevel_error);
      }
    }
    default:
      break;
    }

    alc_ast_t *rhs = pratt_parse(p, is_toplevel, prec, has_assign || assign);
    if ALC_UNLIKELY (rhs == nullptr)
      return nullptr;

    alc_ast_t *expr = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
    expr->data.EXPR.lhs = lhs;
    expr->data.EXPR.rhs = rhs;
    expr->data.EXPR.operator = operator;
    expr->pos = lhs->pos;
    expr->kind = ALC_AST_KIND_EXPR;
    lhs = expr;
  }

  return lhs;
}

static u8 get_precedence(alc_ast_kind_t op_kind)
{
  switch (op_kind) {
  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_EQ:
  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_ADDEQ:
  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_SUBEQ:
  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_MULEQ:
  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_DIVEQ:
  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_MODEQ:
  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_SHLEQ:
  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_SHREQ:
  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_ANDEQ:
  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_OREQ:
  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_XOREQ:
    return 1;

  case ALC_AST_KIND_EXPR_OPERATOR_BOOLEAN_AND:
  case ALC_AST_KIND_EXPR_OPERATOR_BOOLEAN_OR:
    return 2;

  case ALC_AST_KIND_EXPR_OPERATOR_COMPARE_EQ:
  case ALC_AST_KIND_EXPR_OPERATOR_COMPARE_NOTEQ:
  case ALC_AST_KIND_EXPR_OPERATOR_COMPARE_LTHAN:
  case ALC_AST_KIND_EXPR_OPERATOR_COMPARE_GTHAN:
  case ALC_AST_KIND_EXPR_OPERATOR_COMPARE_LTHANEQ:
  case ALC_AST_KIND_EXPR_OPERATOR_COMPARE_GTHANEQ:
    return 3;

  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_ADD:
  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_SUB:
    return 4;

  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_MUL:
  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_DIV:
  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_MOD:
    return 5;

  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_SHL:
  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_SHR:
    return 6;

  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_AND:
  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_OR:
  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_XOR:
    return 7;

  default:
    ALC_NOREACH();
  }
}

static alc_ast_t *parse_operator(alc_parser_t *p)
{
  usize pos = p->pos;

#define _GEN_AND_ADVANCE(_name, _type, _adv)                                            \
  {                                                                                     \
    alc_ast_t *__alc__##_name = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t)); \
    __alc__##_name->pos = pos;                                                          \
    __alc__##_name->kind = ALC_AST_KIND_EXPR_OPERATOR_##_type;                          \
    p->pos += (_adv);                                                                   \
    return __alc__##_name;                                                              \
  }

#define _SINGLE(_type1) _GEN_AND_ADVANCE(single_op, _type1, 1)

#define _DOUBLE(_type2, _type1)                                                  \
  {                                                                              \
    if (!p->tokens[p->pos].has_whitespace_after && p->pos + 1 < p->tokens_num && \
        p->tokens[p->pos + 1].type == ALC_TOKEN_TYPE_EQ) {                       \
      _GEN_AND_ADVANCE(double_op, _type2, 2)                                     \
    }                                                                            \
    _GEN_AND_ADVANCE(single_op, _type1, 1)                                       \
  }

#define _DOUBLE2(_type21, _type22, _type1)                                       \
  {                                                                              \
    if (!p->tokens[p->pos].has_whitespace_after && p->pos + 1 < p->tokens_num) { \
      if (p->tokens[p->pos + 1].type == ALC_TOKEN_TYPE_EQ) {                     \
        _GEN_AND_ADVANCE(double_first_op, _type21, 2)                            \
      } else if (p->tokens[p->pos + 1].type == toktype) {                        \
        _GEN_AND_ADVANCE(double_second_op, _type22, 2)                           \
      }                                                                          \
    }                                                                            \
    _GEN_AND_ADVANCE(single_op, _type1, 1)                                       \
  }

#define _TRIPLE(_type3, _type21, _type22, _type1)                                        \
  {                                                                                      \
    if (!p->tokens[p->pos].has_whitespace_after && p->pos + 1 < p->tokens_num) {         \
      if (p->tokens[p->pos + 1].type == ALC_TOKEN_TYPE_EQ) {                             \
        _GEN_AND_ADVANCE(double_first_op, _type21, 2)                                    \
      } else if (p->tokens[p->pos + 1].type == toktype) {                                \
        if (!p->tokens[p->pos + 1].has_whitespace_after && p->pos + 2 < p->tokens_num && \
            p->tokens[p->pos + 2].type == ALC_TOKEN_TYPE_EQ) {                           \
          _GEN_AND_ADVANCE(triple_op, _type3, 3)                                         \
        }                                                                                \
        _GEN_AND_ADVANCE(double_second_op, _type22, 2)                                   \
      }                                                                                  \
    }                                                                                    \
    _GEN_AND_ADVANCE(single_op, _type1, 1)                                               \
  }

  alc_token_type_t toktype = p->tokens[p->pos].type;

  switch (toktype) {
    // += +
  case ALC_TOKEN_TYPE_PLUS: {
    _DOUBLE(ASSIGN_ADDEQ, BINARY_ADD)
  }

    // -= -
  case ALC_TOKEN_TYPE_MINUS: {
    _DOUBLE(ASSIGN_SUBEQ, BINARY_SUB)
  }

    // *= *
  case ALC_TOKEN_TYPE_ASTERISK: {
    _DOUBLE(ASSIGN_MULEQ, BINARY_MUL)
  }

    // /= /
  case ALC_TOKEN_TYPE_SLASH: {
    _DOUBLE(ASSIGN_DIVEQ, BINARY_DIV)
  }

    // &= && &
  case ALC_TOKEN_TYPE_AMPERSAND: {
    _DOUBLE2(ASSIGN_ANDEQ, BOOLEAN_AND, BINARY_AND)
  }

    // |= || |
  case ALC_TOKEN_TYPE_PIPE: {
    _DOUBLE2(ASSIGN_OREQ, BOOLEAN_OR, BINARY_OR)
  }

    // ^= ^
  case ALC_TOKEN_TYPE_CIRCUMFLEX: {
    _DOUBLE(ASSIGN_XOREQ, BINARY_XOR)
  }

    // == =
  case ALC_TOKEN_TYPE_EQ: {
    _DOUBLE(COMPARE_EQ, ASSIGN_EQ)
  }

    // !=
  case ALC_TOKEN_TYPE_EXCLMARK: {
    if ALC_LIKELY (!p->tokens[p->pos].has_whitespace_after && p->pos + 1 < p->tokens_num &&
                   p->tokens[p->pos + 1].type == ALC_TOKEN_TYPE_EQ) {
      _GEN_AND_ADVANCE(not_eq_op, COMPARE_NOTEQ, 2)
    }
    break;
  }

    // <<= <= << <
  case ALC_TOKEN_TYPE_LARROW: {
    _TRIPLE(ASSIGN_SHLEQ, COMPARE_LTHANEQ, BINARY_SHL, COMPARE_LTHAN)
  }

    // >>= >= >> >
  case ALC_TOKEN_TYPE_RARROW: {
    _TRIPLE(ASSIGN_SHREQ, COMPARE_GTHANEQ, BINARY_SHR, COMPARE_GTHAN)
  }

  default:
    break;
  }

  return nullptr;
}

static b8 is_namespace(const alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  alc_token_t *tok1 = peek(p, 0), *tok2 = peek(p, 1), *tok3 = peek(p, 2);
  return tok1 != nullptr && tok2 != nullptr && tok3 != nullptr && !tok1->has_whitespace_after &&
         !tok2->has_whitespace_after && !tok3->has_whitespace_after &&
         tok1->type == ALC_TOKEN_TYPE_ID && tok2->type == ALC_TOKEN_TYPE_COLON &&
         tok3->type == tok2->type;
}

static b8 is_generic_call_or_namespace(const alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  alc_token_t *tok1 = peek(p, 0), *tok2 = peek(p, 1), *tok3 = peek(p, 2);
  return tok1 != nullptr && tok2 != nullptr && tok3 != nullptr && !tok1->has_whitespace_after &&
         !tok2->has_whitespace_after && tok1->type == ALC_TOKEN_TYPE_ID &&
         tok2->type == ALC_TOKEN_TYPE_EXCLMARK && tok3->type == ALC_TOKEN_TYPE_LARROW;
}

static b8 is_call(const alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  alc_token_t *tok1 = peek(p, 0), *tok2 = peek(p, 1);
  return tok1 != nullptr && tok2 != nullptr && tok1->type == ALC_TOKEN_TYPE_ID &&
         tok2->type == ALC_TOKEN_TYPE_LPAREN;
}

static b8 parse_call_arguments(alc_parser_t *p, alc_ast_t ***out_arguments, usize *out_n)
{
  ALC_ASSUME(p != nullptr);
  ALC_ASSUME(out_arguments != nullptr);
  ALC_ASSUME(out_n != nullptr);

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return false;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_LPAREN) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_LPAREN);
    return false;
  }

  p->pos++;

  b8 first = true;
  alc_ast_t **arguments = vector_create(alc_ast_t *);
  while (p->pos < p->tokens_num) {
    if (p->tokens[p->pos].type == ALC_TOKEN_TYPE_RPAREN)
      break;

    if (!first) {
      if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_COMMA) {
        add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_COMMA);
        vector_destroy(arguments);
        return false;
      }

      p->pos++;

      if ALC_UNLIKELY (p->pos >= p->tokens_num) {
        add_error_unexpected_eof(p, p->pos);
        vector_destroy(arguments);
        return false;
      }
    }

    alc_ast_t *argument = p->tokens[p->pos].type == ALC_TOKEN_TYPE_LCBRACK ? parse_initlist(p) :
                                                                             parse_expr(p, false);
    if ALC_UNLIKELY (argument == nullptr) {
      vector_destroy(arguments);
      return false;
    }

    vector_push(arguments, argument);

    first = false;
  }

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    vector_destroy(arguments);
    return false;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_RPAREN) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_RPAREN);
    vector_destroy(arguments);
    return false;
  }

  p->pos++;

  *out_arguments = vector_to_array(arguments, out_n);
  vector_destroy(arguments);
  return true;
}

static alc_ast_t *parse_operands(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  switch (p->tokens[p->pos].type) {
  case ALC_TOKEN_TYPE_ID: {
    if (strcmp(p->tokens[p->pos].value, "sizeof") == 0)
      return parse_sizeof(p);
    else if (strcmp(p->tokens[p->pos].value, "alignof") == 0)
      return parse_alignof(p);
    else if (strcmp(p->tokens[p->pos].value, "cast") == 0)
      return parse_cast(p);
    return parse_namespaces_and_identifier_operands(p);
  }

  case ALC_TOKEN_TYPE_NUMBER:
  case ALC_TOKEN_TYPE_NUMBER_HEX:
  case ALC_TOKEN_TYPE_NUMBER_BIN:
  case ALC_TOKEN_TYPE_NUMBER_OCT: {
    //ALC_TODO("JOPAAAA");

    usize pos = p->pos;
    u64 value = str_to_num(p->tokens[p->pos].value, p->tokens[p->pos].type);

    b8 has_ws = p->tokens[p->pos].has_whitespace_after;
    p->pos++;

    char *typespec = parse_typespec(p, has_ws);

    alc_ast_t *number_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
    number_ast->data.EXPR_OPERAND_NUMBER.value = value;
    number_ast->data.EXPR_OPERAND_NUMBER.typespec = typespec;
    number_ast->pos = pos;
    number_ast->kind = ALC_AST_KIND_EXPR_OPERAND_NUMBER;
    return parse_post(p, number_ast);
  }

  case ALC_TOKEN_TYPE_NUMBER_FLOAT: {
    usize pos = p->pos;
    f64 value = atof(p->tokens[p->pos].value);

    b8 has_ws = p->tokens[p->pos].has_whitespace_after;
    p->pos++;

    char *typespec = parse_typespec(p, has_ws);

    alc_ast_t *number_float_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
    number_float_ast->data.EXPR_OPERAND_NUMBER_FLOAT.value = value;
    number_float_ast->data.EXPR_OPERAND_NUMBER_FLOAT.typespec = typespec;
    number_float_ast->pos = pos;
    number_float_ast->kind = ALC_AST_KIND_EXPR_OPERAND_NUMBER_FLOAT;
    return parse_post(p, number_float_ast);
  }

  case ALC_TOKEN_TYPE_STRING: {
    // TODO: I'm not sure if I need to copy string content.
    // I probably can just give it a pointer to the value in token.
    // It is static so it doesn't really matter I guess :/
    const char *content = p->tokens[p->pos].value;
    usize content_len = strlen(content) + 1;

    b8 has_ws = p->tokens[p->pos].has_whitespace_after;
    usize pos = p->pos++;
    char *typespec = parse_typespec(p, has_ws);
    alc_ast_t *string_ast =
      alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + sizeof(char) * content_len);
    string_ast->data.EXPR_OPERAND_STRING.content = (char *)string_ast + sizeof(alc_ast_t);
    string_ast->data.EXPR_OPERAND_STRING.typespec = typespec;
    string_ast->pos = pos;
    string_ast->kind = ALC_AST_KIND_EXPR_OPERAND_STRING;
    memcpy(string_ast->data.EXPR_OPERAND_STRING.content, content, content_len);
    return parse_post(p, string_ast);
  }

  case ALC_TOKEN_TYPE_SYMBOL: {
    const char *content = p->tokens[p->pos].value;
    usize content_len = strlen(content) + 1;

    b8 has_ws = p->tokens[p->pos].has_whitespace_after;
    usize pos = p->pos++;
    char *typespec = parse_typespec(p, has_ws);
    alc_ast_t *symbol_ast =
      alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + sizeof(char) * content_len);
    symbol_ast->data.EXPR_OPERAND_SYMBOL.content = (char *)symbol_ast + sizeof(alc_ast_t);
    symbol_ast->data.EXPR_OPERAND_SYMBOL.typespec = typespec;
    symbol_ast->pos = pos;
    symbol_ast->kind = ALC_AST_KIND_EXPR_OPERAND_SYMBOL;
    memcpy(symbol_ast->data.EXPR_OPERAND_SYMBOL.content, content, content_len);
    return parse_post(p, symbol_ast);
  }

  case ALC_TOKEN_TYPE_LPAREN: {
    p->pos++;
    alc_ast_t *expr = parse_expr(p, false);
    if ALC_UNLIKELY (expr == nullptr)
      return nullptr;

    if ALC_UNLIKELY (p->pos >= p->tokens_num) {
      add_error_unexpected_eof(p, p->pos);
      return nullptr;
    }

    if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_RPAREN) {
      add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_RPAREN);
      return nullptr;
    }

    p->pos++;

    return parse_post(p, expr);
  }

  default: {
    add_error_unexpected_token(p, p->pos++, 9, ALC_TOKEN_TYPE_ID, ALC_TOKEN_TYPE_NUMBER,
                               ALC_TOKEN_TYPE_NUMBER_HEX, ALC_TOKEN_TYPE_NUMBER_BIN,
                               ALC_TOKEN_TYPE_NUMBER_OCT, ALC_TOKEN_TYPE_NUMBER_FLOAT,
                               ALC_TOKEN_TYPE_STRING, ALC_TOKEN_TYPE_SYMBOL, ALC_TOKEN_TYPE_LPAREN);
    return nullptr;
  }
  }
}

static alc_ast_t *parse_namespaces_and_identifier_operands(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  alc_ast_t *ast = is_generic_call_or_namespace(p) ? parse_generic_call_or_namespace(p) :
                   is_namespace(p)                 ? parse_namespace(p) :
                   is_call(p)                      ? parse_call(p) :
                                                     parse_identifier(p);

  if ALC_UNLIKELY (ast == nullptr)
    return nullptr;

  return parse_post(p, ast);
}

static alc_ast_t *parse_only_operands(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  alc_ast_t *ast = is_generic_call(p) ? parse_generic_call(p) :
                   is_call(p)         ? parse_call(p) :
                                        parse_identifier(p);

  if ALC_UNLIKELY (ast == nullptr)
    return nullptr;

  return parse_post(p, ast);
}

static alc_ast_t *parse_post(alc_parser_t *p, alc_ast_t *ast)
{
  ALC_ASSUME(p != nullptr);
  ALC_ASSUME(ast != nullptr);

  while (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_LBRACK) {
    usize pos = p->pos++;

    if ALC_UNLIKELY (p->pos >= p->tokens_num) {
      add_error_unexpected_eof(p, p->pos);
      return nullptr;
    }

    alc_ast_t *index_expr = parse_expr(p, false);
    if ALC_UNLIKELY (index_expr == nullptr)
      return nullptr;

    if ALC_UNLIKELY (p->pos >= p->tokens_num) {
      add_error_unexpected_eof(p, p->pos);
      return nullptr;
    }

    if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_RBRACK) {
      add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_RBRACK);
      return nullptr;
    }

    p->pos++;

    alc_ast_t *array = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
    array->data.EXPR_OPERAND_ARRAY_ELEMENT.array = ast;
    array->data.EXPR_OPERAND_ARRAY_ELEMENT.index_expression = index_expr;
    array->pos = pos;
    array->kind = ALC_AST_KIND_EXPR_OPERAND_ARRAY_ELEMENT;
    ast = array;
  }

  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_PERIOD) {
    p->pos++;

    if ALC_UNLIKELY (p->pos >= p->tokens_num) {
      add_error_unexpected_eof(p, p->pos);
      return nullptr;
    }

    alc_ast_t *member = parse_only_operands(p);
    if ALC_UNLIKELY (member == nullptr)
      return nullptr;

    alc_ast_t *access_member = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
    access_member->data.EXPR_OPERAND_ACCESS_MEMBER.from = ast;
    access_member->data.EXPR_OPERAND_ACCESS_MEMBER.what = member;
    access_member->pos = ast->pos;
    access_member->kind = ALC_AST_KIND_EXPR_OPERAND_ACCESS_MEMBER;
    return access_member;
  }

  return ast;
}

static alc_ast_t *parse_identifier(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

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

  alc_ast_t *identifier_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + name_len);
  identifier_ast->data.EXPR_OPERAND_IDENTIFIER.name = (char *)identifier_ast + sizeof(alc_ast_t);
  identifier_ast->pos = pos;
  identifier_ast->kind = ALC_AST_KIND_EXPR_OPERAND_IDENTIFIER;
  memcpy(identifier_ast->data.EXPR_OPERAND_IDENTIFIER.name, name, name_len);

  return identifier_ast;
}

static alc_ast_t *parse_call(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos;

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  p->pos++;

  alc_ast_t **arguments_array;
  usize arguments_num;
  if ALC_UNLIKELY (!parse_call_arguments(p, &arguments_array, &arguments_num))
    return nullptr;

  alc_ast_t *call_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + name_len);
  call_ast->data.EXPR_OPERAND_CALL.callee_name = (char *)call_ast + sizeof(alc_ast_t);
  call_ast->data.EXPR_OPERAND_CALL.arguments = arguments_array;
  call_ast->data.EXPR_OPERAND_CALL.arguments_num = arguments_num;
  call_ast->pos = pos;
  call_ast->kind = ALC_AST_KIND_EXPR_OPERAND_CALL;
  memcpy(call_ast->data.EXPR_OPERAND_CALL.callee_name, name, name_len);
  return call_ast;
}

static alc_ast_t *parse_generic_call(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos;

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  p->pos++;

  alc_ast_t *generic_type_list = parse_generic_type_list(p);
  if ALC_UNLIKELY (generic_type_list == nullptr)
    return nullptr;

  alc_ast_t **arguments_array;
  usize arguments_num;
  if ALC_UNLIKELY (!parse_call_arguments(p, &arguments_array, &arguments_num))
    return nullptr;

  alc_ast_t *generic_call_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + name_len);
  generic_call_ast->data.EXPR_OPERAND_GENERIC_CALL.callee_name =
    (char *)generic_call_ast + sizeof(alc_ast_t);
  generic_call_ast->data.EXPR_OPERAND_GENERIC_CALL.generic_type_list = generic_type_list;
  generic_call_ast->data.EXPR_OPERAND_GENERIC_CALL.arguments = arguments_array;
  generic_call_ast->data.EXPR_OPERAND_GENERIC_CALL.arguments_num = arguments_num;
  generic_call_ast->pos = pos;
  generic_call_ast->kind = ALC_AST_KIND_EXPR_OPERAND_CALL;
  memcpy(generic_call_ast->data.EXPR_OPERAND_GENERIC_CALL.callee_name, name, name_len);
  return generic_call_ast;
}

static alc_ast_t *parse_namespace(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos;

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  p->pos += 3;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  alc_ast_t *subobject = parse_namespaces_and_identifier_operands(p);
  if ALC_UNLIKELY (subobject == nullptr)
    return nullptr;

  alc_ast_t *namespace_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));

  namespace_ast->data.NAMESPACE.name = (char *)namespace_ast + sizeof(alc_ast_t);
  namespace_ast->data.NAMESPACE.subobject = subobject;
  namespace_ast->pos = pos;
  namespace_ast->kind = ALC_AST_KIND_NAMESPACE;
  memcpy(namespace_ast->data.NAMESPACE.name, name, name_len);
  return namespace_ast;
}

static alc_ast_t *parse_generic_call_or_namespace(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos;

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  p->pos++;

  alc_ast_t *generic_type_list = parse_generic_type_list(p);
  if ALC_UNLIKELY (generic_type_list == nullptr)
    return nullptr;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  alc_token_t *tok0 = peek(p, -1), *tok1 = peek(p, 0), *tok2 = peek(p, 1);
  if (tok1 != nullptr && tok2 != nullptr && !tok0->has_whitespace_after &&
      !tok1->has_whitespace_after && !tok2->has_whitespace_after &&
      tok1->type == ALC_TOKEN_TYPE_COLON && tok2->type == ALC_TOKEN_TYPE_COLON) {
    p->pos += 2;
    if ALC_UNLIKELY (p->pos >= p->tokens_num) {
      add_error_unexpected_eof(p, p->pos);
      return nullptr;
    }

    alc_ast_t *subobject = parse_namespaces_and_identifier_operands(p);
    if ALC_UNLIKELY (subobject == nullptr)
      return nullptr;

    alc_ast_t *generic_namespace_ast =
      alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + name_len);
    generic_namespace_ast->data.GENERIC_NAMESPACE.name =
      (char *)generic_namespace_ast + sizeof(alc_ast_t);
    generic_namespace_ast->data.GENERIC_NAMESPACE.generic_type_list = generic_type_list;
    generic_namespace_ast->data.GENERIC_NAMESPACE.subobject = subobject;
    generic_namespace_ast->pos = pos;
    generic_namespace_ast->kind = ALC_AST_KIND_GENERIC_NAMESPACE;
    memcpy(generic_namespace_ast->data.GENERIC_NAMESPACE.name, name, name_len);
    return generic_namespace_ast;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_LPAREN) {
    add_error_unexpected_token(p, p->pos++, 2, ALC_TOKEN_TYPE_LPAREN, ALC_TOKEN_TYPE_COLON);
    return nullptr;
  }

  alc_ast_t **arguments_array;
  usize arguments_num;
  if ALC_UNLIKELY (!parse_call_arguments(p, &arguments_array, &arguments_num))
    return nullptr;

  alc_ast_t *generic_call_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t) + name_len);
  generic_call_ast->data.EXPR_OPERAND_GENERIC_CALL.callee_name =
    (char *)generic_call_ast + sizeof(alc_ast_t);
  generic_call_ast->data.EXPR_OPERAND_GENERIC_CALL.generic_type_list = generic_type_list;
  generic_call_ast->data.EXPR_OPERAND_GENERIC_CALL.arguments = arguments_array;
  generic_call_ast->data.EXPR_OPERAND_GENERIC_CALL.arguments_num = arguments_num;
  generic_call_ast->pos = pos;
  generic_call_ast->kind = ALC_AST_KIND_EXPR_OPERAND_GENERIC_CALL;
  memcpy(generic_call_ast->data.EXPR_OPERAND_GENERIC_CALL.callee_name, name, name_len);
  return generic_call_ast;
}

static alc_ast_t *parse_sizeof(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos++;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_LPAREN) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_LPAREN);
    return nullptr;
  }

  p->pos++;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  alc_ast_t *type = parse_type(p);
  if ALC_UNLIKELY (type == nullptr)
    return nullptr;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_RPAREN) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_RPAREN);
    return nullptr;
  }

  p->pos++;

  alc_ast_t *sizeof_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  sizeof_ast->data.EXPR_OPERAND_SIZE_OF.type = type;
  sizeof_ast->pos = pos;
  sizeof_ast->kind = ALC_AST_KIND_EXPR_OPERAND_SIZE_OF;
  return sizeof_ast;
}

static alc_ast_t *parse_alignof(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos++;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_LPAREN) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_LPAREN);
    return nullptr;
  }

  p->pos++;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  alc_ast_t *expr = parse_expr(p, false);
  if ALC_UNLIKELY (expr == nullptr)
    return nullptr;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_RPAREN) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_RPAREN);
    return nullptr;
  }

  p->pos++;

  alc_ast_t *alignof_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  alignof_ast->data.EXPR_OPERAND_ALIGN_OF.expression = expr;
  alignof_ast->pos = pos;
  alignof_ast->kind = ALC_AST_KIND_EXPR_OPERAND_ALIGN_OF;
  return alignof_ast;
}

static alc_ast_t *parse_cast(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos++;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_LPAREN) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_LPAREN);
    return nullptr;
  }

  p->pos++;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  alc_ast_t *type = parse_type(p);
  if ALC_UNLIKELY (type == nullptr)
    return nullptr;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  if ALC_UNLIKELY (p->tokens[p->pos].type != ALC_TOKEN_TYPE_RPAREN) {
    add_error_unexpected_token(p, p->pos++, 1, ALC_TOKEN_TYPE_RPAREN);
    return nullptr;
  }

  p->pos++;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  alc_ast_t *expr = parse_expr(p, false);
  if ALC_UNLIKELY (expr == nullptr)
    return nullptr;

  alc_ast_t *cast_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  cast_ast->data.EXPR_OPERAND_CAST_TO.type = type;
  cast_ast->data.EXPR_OPERAND_CAST_TO.expression = expr;
  cast_ast->pos = pos;
  cast_ast->kind = ALC_AST_KIND_EXPR_OPERAND_CAST_TO;
  return nullptr;
}

static alc_ast_t *parse_prefix_expr(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos;

  alc_ast_kind_t kind;
  switch (p->tokens[p->pos].type) {
  case ALC_TOKEN_TYPE_ASTERISK:
    kind = ALC_AST_KIND_EXPR_OPERATOR_PREFIX_DEREFERENCE;
    break;
  case ALC_TOKEN_TYPE_EXCLMARK:
    kind = ALC_AST_KIND_EXPR_OPERATOR_PREFIX_BOOLEAN_NOT;
    break;
  case ALC_TOKEN_TYPE_TILDE:
    kind = ALC_AST_KIND_EXPR_OPERATOR_PREFIX_NOT;
    break;
  case ALC_TOKEN_TYPE_MINUS:
    kind = ALC_AST_KIND_EXPR_OPERATOR_PREFIX_NEGATIVE;
    break;
  case ALC_TOKEN_TYPE_AMPERSAND:
    kind = ALC_AST_KIND_EXPR_OPERATOR_PREFIX_ADDRESS;
    break;
  default:
    ALC_NOREACH();
  }

  p->pos++;

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  alc_ast_t *operand = parse_expr(p, false);
  if ALC_UNLIKELY (operand == nullptr)
    return nullptr;

  alc_ast_t *operator_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  operator_ast->pos = pos;
  operator_ast->kind = kind;

  alc_ast_t *prefix_expr_ast = alloc_arena_allocate(&ctx()->arena, sizeof(alc_ast_t));
  prefix_expr_ast->data.PREFIX_EXPR.operand = operand;
  prefix_expr_ast->data.PREFIX_EXPR.operator = operator_ast;
  prefix_expr_ast->pos = operand->pos;
  prefix_expr_ast->kind = ALC_AST_KIND_PREFIX_EXPR;

  return prefix_expr_ast;
}

static alc_ast_t *parse_operands_or_prefix(alc_parser_t *p)
{
  ALC_ASSUME(p != nullptr);

  if ALC_UNLIKELY (p->pos >= p->tokens_num) {
    add_error_unexpected_eof(p, p->pos);
    return nullptr;
  }

  switch (p->tokens[p->pos].type) {
  case ALC_TOKEN_TYPE_ASTERISK:
  case ALC_TOKEN_TYPE_EXCLMARK:
  case ALC_TOKEN_TYPE_TILDE:
  case ALC_TOKEN_TYPE_MINUS:
  case ALC_TOKEN_TYPE_AMPERSAND:
    return parse_prefix_expr(p);
  default:
    return parse_operands(p);
  }
}

static char *parse_typespec(alc_parser_t *p, b8 prev_has_whitespace_after)
{
  ALC_ASSUME(p != nullptr);

  if (prev_has_whitespace_after || p->pos >= p->tokens_num ||
      p->tokens[p->pos].type != ALC_TOKEN_TYPE_ID)
    return nullptr;

  const char *typespec = p->tokens[p->pos].value;
  usize typespec_len = strlen(typespec) + 1;

  char *out = alloc_arena_allocate_aligned(&ctx()->arena, typespec_len, 1);
  for (char *p = out; *typespec; typespec++, p++)
    *p = tolower(*typespec);

  p->pos++;

  return out;
}

static inline u64 str_to_num(const char *str, alc_token_type_t numtype)
{
  switch (numtype) {
  case ALC_TOKEN_TYPE_NUMBER:
    return str_dec_to_num(str);
  case ALC_TOKEN_TYPE_NUMBER_HEX:
    return str_hex_to_num(str);
  case ALC_TOKEN_TYPE_NUMBER_BIN:
    return str_bin_to_num(str);
  case ALC_TOKEN_TYPE_NUMBER_OCT:
    return str_oct_to_num(str);
  default:
    ALC_NOREACH();
  }
}

static inline u64 str_dec_to_num(const char *str)
{
  u64 out = 0;
  for (; *str; str++) {
    ALC_ASSUME(*str >= '0' && *str <= '9');

    out *= 10;
    out += *str - '0';
  }
  return out;
}

static inline u64 str_hex_to_num(const char *str)
{
  u64 out = 0;
  for (; *str; str++) {
    ALC_ASSUME((*str >= '0' && *str <= '9') || (*str >= 'A' && *str <= 'F') ||
               (*str >= 'a' && *str <= 'f'));

    out <<= 4;
    out |= *str >= 'A' && *str <= 'F' ? 10 + *str - 'A' :
           *str >= 'a' && *str <= 'f' ? 10 + *str - 'a' :
                                        *str - '0';
  }
  return out;
}

static inline u64 str_bin_to_num(const char *str)
{
  u64 out = 0;
  for (; *str; str++) {
    ALC_ASSUME(*str == '0' || *str == '1');

    out <<= 1;
    out |= *str - '0';
  }
  return out;
}

static inline u64 str_oct_to_num(const char *str)
{
  u64 out = 0;
  for (; *str; str++) {
    ALC_ASSUME(*str >= '0' && *str <= '7');

    out <<= 3;
    out |= *str - '0';
  }
  return out;
}
