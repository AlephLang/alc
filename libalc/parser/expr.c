#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "alc/vector.h"
#include "alc/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

static Alc_Ast *pratt_parse(Alc_Parser *p, b8 is_toplevel, u8 min_prec, b8 has_assign);
static u8 get_precedence(Alc_Ast_Kind op_kind);
static usize get_operator_length(Alc_Ast_Kind op_kind);
static Alc_Ast *parse_operator(Alc_Parser *p);
static Alc_Ast *parse_prefix_expr(Alc_Parser *p);
static Alc_Ast *parse_operands_or_prefix(Alc_Parser *p);
static Alc_Ast *parse_operand(Alc_Parser *p);
static Alc_Ast *parse_operand_base(Alc_Parser *p);
static Alc_Ast *parse_id_operand(Alc_Parser *p);
static Alc_Ast *parse_operand_package(Alc_Parser *p);
static Alc_Ast *parse_operand_identifier(Alc_Parser *p);
static Alc_Ast *parse_sizeof(Alc_Parser *p);
static Alc_Ast *parse_alignof(Alc_Parser *p);
static Alc_Ast *parse_offsetof(Alc_Parser *p);
static Alc_Ast *parse_cast(Alc_Parser *p);
static Alc_Ast *parse_post(Alc_Parser *p, Alc_Ast *ast);
static Alc_Vector(Alc_Ast *) parse_call_arguments(Alc_Parser *p);
static Alc_Ast *parse_explicit_call_argument(Alc_Parser *p);
static char *parse_typespec(Alc_Parser *p);
static inline b8 is_package(Alc_Parser *p);
static inline u64 str_to_num(const char *str, Alc_Token_Type numtype);
static inline u64 str_dec_to_num(const char *str);
static inline u64 str_hex_to_num(const char *str);
static inline u64 str_bin_to_num(const char *str);
static inline u64 str_oct_to_num(const char *str);

Alc_Ast *parse_expr(Alc_Parser *p, b8 is_toplevel)
{
  return pratt_parse(p, is_toplevel, 0, false);
}

Alc_Ast *parse_stmt_expr(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  Alc_Ast *expr = parse_expr(p, true);
  _VERIFY_AST(expr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_SEMICOLON);

  p->pos++;

  Alc_Ast *stmt_expr = alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  stmt_expr->STMT_EXPR.expression = expr;
  stmt_expr->pos = expr->pos;
  stmt_expr->kind = ALC_AST_KIND_STMT_EXPR;
  return stmt_expr;
}

static Alc_Ast *pratt_parse(Alc_Parser *p, b8 is_toplevel, u8 min_prec, b8 has_assign)
{
  _VERIFY_POS(p, p->pos);

  Alc_Ast *lhs = parse_operands_or_prefix(p);
  _VERIFY_AST(lhs);

  while (p->pos < p->tokens_num) {
    usize saved_parser_pos = p->pos;

    Alc_Ast *operator = parse_operator(p);
    if (operator == nullptr) {
      p->pos = saved_parser_pos;
      break;
    }

    u8 prec = get_precedence(operator->kind);
    if (prec <= min_prec) {
      p->pos = saved_parser_pos;
      break;
    }

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
      if ALC_UNLIKELY (has_assign) {
        Alc_Parser_Error two_assign_operators_error = {
          .pos = operator->pos,
          .len = get_operator_length(operator->kind),
          .type = ALC_PARSER_ERROR_TYPE_TWO_ASSIGN_OPERATORS_IN_EXPRESSION,
        };
        add_error(p, two_assign_operators_error);
      } else if ALC_UNLIKELY (!is_toplevel) {
        Alc_Parser_Error assign_in_non_toplevel_error = {
          .pos = operator->pos,
          .len = get_operator_length(operator->kind),
          .type = ALC_PARSER_ERROR_TYPE_ASSIGN_OPERATOR_IN_NON_TOPLEVEL_EXPRESSION,
        };
        add_error(p, assign_in_non_toplevel_error);
      }
      has_assign = true;
    }
    default:
      break;
    }

    Alc_Ast *rhs = pratt_parse(p, is_toplevel, prec, has_assign);
    _VERIFY_AST(rhs);

    Alc_Ast *expr = alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
    expr->EXPR.lhs = lhs;
    expr->EXPR.rhs = rhs;
    expr->EXPR.operator = operator;
    expr->pos = lhs->pos;
    expr->kind = ALC_AST_KIND_EXPR;
    lhs = expr;
  }

  return lhs;
}

static u8 get_precedence(Alc_Ast_Kind op_kind)
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

static usize get_operator_length(Alc_Ast_Kind op_kind)
{
  switch (op_kind) {
  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_SHLEQ:
  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_SHREQ:
    return 3;

  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_SHL:
  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_SHR:
  case ALC_AST_KIND_EXPR_OPERATOR_COMPARE_EQ:
  case ALC_AST_KIND_EXPR_OPERATOR_COMPARE_NOTEQ:
  case ALC_AST_KIND_EXPR_OPERATOR_COMPARE_LTHANEQ:
  case ALC_AST_KIND_EXPR_OPERATOR_COMPARE_GTHANEQ:
  case ALC_AST_KIND_EXPR_OPERATOR_BOOLEAN_AND:
  case ALC_AST_KIND_EXPR_OPERATOR_BOOLEAN_OR:
  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_ADDEQ:
  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_SUBEQ:
  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_MULEQ:
  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_DIVEQ:
  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_MODEQ:
  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_ANDEQ:
  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_OREQ:
  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_XOREQ:
    return 2;

  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_ADD:
  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_SUB:
  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_MUL:
  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_DIV:
  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_MOD:
  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_AND:
  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_OR:
  case ALC_AST_KIND_EXPR_OPERATOR_BINARY_XOR:
  case ALC_AST_KIND_EXPR_OPERATOR_COMPARE_LTHAN:
  case ALC_AST_KIND_EXPR_OPERATOR_COMPARE_GTHAN:
  case ALC_AST_KIND_EXPR_OPERATOR_ASSIGN_EQ:
  case ALC_AST_KIND_EXPR_OPERATOR_PREFIX_NOT:
  case ALC_AST_KIND_EXPR_OPERATOR_PREFIX_BOOLEAN_NOT:
  case ALC_AST_KIND_EXPR_OPERATOR_PREFIX_NEGATIVE:
  case ALC_AST_KIND_EXPR_OPERATOR_PREFIX_DEREFERENCE:
  case ALC_AST_KIND_EXPR_OPERATOR_PREFIX_ADDRESS:
    return 1;

  default:
    ALC_NOREACH();
  }
}

static Alc_Ast *parse_operator(Alc_Parser *p)
{
  usize pos = p->pos;

#define _GEN_AND_ADVANCE(_name, _type, _adv)                                            \
  {                                                                                     \
    Alc_Ast *__alc__##_name = alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast)); \
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

  Alc_Token_Type toktype = p->tokens[p->pos].type;

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

static Alc_Ast *parse_operand(Alc_Parser *p)
{
  Alc_Ast *base = parse_operand_base(p);
  _VERIFY_AST(base);
  return parse_post(p, base);
}

static Alc_Ast *parse_post(Alc_Parser *p, Alc_Ast *ast)
{
  while (p->pos < p->tokens_num) {
    Alc_Token *cur = &p->tokens[p->pos];

    if (cur->type == ALC_TOKEN_TYPE_LBRACK) {
      // Index into array

      p->pos++;

      Alc_Ast *index_expr = parse_expr(p, false);
      _VERIFY_AST(index_expr);

      _VERIFY_POS(p, p->pos);
      _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RBRACK);

      p->pos++;

      Alc_Ast *array_ast = alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
      array_ast->EXPR_OPERAND_ARRAY_ELEMENT.array = ast;
      array_ast->EXPR_OPERAND_ARRAY_ELEMENT.index_expression = index_expr;
      array_ast->pos = ast->pos;
      array_ast->kind = ALC_AST_KIND_EXPR_OPERAND_ARRAY_ELEMENT;

      ast = array_ast;
    } else if (cur->type == ALC_TOKEN_TYPE_LPAREN) {
      // Call

      Alc_Vector(Alc_Ast *) arguments_v = parse_call_arguments(p);
      if ALC_UNLIKELY (arguments_v == nullptr)
        return nullptr;

      Alc_Ast *call_ast = alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
      call_ast->EXPR_OPERAND_CALL.base = ast;
      call_ast->EXPR_OPERAND_CALL.arguments =
        alc_vector_to_array(arguments_v, &call_ast->EXPR_OPERAND_CALL.arguments_num);
      call_ast->pos = ast->pos;
      call_ast->kind = ALC_AST_KIND_EXPR_OPERAND_CALL;

      alc_vector_destroy(arguments_v);

      ast = call_ast;
    } else
      break;
  }

  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_PERIOD) {
    p->pos++;

    _VERIFY_POS(p, p->pos);
    if (p->tokens[p->pos].type == ALC_TOKEN_TYPE_NUMBER) {
      u64 index_number = str_dec_to_num(p->tokens[p->pos].value);
      p->pos++;

      Alc_Ast *access_field_token_ast = alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
      access_field_token_ast->EXPR_OPERAND_ACCESS_FIELD_TUPLE.tuple = ast;
      access_field_token_ast->EXPR_OPERAND_ACCESS_FIELD_TUPLE.index = index_number;
      access_field_token_ast->pos = ast->pos;
      access_field_token_ast->kind = ALC_AST_KIND_EXPR_OPERAND_ACCESS_FIELD_TUPLE;

      return access_field_token_ast;
    }

    Alc_Ast *accessed = parse_operand_identifier(p);
    _VERIFY_AST(accessed);
    accessed = parse_post(p, accessed);

    Alc_Ast *access_ast = alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
    access_ast->EXPR_OPERAND_ACCESS.from = ast;
    access_ast->EXPR_OPERAND_ACCESS.what = accessed;
    access_ast->pos = ast->pos;
    access_ast->kind = ALC_AST_KIND_EXPR_OPERAND_ACCESS;

    return access_ast;
  }

  return ast;
}

static Alc_Ast *parse_operand_base(Alc_Parser *p)
{
  _VERIFY_POS(p, p->pos);

  Alc_Token *tok = &p->tokens[p->pos];
  switch (tok->type) {
  case ALC_TOKEN_TYPE_ID: {
    const char *value = tok->value;
    if (strcmp(value, "sizeof") == 0)
      return parse_sizeof(p);
    else if (strcmp(value, "alignof") == 0)
      return parse_alignof(p);
    else if (strcmp(value, "offsetof") == 0)
      return parse_offsetof(p);
    else if (strcmp(value, "cast") == 0)
      return parse_cast(p);
    else
      return parse_id_operand(p);
  }

  case ALC_TOKEN_TYPE_NUMBER:
  case ALC_TOKEN_TYPE_NUMBER_BIN:
  case ALC_TOKEN_TYPE_NUMBER_OCT:
  case ALC_TOKEN_TYPE_NUMBER_HEX: {
    u64 value = str_to_num(tok->value, tok->type);
    usize pos = p->pos++;

    char *typespec = parse_typespec(p);

    Alc_Ast *number_ast = alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
    number_ast->EXPR_OPERAND_NUMBER.value = value;
    number_ast->EXPR_OPERAND_NUMBER.typespec = typespec;
    number_ast->pos = pos;
    number_ast->kind = ALC_AST_KIND_EXPR_OPERAND_NUMBER;

    return number_ast;
  }

  case ALC_TOKEN_TYPE_NUMBER_FLOAT: {
    f64 value = atof(tok->value);
    usize pos = p->pos++;

    char *typespec = parse_typespec(p);

    Alc_Ast *number_float_ast = alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
    number_float_ast->EXPR_OPERAND_NUMBER_FLOAT.value = value;
    number_float_ast->EXPR_OPERAND_NUMBER_FLOAT.typespec = typespec;
    number_float_ast->pos = pos;
    number_float_ast->kind = ALC_AST_KIND_EXPR_OPERAND_NUMBER_FLOAT;

    return number_float_ast;
  }

  case ALC_TOKEN_TYPE_STRING: {
    const char *content = tok->value;
    usize content_len = strlen(content) + 1;

    usize pos = p->pos++;

    char *typespec = parse_typespec(p);

    Alc_Ast *string_ast =
      alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + (sizeof(char) * content_len));
    string_ast->EXPR_OPERAND_STRING.content = (char *)string_ast + sizeof(Alc_Ast);
    string_ast->EXPR_OPERAND_STRING.typespec = typespec;
    string_ast->pos = pos;
    string_ast->kind = ALC_AST_KIND_EXPR_OPERAND_STRING;
    memcpy(string_ast->EXPR_OPERAND_STRING.content, content, sizeof(char) * content_len);

    return string_ast;
  }

    // Pasted from "case ALC_TOKEN_TYPE_STRING" above
  case ALC_TOKEN_TYPE_SYMBOL: {
    const char *content = tok->value;
    usize content_len = strlen(content) + 1;

    usize pos = p->pos++;

    char *typespec = parse_typespec(p);

    Alc_Ast *symbol_ast =
      alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + (sizeof(char) * content_len));
    symbol_ast->EXPR_OPERAND_SYMBOL.content = (char *)symbol_ast + sizeof(Alc_Ast);
    symbol_ast->EXPR_OPERAND_SYMBOL.typespec = typespec;
    symbol_ast->pos = pos;
    symbol_ast->kind = ALC_AST_KIND_EXPR_OPERAND_SYMBOL;
    memcpy(symbol_ast->EXPR_OPERAND_SYMBOL.content, content, sizeof(char) * content_len);

    return symbol_ast;
  }

  case ALC_TOKEN_TYPE_LPAREN: {
    p->pos++;
    Alc_Ast *expr = parse_expr(p, false);
    _VERIFY_AST(expr);

    _VERIFY_POS(p, p->pos);
    _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RPAREN);

    p->pos++;

    return expr;
  }

  default: {
    Alc_Vector(Alc_Token_Type) expected_v = alc_vector_reserve(Alc_Token_Type, 9);
    alc_vector_push(expected_v, ALC_TOKEN_TYPE_ID);
    alc_vector_push(expected_v, ALC_TOKEN_TYPE_NUMBER);
    alc_vector_push(expected_v, ALC_TOKEN_TYPE_NUMBER_BIN);
    alc_vector_push(expected_v, ALC_TOKEN_TYPE_NUMBER_OCT);
    alc_vector_push(expected_v, ALC_TOKEN_TYPE_NUMBER_HEX);
    alc_vector_push(expected_v, ALC_TOKEN_TYPE_NUMBER_FLOAT);
    alc_vector_push(expected_v, ALC_TOKEN_TYPE_STRING);
    alc_vector_push(expected_v, ALC_TOKEN_TYPE_SYMBOL);
    alc_vector_push(expected_v, ALC_TOKEN_TYPE_LPAREN);
    add_error_unexpected_token_v(p, p->pos++, expected_v);
    return nullptr;
  }
  }
}

static Alc_Ast *parse_id_operand(Alc_Parser *p)
{
  if (is_package(p))
    return parse_operand_package(p);
  return parse_operand_identifier(p);
}

static Alc_Ast *parse_operand_package(Alc_Parser *p)
{
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

  Alc_Ast *symbol = parse_operand_base(p);
  _VERIFY_AST(symbol);

  Alc_Ast *package_ast =
    alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + (sizeof(char) * name_len));
  package_ast->EXPR_OPERAND_PACKAGE.name = (char *)package_ast + sizeof(Alc_Ast);
  package_ast->EXPR_OPERAND_PACKAGE.symbol = symbol;
  package_ast->pos = pos;
  package_ast->kind = ALC_AST_KIND_EXPR_OPERAND_PACKAGE;
  memcpy(package_ast->EXPR_OPERAND_PACKAGE.name, name, sizeof(char) * name_len);

  return package_ast;
}

static Alc_Ast *parse_operand_identifier(Alc_Parser *p)
{
  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  usize pos = p->pos;
  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  p->pos++;

  if (p->pos + 1 < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_EXCLMARK &&
      p->tokens[p->pos + 1].type == ALC_TOKEN_TYPE_LPAREN) {
    Alc_Ast *generic_type_list = parse_generic_type_list(p);
    _VERIFY_AST(generic_type_list);

    Alc_Ast *operand_id_generic =
      alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + (sizeof(char) * name_len));
    operand_id_generic->EXPR_OPERAND_IDENTIFIER_GENERIC.name =
      (char *)operand_id_generic + sizeof(Alc_Ast);
    operand_id_generic->EXPR_OPERAND_IDENTIFIER_GENERIC.generic_type_list = generic_type_list;
    operand_id_generic->pos = pos;
    operand_id_generic->kind = ALC_AST_KIND_EXPR_OPERAND_IDENTIFIER_GENERIC;
    memcpy(operand_id_generic->EXPR_OPERAND_IDENTIFIER_GENERIC.name, name, sizeof(char) * name_len);
    return operand_id_generic;
  }

  Alc_Ast *operand_id =
    alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + (sizeof(char) * name_len));
  operand_id->EXPR_OPERAND_IDENTIFIER.name = (char *)operand_id + sizeof(Alc_Ast);
  operand_id->pos = pos;
  operand_id->kind = ALC_AST_KIND_EXPR_OPERAND_IDENTIFIER;
  memcpy(operand_id->EXPR_OPERAND_IDENTIFIER.name, name, sizeof(char) * name_len);
  return operand_id;
}

static Alc_Ast *parse_sizeof(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);
  _VERIFY_VALUE(p, p->pos, "sizeof");

  usize pos = p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_LPAREN);

  p->pos++;

  Alc_Ast *type = parse_type(p);
  _VERIFY_AST(type);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RPAREN);

  p->pos++;

  Alc_Ast *sizeof_ast = alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  sizeof_ast->EXPR_OPERAND_SIZE_OF.type = type;
  sizeof_ast->pos = pos;
  sizeof_ast->kind = ALC_AST_KIND_EXPR_OPERAND_SIZE_OF;
  return sizeof_ast;
}

static Alc_Ast *parse_alignof(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);
  _VERIFY_VALUE(p, p->pos, "alignof");

  usize pos = p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_LPAREN);

  p->pos++;

  Alc_Ast *expr = parse_expr(p, false);
  _VERIFY_AST(expr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RPAREN);

  p->pos++;

  Alc_Ast *alignof_ast = alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  alignof_ast->EXPR_OPERAND_ALIGN_OF.expression = expr;
  alignof_ast->pos = pos;
  alignof_ast->kind = ALC_AST_KIND_EXPR_OPERAND_ALIGN_OF;
  return alignof_ast;
}

static Alc_Ast *parse_offsetof(Alc_Parser *p)
{
  usize pos = p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_LPAREN);

  p->pos++;

  Alc_Ast *base_structure = parse_type_raw(p);
  _VERIFY_AST(base_structure);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COMMA);

  p->pos++;

  // Maybe should do it in other way to make analysis easier.
  Alc_Ast *field_expression = parse_expr(p, false);
  _VERIFY_AST(field_expression);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RPAREN);

  p->pos++;

  Alc_Ast *offsetof_ast = alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  offsetof_ast->EXPR_OPERAND_OFFSET_OF.base_structure = base_structure;
  offsetof_ast->EXPR_OPERAND_OFFSET_OF.field_expression = field_expression;
  offsetof_ast->pos = pos;
  offsetof_ast->kind = ALC_AST_KIND_EXPR_OPERAND_OFFSET_OF;
  return offsetof_ast;
}

static Alc_Ast *parse_cast(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  usize pos = p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_LPAREN);

  p->pos++;

  Alc_Ast *type = parse_type(p);
  _VERIFY_AST(type);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RPAREN);

  p->pos++;

  Alc_Ast *expr = parse_expr(p, false);
  _VERIFY_AST(expr);

  Alc_Ast *cast_ast = alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  cast_ast->EXPR_OPERAND_CAST_TO.type = type;
  cast_ast->EXPR_OPERAND_CAST_TO.expression = expr;
  cast_ast->pos = pos;
  cast_ast->kind = ALC_AST_KIND_EXPR_OPERAND_CAST_TO;
  return cast_ast;
}

static Alc_Vector(Alc_Ast *) parse_call_arguments(Alc_Parser *p)
{
  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_LPAREN);

  p->pos++;

  Alc_Vector(Alc_Ast *) arguments_v = alc_vector_create(Alc_Ast *);
  b8 first = true;
  while (p->pos < p->tokens_num && p->tokens[p->pos].type != ALC_TOKEN_TYPE_RPAREN) {
    if (!first) {
      _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COMMA, { alc_vector_destroy(arguments_v); });
      p->pos++;

      _VERIFY_POS(p, p->pos, { alc_vector_destroy(arguments_v); });
    }

    Alc_Ast *argument = p->tokens[p->pos].type == ALC_TOKEN_TYPE_PERIOD ?
                          parse_explicit_call_argument(p) :
                          parse_expr(p, false);
    _VERIFY_AST(argument, { alc_vector_destroy(arguments_v); });

    alc_vector_push(arguments_v, argument);

    first = false;
  }

  _VERIFY_POS(p, p->pos, { alc_vector_destroy(arguments_v); });
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RPAREN, { alc_vector_destroy(arguments_v); });

  p->pos++;

  return arguments_v;
}

static Alc_Ast *parse_explicit_call_argument(Alc_Parser *p)
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

  Alc_Ast *expr = parse_expr(p, false);
  _VERIFY_AST(expr);

  Alc_Ast *explicit_call_argument_ast =
    alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + (sizeof(char) * name_len));
  explicit_call_argument_ast->EXPLICIT_CALL_ARGUMENT.name =
    (char *)explicit_call_argument_ast + sizeof(Alc_Ast);
  explicit_call_argument_ast->EXPLICIT_CALL_ARGUMENT.expression = expr;
  explicit_call_argument_ast->pos = pos;
  explicit_call_argument_ast->kind = ALC_AST_KIND_EXPLICIT_CALL_ARGUMENT;
  memcpy(explicit_call_argument_ast->EXPLICIT_CALL_ARGUMENT.name, name, sizeof(char) * name_len);

  return explicit_call_argument_ast;
}

static Alc_Ast *parse_prefix_expr(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);

  usize pos = p->pos;

  Alc_Ast_Kind kind;
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

  Alc_Ast *operand = parse_expr(p, false);
  _VERIFY_AST(operand);

  Alc_Ast *operator_ast = alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  operator_ast->pos = pos;
  operator_ast->kind = kind;

  Alc_Ast *prefix_expr_ast = alc_alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  prefix_expr_ast->PREFIX_EXPR.operand = operand;
  prefix_expr_ast->PREFIX_EXPR.operator = operator_ast;
  prefix_expr_ast->pos = operand->pos;
  prefix_expr_ast->kind = ALC_AST_KIND_PREFIX_EXPR;

  return prefix_expr_ast;
}

static Alc_Ast *parse_operands_or_prefix(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);

  switch (p->tokens[p->pos].type) {
  case ALC_TOKEN_TYPE_ASTERISK:
  case ALC_TOKEN_TYPE_EXCLMARK:
  case ALC_TOKEN_TYPE_TILDE:
  case ALC_TOKEN_TYPE_MINUS:
  case ALC_TOKEN_TYPE_AMPERSAND:
    return parse_prefix_expr(p);
  default:
    return parse_operand(p);
  }
}

static char *parse_typespec(Alc_Parser *p)
{
  if (p->pos >= p->tokens_num || p->tokens[p->pos].type != ALC_TOKEN_TYPE_ID)
    return nullptr;

  const char *typespec = p->tokens[p->pos].value;
  usize typespec_len = strlen(typespec) + 1;

  char *out = alc_alloc_arena_allocate_aligned(&ctx()->arena, typespec_len, 1);
  for (char *p = out; *typespec; typespec++, p++)
    *p = tolower(*typespec);

  p->pos++;

  return out;
}

static inline b8 is_package(Alc_Parser *p)
{
  return p->pos + 2 < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_ID &&
         p->tokens[p->pos + 1].type == ALC_TOKEN_TYPE_COLON &&
         p->tokens[p->pos + 2].type == ALC_TOKEN_TYPE_COLON;
}

static inline u64 str_to_num(const char *str, Alc_Token_Type numtype)
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
