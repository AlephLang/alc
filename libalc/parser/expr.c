#include "alc/ast.h"
#include "alc/defs.h"
#include "alc/parser.h"
#include "alc/token.h"
#include "alc/vector.h"
#include "allocs/alloc_arena.h"
#include "global.h"
#include "parser/parser_private.h"
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

static Alc_Ast *pratt_parse(Alc_Parser *p, b8 is_toplevel, u8 min_prec, b8 has_assign);
static u8 get_precedence(Alc_Ast_Kind op_kind);
static usize get_operator_length(Alc_Ast_Kind op_kind);
static Alc_Ast *parse_operator(Alc_Parser *p);
static b8 is_namespace(const Alc_Parser *p);
static b8 is_generic_call_or_namespace(const Alc_Parser *p);
static inline b8 is_generic_call(const Alc_Parser *p)
{
  return is_generic_call_or_namespace(p);
}
static b8 is_call(const Alc_Parser *p);
static Alc_Ast **parse_call_arguments(Alc_Parser *p, usize *out_n);
static Alc_Ast *parse_explicit_call_argument(Alc_Parser *p);
static Alc_Ast *parse_operands(Alc_Parser *p);
static Alc_Ast *parse_namespaces_and_identifier_operands(Alc_Parser *p);
static Alc_Ast *parse_only_operands(Alc_Parser *p);
static Alc_Ast *parse_post(Alc_Parser *p, Alc_Ast *ast);
static Alc_Ast *parse_identifier(Alc_Parser *p);
static Alc_Ast *parse_call(Alc_Parser *p);
static Alc_Ast *parse_generic_call(Alc_Parser *p);
static Alc_Ast *parse_namespace(Alc_Parser *p);
static Alc_Ast *parse_generic_call_or_namespace(Alc_Parser *p);
static Alc_Ast *parse_sizeof(Alc_Parser *p);
static Alc_Ast *parse_alignof(Alc_Parser *p);
static Alc_Ast *parse_cast(Alc_Parser *p);
static Alc_Ast *parse_prefix_expr(Alc_Parser *p);
static Alc_Ast *parse_operands_or_prefix(Alc_Parser *p);
static char *parse_typespec(Alc_Parser *p, b8 prev_has_whitespace_after);
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

  Alc_Ast *stmt_expr = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  stmt_expr->data.STMT_EXPR.expression = expr;
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
    if (operator == nullptr)
      break;

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

    Alc_Ast *expr = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
    expr->data.EXPR.lhs = lhs;
    expr->data.EXPR.rhs = rhs;
    expr->data.EXPR.operator = operator;
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

#define _GEN_AND_ADVANCE(_name, _type, _adv)                                        \
  {                                                                                 \
    Alc_Ast *__alc__##_name = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast)); \
    __alc__##_name->pos = pos;                                                      \
    __alc__##_name->kind = ALC_AST_KIND_EXPR_OPERATOR_##_type;                      \
    p->pos += (_adv);                                                               \
    return __alc__##_name;                                                          \
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

static b8 is_namespace(const Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  Alc_Token *tok1 = peek(p, 0), *tok2 = peek(p, 1), *tok3 = peek(p, 2);
  return tok1 != nullptr && tok2 != nullptr && tok3 != nullptr && !tok1->has_whitespace_after &&
         !tok2->has_whitespace_after && !tok3->has_whitespace_after &&
         tok1->type == ALC_TOKEN_TYPE_ID && tok2->type == ALC_TOKEN_TYPE_COLON &&
         tok3->type == tok2->type;
}

static b8 is_generic_call_or_namespace(const Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  Alc_Token *tok1 = peek(p, 0), *tok2 = peek(p, 1), *tok3 = peek(p, 2);
  return tok1 != nullptr && tok2 != nullptr && tok3 != nullptr && !tok1->has_whitespace_after &&
         !tok2->has_whitespace_after && tok1->type == ALC_TOKEN_TYPE_ID &&
         tok2->type == ALC_TOKEN_TYPE_EXCLMARK && tok3->type == ALC_TOKEN_TYPE_LARROW;
}

static b8 is_call(const Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  Alc_Token *tok1 = peek(p, 0), *tok2 = peek(p, 1);
  return tok1 != nullptr && tok2 != nullptr && tok1->type == ALC_TOKEN_TYPE_ID &&
         tok2->type == ALC_TOKEN_TYPE_LPAREN;
}

static Alc_Ast **parse_call_arguments(Alc_Parser *p, usize *out_n)
{
  ALC_ASSUME(p != nullptr);
  ALC_ASSUME(out_n != nullptr);

  *out_n = -1;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_LPAREN);

  p->pos++;

  b8 first = true;
  Alc_Vector(Alc_Ast *) arguments = alc_vector_create(Alc_Ast *);
  while (p->pos < p->tokens_num) {
    if (p->tokens[p->pos].type == ALC_TOKEN_TYPE_RPAREN)
      break;

    if (!first) {
      _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_COMMA, { alc_vector_destroy(arguments); });
      p->pos++;
    }

    Alc_Ast *argument;
    switch (p->tokens[p->pos].type) {
    case ALC_TOKEN_TYPE_LCBRACK: {
      argument = parse_initlist(p);
    } break;

    case ALC_TOKEN_TYPE_ID: {
      if (p->pos + 1 < p->tokens_num && p->tokens[p->pos + 1].type == ALC_TOKEN_TYPE_EQ) {
        argument = parse_explicit_call_argument(p);
        break;
      }

      argument = parse_expr(p, false);
    } break;

    default: {
      argument = parse_expr(p, false);
    } break;
    }

    _VERIFY_AST(argument, { alc_vector_destroy(arguments); });

    alc_vector_push(arguments, argument);

    first = false;
  }

  _VERIFY_POS(p, p->pos, { alc_vector_destroy(arguments); });
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RPAREN, { alc_vector_destroy(arguments); });

  p->pos++;

  Alc_Ast **arr = alc_vector_to_array(arguments, out_n);
  alc_vector_destroy(arguments);
  return arr;
}

static Alc_Ast *parse_explicit_call_argument(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  usize pos = p->pos;

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  p->pos++;

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_EQ);

  p->pos++;

  _VERIFY_POS(p, p->pos);
  Alc_Ast *expr = p->tokens[p->pos].type == ALC_TOKEN_TYPE_LCBRACK ? parse_initlist(p) :
                                                                     parse_expr(p, false);
  _VERIFY_AST(expr);

  Alc_Ast *explicit_call_argument_ast =
    alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + sizeof(char) * name_len);
  explicit_call_argument_ast->data.EXPLICIT_CALL_ARGUMENT.name =
    (char *)explicit_call_argument_ast + sizeof(Alc_Ast);
  explicit_call_argument_ast->data.EXPLICIT_CALL_ARGUMENT.expression = expr;
  explicit_call_argument_ast->pos = pos;
  explicit_call_argument_ast->kind = ALC_AST_KIND_EXPLICIT_CALL_ARGUMENT;
  memcpy(explicit_call_argument_ast->data.EXPLICIT_CALL_ARGUMENT.name, name,
         sizeof(char) * name_len);
  return explicit_call_argument_ast;
}

static Alc_Ast *parse_operands(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);
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
    usize pos = p->pos;
    u64 value = str_to_num(p->tokens[p->pos].value, p->tokens[p->pos].type);

    b8 has_ws = p->tokens[p->pos].has_whitespace_after;
    p->pos++;

    char *typespec = parse_typespec(p, has_ws);

    Alc_Ast *number_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
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

    Alc_Ast *number_float_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
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
    Alc_Ast *string_ast =
      alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + sizeof(char) * content_len);
    string_ast->data.EXPR_OPERAND_STRING.content = (char *)string_ast + sizeof(Alc_Ast);
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
    Alc_Ast *symbol_ast =
      alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + sizeof(char) * content_len);
    symbol_ast->data.EXPR_OPERAND_SYMBOL.content = (char *)symbol_ast + sizeof(Alc_Ast);
    symbol_ast->data.EXPR_OPERAND_SYMBOL.typespec = typespec;
    symbol_ast->pos = pos;
    symbol_ast->kind = ALC_AST_KIND_EXPR_OPERAND_SYMBOL;
    memcpy(symbol_ast->data.EXPR_OPERAND_SYMBOL.content, content, content_len);
    return parse_post(p, symbol_ast);
  }

  case ALC_TOKEN_TYPE_LPAREN: {
    p->pos++;
    Alc_Ast *expr = parse_expr(p, false);
    _VERIFY_AST(expr);

    _VERIFY_POS(p, p->pos);
    _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RPAREN);

    p->pos++;

    return parse_post(p, expr);
  }

  default: {
    Alc_Vector(Alc_Token_Type) expected_v = alc_vector_reserve(Alc_Token_Type, 9);
    alc_vector_push(expected_v, ALC_TOKEN_TYPE_ID);
    alc_vector_push(expected_v, ALC_TOKEN_TYPE_NUMBER);
    alc_vector_push(expected_v, ALC_TOKEN_TYPE_NUMBER_HEX);
    alc_vector_push(expected_v, ALC_TOKEN_TYPE_NUMBER_BIN);
    alc_vector_push(expected_v, ALC_TOKEN_TYPE_NUMBER_OCT);
    alc_vector_push(expected_v, ALC_TOKEN_TYPE_NUMBER_FLOAT);
    alc_vector_push(expected_v, ALC_TOKEN_TYPE_STRING);
    alc_vector_push(expected_v, ALC_TOKEN_TYPE_SYMBOL);
    alc_vector_push(expected_v, ALC_TOKEN_TYPE_LPAREN);
    add_error_unexpected_token_v(p, p->pos++, expected_v);
    return nullptr;
  }
  }
}

static Alc_Ast *parse_namespaces_and_identifier_operands(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  Alc_Ast *ast = is_generic_call_or_namespace(p) ? parse_generic_call_or_namespace(p) :
                 is_namespace(p)                 ? parse_namespace(p) :
                 is_call(p)                      ? parse_call(p) :
                                                   parse_identifier(p);
  _VERIFY_AST(ast);

  return parse_post(p, ast);
}

static Alc_Ast *parse_only_operands(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  Alc_Ast *ast = is_generic_call(p) ? parse_generic_call(p) :
                 is_call(p)         ? parse_call(p) :
                                      parse_identifier(p);
  _VERIFY_AST(ast);

  return parse_post(p, ast);
}

static Alc_Ast *parse_post(Alc_Parser *p, Alc_Ast *ast)
{
  ALC_ASSUME(p != nullptr);
  ALC_ASSUME(ast != nullptr);

  while (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_LBRACK) {
    usize pos = p->pos++;

    Alc_Ast *index_expr = parse_expr(p, false);
    _VERIFY_AST(index_expr);

    _VERIFY_POS(p, p->pos);
    _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_RBRACK);

    p->pos++;

    Alc_Ast *array = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
    array->data.EXPR_OPERAND_ARRAY_ELEMENT.array = ast;
    array->data.EXPR_OPERAND_ARRAY_ELEMENT.index_expression = index_expr;
    array->pos = pos;
    array->kind = ALC_AST_KIND_EXPR_OPERAND_ARRAY_ELEMENT;
    ast = array;
  }

  if (p->pos < p->tokens_num && p->tokens[p->pos].type == ALC_TOKEN_TYPE_PERIOD) {
    p->pos++;

    Alc_Ast *member = parse_only_operands(p);
    _VERIFY_AST(member);

    Alc_Ast *access_member = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
    access_member->data.EXPR_OPERAND_ACCESS_MEMBER.from = ast;
    access_member->data.EXPR_OPERAND_ACCESS_MEMBER.what = member;
    access_member->pos = ast->pos;
    access_member->kind = ALC_AST_KIND_EXPR_OPERAND_ACCESS_MEMBER;
    return access_member;
  }

  return ast;
}

static Alc_Ast *parse_identifier(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  usize pos = p->pos++;

  Alc_Ast *identifier_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + name_len);
  identifier_ast->data.EXPR_OPERAND_IDENTIFIER.name = (char *)identifier_ast + sizeof(Alc_Ast);
  identifier_ast->pos = pos;
  identifier_ast->kind = ALC_AST_KIND_EXPR_OPERAND_IDENTIFIER;
  memcpy(identifier_ast->data.EXPR_OPERAND_IDENTIFIER.name, name, name_len);

  return identifier_ast;
}

static Alc_Ast *parse_call(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  usize pos = p->pos++;

  usize arguments_num;
  Alc_Ast **arguments_array = parse_call_arguments(p, &arguments_num);
  if ALC_UNLIKELY (arguments_num == (usize)-1)
    return nullptr;

  Alc_Ast *call_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + name_len);
  call_ast->data.EXPR_OPERAND_CALL.callee_name = (char *)call_ast + sizeof(Alc_Ast);
  call_ast->data.EXPR_OPERAND_CALL.arguments = arguments_array;
  call_ast->data.EXPR_OPERAND_CALL.arguments_num = arguments_num;
  call_ast->pos = pos;
  call_ast->kind = ALC_AST_KIND_EXPR_OPERAND_CALL;
  memcpy(call_ast->data.EXPR_OPERAND_CALL.callee_name, name, name_len);
  return call_ast;
}

static Alc_Ast *parse_generic_call(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  usize pos = p->pos++;

  Alc_Ast *generic_type_list = parse_generic_type_list(p);
  _VERIFY_AST(generic_type_list);

  usize arguments_num;
  Alc_Ast **arguments_array = parse_call_arguments(p, &arguments_num);
  if ALC_UNLIKELY (arguments_num == (usize)-1)
    return nullptr;

  Alc_Ast *generic_call_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + name_len);
  generic_call_ast->data.EXPR_OPERAND_GENERIC_CALL.callee_name =
    (char *)generic_call_ast + sizeof(Alc_Ast);
  generic_call_ast->data.EXPR_OPERAND_GENERIC_CALL.generic_type_list = generic_type_list;
  generic_call_ast->data.EXPR_OPERAND_GENERIC_CALL.arguments = arguments_array;
  generic_call_ast->data.EXPR_OPERAND_GENERIC_CALL.arguments_num = arguments_num;
  generic_call_ast->pos = pos;
  generic_call_ast->kind = ALC_AST_KIND_EXPR_OPERAND_CALL;
  memcpy(generic_call_ast->data.EXPR_OPERAND_GENERIC_CALL.callee_name, name, name_len);
  return generic_call_ast;
}

static Alc_Ast *parse_namespace(Alc_Parser *p)
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

  Alc_Ast *subobject = parse_namespaces_and_identifier_operands(p);
  _VERIFY_AST(subobject);

  Alc_Ast *namespace_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  namespace_ast->data.NAMESPACE.name = (char *)namespace_ast + sizeof(Alc_Ast);
  namespace_ast->data.NAMESPACE.subobject = subobject;
  namespace_ast->pos = pos;
  namespace_ast->kind = ALC_AST_KIND_NAMESPACE;
  memcpy(namespace_ast->data.NAMESPACE.name, name, name_len);
  return namespace_ast;
}

static Alc_Ast *parse_generic_call_or_namespace(Alc_Parser *p)
{
  ALC_ASSUME(p != nullptr);

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_ID);

  const char *name = p->tokens[p->pos].value;
  usize name_len = strlen(name) + 1;

  usize pos = p->pos++;

  Alc_Ast *generic_type_list = parse_generic_type_list(p);
  _VERIFY_AST(generic_type_list);

  Alc_Token *tok0 = peek(p, -1), *tok1 = peek(p, 0), *tok2 = peek(p, 1);
  if (tok1 != nullptr && tok2 != nullptr && !tok0->has_whitespace_after &&
      !tok1->has_whitespace_after && !tok2->has_whitespace_after &&
      tok1->type == ALC_TOKEN_TYPE_COLON && tok2->type == ALC_TOKEN_TYPE_COLON) {
    p->pos += 2;

    Alc_Ast *subobject = parse_namespaces_and_identifier_operands(p);
    _VERIFY_AST(subobject);

    Alc_Ast *generic_namespace_ast =
      alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + name_len);
    generic_namespace_ast->data.GENERIC_NAMESPACE.name =
      (char *)generic_namespace_ast + sizeof(Alc_Ast);
    generic_namespace_ast->data.GENERIC_NAMESPACE.generic_type_list = generic_type_list;
    generic_namespace_ast->data.GENERIC_NAMESPACE.subobject = subobject;
    generic_namespace_ast->pos = pos;
    generic_namespace_ast->kind = ALC_AST_KIND_GENERIC_NAMESPACE;
    memcpy(generic_namespace_ast->data.GENERIC_NAMESPACE.name, name, name_len);
    return generic_namespace_ast;
  }

  _VERIFY_POS(p, p->pos);
  _VERIFY_TOKEN(p, p->pos, ALC_TOKEN_TYPE_LPAREN);

  usize arguments_num;
  Alc_Ast **arguments_array = parse_call_arguments(p, &arguments_num);
  if ALC_UNLIKELY (arguments_num == (usize)-1)
    return nullptr;

  Alc_Ast *generic_call_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast) + name_len);
  generic_call_ast->data.EXPR_OPERAND_GENERIC_CALL.callee_name =
    (char *)generic_call_ast + sizeof(Alc_Ast);
  generic_call_ast->data.EXPR_OPERAND_GENERIC_CALL.generic_type_list = generic_type_list;
  generic_call_ast->data.EXPR_OPERAND_GENERIC_CALL.arguments = arguments_array;
  generic_call_ast->data.EXPR_OPERAND_GENERIC_CALL.arguments_num = arguments_num;
  generic_call_ast->pos = pos;
  generic_call_ast->kind = ALC_AST_KIND_EXPR_OPERAND_GENERIC_CALL;
  memcpy(generic_call_ast->data.EXPR_OPERAND_GENERIC_CALL.callee_name, name, name_len);
  return generic_call_ast;
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

  Alc_Ast *sizeof_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  sizeof_ast->data.EXPR_OPERAND_SIZE_OF.type = type;
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

  Alc_Ast *alignof_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  alignof_ast->data.EXPR_OPERAND_ALIGN_OF.expression = expr;
  alignof_ast->pos = pos;
  alignof_ast->kind = ALC_AST_KIND_EXPR_OPERAND_ALIGN_OF;
  return alignof_ast;
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

  Alc_Ast *cast_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  cast_ast->data.EXPR_OPERAND_CAST_TO.type = type;
  cast_ast->data.EXPR_OPERAND_CAST_TO.expression = expr;
  cast_ast->pos = pos;
  cast_ast->kind = ALC_AST_KIND_EXPR_OPERAND_CAST_TO;
  return cast_ast;
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

  Alc_Ast *operator_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  operator_ast->pos = pos;
  operator_ast->kind = kind;

  Alc_Ast *prefix_expr_ast = alloc_arena_allocate(&ctx()->arena, sizeof(Alc_Ast));
  prefix_expr_ast->data.PREFIX_EXPR.operand = operand;
  prefix_expr_ast->data.PREFIX_EXPR.operator = operator_ast;
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
    return parse_operands(p);
  }
}

static char *parse_typespec(Alc_Parser *p, b8 prev_has_whitespace_after)
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
