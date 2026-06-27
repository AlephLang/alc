#ifndef __ALC_AST_H__
#define __ALC_AST_H__

#include <alc/defs.h>

// #define ALC_AST_KIND_X(_name)
#define ALC_AST_KINDS                                   \
  ALC_AST_KIND_X(ROOT)                                  \
  ALC_AST_KIND_X(EXPR)                                  \
  ALC_AST_KIND_X(PREFIX_EXPR)                           \
  ALC_AST_KIND_X(MODULE)                                \
  ALC_AST_KIND_X(IMPORT)                                \
  ALC_AST_KIND_X(TYPEDEF)                               \
  ALC_AST_KIND_X(EXTERN_FUNC)                           \
  ALC_AST_KIND_X(EXTERN_VARDECL)                        \
  ALC_AST_KIND_X(QUALIFIER)                             \
  ALC_AST_KIND_X(NONE)                                  \
  ALC_AST_KIND_X(VARIADIC)                              \
  ALC_AST_KIND_X(STRUCT)                                \
  ALC_AST_KIND_X(UNION)                                 \
  ALC_AST_KIND_X(ENUM)                                  \
  ALC_AST_KIND_X(ENUM_ELEMENT)                          \
  ALC_AST_KIND_X(FUNC)                                  \
  ALC_AST_KIND_X(ARGUMENT_LIST)                         \
  ALC_AST_KIND_X(NAMESPACE)                             \
  ALC_AST_KIND_X(SCOPE)                                 \
  ALC_AST_KIND_X(CASE_CHAIN)                            \
  ALC_AST_KIND_X(CASE)                                  \
  ALC_AST_KIND_X(DEFAULT)                               \
  ALC_AST_KIND_X(ATTRIBUTE)                             \
  ALC_AST_KIND_X(ATTRIBUTE_LIST)                        \
  ALC_AST_KIND_X(EXPLICIT_CALL_ARGUMENT)                \
  ALC_AST_KIND_X(LABEL)                                 \
  ALC_AST_KIND_X(STMT_BLOCK)                            \
  ALC_AST_KIND_X(STMT_RETURN)                           \
  ALC_AST_KIND_X(STMT_GOTO)                             \
  ALC_AST_KIND_X(STMT_LABEL)                            \
  ALC_AST_KIND_X(STMT_BREAK)                            \
  ALC_AST_KIND_X(STMT_CONTINUE)                         \
  ALC_AST_KIND_X(STMT_FALLTHROUGH)                      \
  ALC_AST_KIND_X(STMT_WHILE)                            \
  ALC_AST_KIND_X(STMT_FOR)                              \
  ALC_AST_KIND_X(STMT_DO_WHILE)                         \
  ALC_AST_KIND_X(STMT_LOOP)                             \
  ALC_AST_KIND_X(STMT_FOREACH)                          \
  ALC_AST_KIND_X(STMT_EXPR)                             \
  ALC_AST_KIND_X(STMT_SWITCH)                           \
  ALC_AST_KIND_X(STMT_DEFER)                            \
  ALC_AST_KIND_X(STMT_IF)                               \
  ALC_AST_KIND_X(STMT_ELSE)                             \
  ALC_AST_KIND_X(TYPE_PLAIN)                            \
  ALC_AST_KIND_X(TYPE_POINTER)                          \
  ALC_AST_KIND_X(TYPE_ARRAY)                            \
  ALC_AST_KIND_X(TYPE_FUNCTION_POINTER)                 \
  ALC_AST_KIND_X(TYPE_TYPE_OF)                          \
  ALC_AST_KIND_X(VAR_DECL)                              \
  ALC_AST_KIND_X(VAR_DEF)                               \
  ALC_AST_KIND_X(EXPR_OPERAND_IDENTIFIER)               \
  ALC_AST_KIND_X(EXPR_OPERAND_NUMBER)                   \
  ALC_AST_KIND_X(EXPR_OPERAND_NUMBER_FLOAT)             \
  ALC_AST_KIND_X(EXPR_OPERAND_ARRAY_ELEMENT)            \
  ALC_AST_KIND_X(EXPR_OPERAND_CAST_TO)                  \
  ALC_AST_KIND_X(EXPR_OPERAND_CALL)                     \
  ALC_AST_KIND_X(EXPR_OPERAND_GENERIC_CALL)             \
  ALC_AST_KIND_X(EXPR_OPERAND_STRING)                   \
  ALC_AST_KIND_X(EXPR_OPERAND_SYMBOL)                   \
  ALC_AST_KIND_X(EXPR_OPERAND_ACCESS_MEMBER)            \
  ALC_AST_KIND_X(EXPR_OPERAND_SIZE_OF)                  \
  ALC_AST_KIND_X(EXPR_OPERAND_ALIGN_OF)                 \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_ADD)              \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_SUB)              \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_MUL)              \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_DIV)              \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_MOD)              \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_SHL)              \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_SHR)              \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_AND)              \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_OR)               \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_XOR)              \
  ALC_AST_KIND_X(EXPR_OPERATOR_COMPARE_EQ)              \
  ALC_AST_KIND_X(EXPR_OPERATOR_COMPARE_NOTEQ)           \
  ALC_AST_KIND_X(EXPR_OPERATOR_COMPARE_LTHAN)           \
  ALC_AST_KIND_X(EXPR_OPERATOR_COMPARE_GTHAN)           \
  ALC_AST_KIND_X(EXPR_OPERATOR_COMPARE_LTHANEQ)         \
  ALC_AST_KIND_X(EXPR_OPERATOR_COMPARE_GTHANEQ)         \
  ALC_AST_KIND_X(EXPR_OPERATOR_BOOLEAN_AND)             \
  ALC_AST_KIND_X(EXPR_OPERATOR_BOOLEAN_OR)              \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_EQ)               \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_ADDEQ)            \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_SUBEQ)            \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_MULEQ)            \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_DIVEQ)            \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_MODEQ)            \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_SHLEQ)            \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_SHREQ)            \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_ANDEQ)            \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_OREQ)             \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_XOREQ)            \
  ALC_AST_KIND_X(EXPR_OPERATOR_PREFIX_NOT)              \
  ALC_AST_KIND_X(EXPR_OPERATOR_PREFIX_BOOLEAN_NOT)      \
  ALC_AST_KIND_X(EXPR_OPERATOR_PREFIX_NEGATIVE)         \
  ALC_AST_KIND_X(EXPR_OPERATOR_PREFIX_DEREFERENCE)      \
  ALC_AST_KIND_X(EXPR_OPERATOR_PREFIX_ADDRESS)          \
  ALC_AST_KIND_X(INITLIST)                              \
  ALC_AST_KIND_X(INITLIST_ENTRY)                        \
  ALC_AST_KIND_X(INITLIST_ENTRY_EXPLICIT)               \
  ALC_AST_KIND_X(INITLIST_ENTRY_EXPLICIT_ARRAY_ELEMENT) \
  ALC_AST_KIND_X(GENERIC_STRUCT)                        \
  ALC_AST_KIND_X(GENERIC_FUNC)                          \
  ALC_AST_KIND_X(GENERIC_PLACEHOLDER_TYPE_LIST)         \
  ALC_AST_KIND_X(GENERIC_PLACEHOLDER_TYPE)              \
  ALC_AST_KIND_X(GENERIC_TYPE_LIST)                     \
  ALC_AST_KIND_X(GENERIC_TYPE)                          \
  ALC_AST_KIND_X(GENERIC_NAMESPACE)

#define ALC_AST_KIND_FULL_NAME(_name) ALC_AST_KIND_##_name

typedef enum {
#define ALC_AST_KIND_X(_name) ALC_AST_KIND_FULL_NAME(_name),
  ALC_AST_KINDS
#undef ALC_AST_KIND_X
} Alc_Ast_Kind;

typedef enum {
  ALC_AST_FUNCTION_KIND_DEFAULT,
  ALC_AST_FUNCTION_KIND_EXPLICIT,
  ALC_AST_FUNCTION_KIND_EXPORTED,
} Alc_Ast_Function_Kind;

typedef enum {
  ALC_AST_STRUCT_KIND_DEFAULT,
  ALC_AST_STRUCT_KIND_PARTIAL,
} Alc_Ast_Struct_Kind;

typedef struct __Alc_Ast {
  union {
    struct {
      struct __Alc_Ast **toplevel_statements;
      usize toplevel_statements_num;
    } ROOT;
    struct {
      struct __Alc_Ast *lhs;
      struct __Alc_Ast *rhs;
      struct __Alc_Ast *operator;
    } EXPR;
    struct {
      struct __Alc_Ast *operand;
      struct __Alc_Ast *operator;
    } PREFIX_EXPR;
    struct {
      char *name;
      struct __Alc_Ast *submodule;
    } MODULE;
    struct {
      struct __Alc_Ast *module;
    } IMPORT;
    struct {
      char *name;
      struct __Alc_Ast *generic_placeholder_type_list;
      struct __Alc_Ast *aliased_type;
      struct __Alc_Ast *attribute_list;
    } TYPEDEF;
    struct {
      char *name;
      struct __Alc_Ast *argument_list;
      struct __Alc_Ast *return_type;
    } EXTERN_FUNC;
    struct {
      char *name;
      struct __Alc_Ast *type;
    } EXTERN_VARDECL;
    struct {
      char *name;
      struct __Alc_Ast *qualified;
    } QUALIFIER;
    struct {
      char *name;
      struct __Alc_Ast **children;
      usize children_num;
      struct __Alc_Ast *attribute_list;
      Alc_Ast_Struct_Kind kind;
    } STRUCT;
    struct {
      char *name;
      struct __Alc_Ast **children;
      usize children_num;
      struct __Alc_Ast *attribute_list;
    } UNION;
    struct {
      char *name;
      struct __Alc_Ast **elements;
      usize elements_num;
      struct __Alc_Ast *attribute_list;
    } ENUM;
    struct {
      char *name;
      struct __Alc_Ast *expression;
    } ENUM_ELEMENT;
    struct {
      char *name;
      struct __Alc_Ast *argument_list;
      struct __Alc_Ast *return_type;
      struct __Alc_Ast *body;
      struct __Alc_Ast *attribute_list;
      Alc_Ast_Function_Kind kind;
    } FUNC;
    struct {
      struct __Alc_Ast **arguments;
      usize arguments_num;
    } ARGUMENT_LIST;
    struct {
      char *name;
      struct __Alc_Ast *subobject;
    } NAMESPACE;
    struct {
      char *type;
    } SCOPE;
    struct {
      struct __Alc_Ast **cases;
      usize cases_num;
      struct __Alc_Ast *body;
    } CASE_CHAIN;
    struct {
      struct __Alc_Ast *expression;
    } CASE;
    struct {
      char *name;
      struct __Alc_Ast **arguments;
      usize arguments_num;
      // NOTE: This is used to indicate that attribute has parenthesis, in which
      // arguments should be placed (not that it really has arguments).
      b8 has_arguments;
    } ATTRIBUTE;
    struct {
      struct __Alc_Ast **attributes;
      usize attributes_num;
    } ATTRIBUTE_LIST;
    struct {
      char *name;
      struct __Alc_Ast *expression;
    } EXPLICIT_CALL_ARGUMENT;
    struct {
      char *name;
    } LABEL;
    struct {
      struct __Alc_Ast **statements;
      usize statements_num;
    } STMT_BLOCK;
    struct {
      struct __Alc_Ast *expression;
    } STMT_RETURN;
    struct {
      struct __Alc_Ast *label;
    } STMT_GOTO;
    struct {
      struct __Alc_Ast *label;
    } STMT_LABEL;
    struct {
      struct __Alc_Ast *condition;
      struct __Alc_Ast *body;
      struct __Alc_Ast *attribute_list;
    } STMT_WHILE;
    struct {
      struct __Alc_Ast *init_statement;
      struct __Alc_Ast *condition;
      struct __Alc_Ast *expression;
      struct __Alc_Ast *body;
      struct __Alc_Ast *attribute_list;
    } STMT_FOR;
    struct {
      struct __Alc_Ast *condition;
      struct __Alc_Ast *body;
      struct __Alc_Ast *attribute_list;
    } STMT_DO_WHILE;
    struct {
      struct __Alc_Ast *body;
    } STMT_LOOP;
    struct {
      char *item_name;
      struct __Alc_Ast *iterator;
      struct __Alc_Ast *body;
      struct __Alc_Ast *attribute_list;
      char *i_name;
    } STMT_FOREACH;
    struct {
      struct __Alc_Ast *expression;
    } STMT_EXPR;
    struct {
      struct __Alc_Ast *expression;
      struct __Alc_Ast **case_chains;
      usize case_chains_num;
    } STMT_SWITCH;
    struct {
      struct __Alc_Ast *body;
    } STMT_DEFER;
    struct {
      struct __Alc_Ast *condition;
      struct __Alc_Ast *body;
      struct __Alc_Ast *else_statement;
      struct __Alc_Ast *attribute_list;
    } STMT_IF;
    struct {
      struct __Alc_Ast *body;
    } STMT_ELSE;
    struct {
      char *name;
    } TYPE_PLAIN;
    struct {
      struct __Alc_Ast *type;
    } TYPE_POINTER;
    struct {
      struct __Alc_Ast *type;
      struct __Alc_Ast *size_expression;
    } TYPE_ARRAY;
    struct {
      struct __Alc_Ast *argument_list;
      struct __Alc_Ast *return_type;
    } TYPE_FUNCTION_POINTER;
    struct {
      struct __Alc_Ast *expression;
    } TYPE_TYPE_OF;
    struct {
      char *name;
      struct __Alc_Ast *type;
      struct __Alc_Ast *attribute_list;
    } VAR_DECL;
    struct {
      char *name;
      struct __Alc_Ast *type;
      struct __Alc_Ast *expression;
      struct __Alc_Ast *attribute_list;
    } VAR_DEF;
    struct {
      char *name;
    } EXPR_OPERAND_IDENTIFIER;
    struct {
      u64 value;
      char *typespec;
    } EXPR_OPERAND_NUMBER;
    struct {
      f64 value;
      char *typespec;
    } EXPR_OPERAND_NUMBER_FLOAT;
    struct {
      struct __Alc_Ast *array;
      struct __Alc_Ast *index_expression;
    } EXPR_OPERAND_ARRAY_ELEMENT;
    struct {
      struct __Alc_Ast *type;
      struct __Alc_Ast *expression;
    } EXPR_OPERAND_CAST_TO;
    struct {
      char *callee_name;
      struct __Alc_Ast **arguments;
      usize arguments_num;
    } EXPR_OPERAND_CALL;
    struct {
      char *callee_name;
      struct __Alc_Ast *generic_type_list;
      struct __Alc_Ast **arguments;
      usize arguments_num;
    } EXPR_OPERAND_GENERIC_CALL;
    struct {
      char *content;
      char *typespec;
    } EXPR_OPERAND_STRING;
    struct {
      char *content;
      char *typespec;
    } EXPR_OPERAND_SYMBOL;
    struct {
      struct __Alc_Ast *from;
      struct __Alc_Ast *what;
    } EXPR_OPERAND_ACCESS_MEMBER;
    struct {
      struct __Alc_Ast *type;
    } EXPR_OPERAND_SIZE_OF;
    struct {
      struct __Alc_Ast *expression;
    } EXPR_OPERAND_ALIGN_OF;
    struct {
      struct __Alc_Ast **entries;
      usize entries_num;
    } INITLIST;
    struct {
      struct __Alc_Ast *expression;
    } INITLIST_ENTRY;
    struct {
      char *field_name;
      struct __Alc_Ast *expression;
    } INITLIST_ENTRY_EXPLICIT;
    struct {
      struct __Alc_Ast **index_expressions;
      usize index_expressions_num;
      struct __Alc_Ast *expression;
    } INITLIST_ENTRY_EXPLICIT_ARRAY_ELEMENT;
    struct {
      char *name;
      struct __Alc_Ast *generic_placeholder_type_list;
      struct __Alc_Ast **children;
      usize children_num;
      struct __Alc_Ast *attribute_list;
      Alc_Ast_Struct_Kind kind;
    } GENERIC_STRUCT;
    struct {
      char *name;
      Alc_Ast_Function_Kind kind;
      struct __Alc_Ast *generic_placeholder_type_list;
      struct __Alc_Ast *argument_list;
      struct __Alc_Ast *return_type;
      struct __Alc_Ast *body;
      struct __Alc_Ast *attribute_list;
    } GENERIC_FUNC;
    struct {
      struct __Alc_Ast **generic_placeholder_types;
      usize generic_placeholder_types_num;
    } GENERIC_PLACEHOLDER_TYPE_LIST;
    struct {
      char *name;
      struct __Alc_Ast *default_type;
    } GENERIC_PLACEHOLDER_TYPE;
    struct {
      struct __Alc_Ast **generic_types;
      usize generic_types_num;
    } GENERIC_TYPE_LIST;
    struct {
      char *name;
      struct __Alc_Ast *generic_type_list;
    } GENERIC_TYPE;
    struct {
      char *name;
      struct __Alc_Ast *generic_type_list;
      struct __Alc_Ast *subobject;
    } GENERIC_NAMESPACE;
  } data;

  usize pos;
  Alc_Ast_Kind kind;
} Alc_Ast;

ALC_API void alc_ast_print(const Alc_Ast *ast);

#endif // __ALC_AST_H__
