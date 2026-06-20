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
} alc_ast_kind_t;

typedef enum {
  ALC_AST_FUNCTION_KIND_DEFAULT,
  ALC_AST_FUNCTION_KIND_EXPLICIT,
  ALC_AST_FUNCTION_KIND_EXPORTED,
} alc_ast_function_kind_t;

typedef enum {
  ALC_AST_STRUCT_KIND_DEFAULT,
  ALC_AST_STRUCT_KIND_PARTIAL,
} alc_ast_struct_kind_t;

typedef struct __alc_ast_t {
  union {
    struct {
      struct __alc_ast_t **toplevel_statements;
      usize toplevel_statements_num;
    } ROOT;
    struct {
      struct __alc_ast_t *lhs;
      struct __alc_ast_t *rhs;
      struct __alc_ast_t *operator;
    } EXPR;
    struct {
      struct __alc_ast_t *operand;
      struct __alc_ast_t *operator;
    } PREFIX_EXPR;
    struct {
      char *name;
      struct __alc_ast_t *submodule;
    } MODULE;
    struct {
      struct __alc_ast_t *module;
    } IMPORT;
    struct {
      char *name;
      struct __alc_ast_t *generic_placeholder_type_list;
      struct __alc_ast_t *aliased_type;
      struct __alc_ast_t *attribute_list;
    } TYPEDEF;
    struct {
      char *name;
      struct __alc_ast_t *argument_list;
      struct __alc_ast_t *return_type;
    } EXTERN_FUNC;
    struct {
      char *name;
      struct __alc_ast_t *type;
    } EXTERN_VARDECL;
    struct {
      char *name;
      struct __alc_ast_t *qualified;
    } QUALIFIER;
    struct {
      char *name;
      struct __alc_ast_t **children;
      usize children_num;
      struct __alc_ast_t *attribute_list;
      alc_ast_struct_kind_t kind;
    } STRUCT;
    struct {
      char *name;
      struct __alc_ast_t **children;
      usize children_num;
      struct __alc_ast_t *attribute_list;
    } UNION;
    struct {
      char *name;
      struct __alc_ast_t **elements;
      usize elements_num;
      struct __alc_ast_t *attribute_list;
    } ENUM;
    struct {
      char *name;
      struct __alc_ast_t *expression;
    } ENUM_ELEMENT;
    struct {
      char *name;
      struct __alc_ast_t *argument_list;
      struct __alc_ast_t *return_type;
      struct __alc_ast_t *body;
      struct __alc_ast_t *attribute_list;
      alc_ast_function_kind_t kind;
    } FUNC;
    struct {
      struct __alc_ast_t **arguments;
      usize arguments_num;
    } ARGUMENT_LIST;
    struct {
      char *name;
      struct __alc_ast_t *subobject;
    } NAMESPACE;
    struct {
      char *type;
    } SCOPE;
    struct {
      struct __alc_ast_t **cases;
      usize cases_num;
      struct __alc_ast_t *body;
    } CASE_CHAIN;
    struct {
      struct __alc_ast_t *expression;
    } CASE;
    struct {
      char *name;
      struct __alc_ast_t **arguments;
      usize arguments_num;
      // NOTE: This is used to indicate that attribute has parenthesis, in which
      // arguments should be placed (not that it really has arguments).
      b8 has_arguments;
    } ATTRIBUTE;
    struct {
      struct __alc_ast_t **attributes;
      usize attributes_num;
    } ATTRIBUTE_LIST;
    struct {
      char *name;
      struct __alc_ast_t *expression;
    } EXPLICIT_CALL_ARGUMENT;
    struct {
      char *name;
    } LABEL;
    struct {
      struct __alc_ast_t **statements;
      usize statements_num;
    } STMT_BLOCK;
    struct {
      struct __alc_ast_t *expression;
    } STMT_RETURN;
    struct {
      struct __alc_ast_t *label;
    } STMT_GOTO;
    struct {
      struct __alc_ast_t *label;
    } STMT_LABEL;
    struct {
      struct __alc_ast_t *condition;
      struct __alc_ast_t *body;
      struct __alc_ast_t *attribute_list;
    } STMT_WHILE;
    struct {
      struct __alc_ast_t *init_statement;
      struct __alc_ast_t *condition;
      struct __alc_ast_t *expression;
      struct __alc_ast_t *body;
      struct __alc_ast_t *attribute_list;
    } STMT_FOR;
    struct {
      struct __alc_ast_t *condition;
      struct __alc_ast_t *body;
      struct __alc_ast_t *attribute_list;
    } STMT_DO_WHILE;
    struct {
      struct __alc_ast_t *body;
    } STMT_LOOP;
    struct {
      char *item_name;
      struct __alc_ast_t *iterator;
      struct __alc_ast_t *body;
      struct __alc_ast_t *attribute_list;
    } STMT_FOREACH;
    struct {
      struct __alc_ast_t *expression;
    } STMT_EXPR;
    struct {
      struct __alc_ast_t *expression;
      struct __alc_ast_t **case_chains;
      usize case_chains_num;
    } STMT_SWITCH;
    struct {
      struct __alc_ast_t *body;
    } STMT_DEFER;
    struct {
      struct __alc_ast_t *condition;
      struct __alc_ast_t *body;
      struct __alc_ast_t *else_statement;
      struct __alc_ast_t *attribute_list;
    } STMT_IF;
    struct {
      struct __alc_ast_t *body;
    } STMT_ELSE;
    struct {
      char *name;
    } TYPE_PLAIN;
    struct {
      struct __alc_ast_t *type;
    } TYPE_POINTER;
    struct {
      struct __alc_ast_t *type;
      struct __alc_ast_t *size_expression;
    } TYPE_ARRAY;
    struct {
      struct __alc_ast_t *argument_list;
      struct __alc_ast_t *return_type;
    } TYPE_FUNCTION_POINTER;
    struct {
      struct __alc_ast_t *expression;
    } TYPE_TYPE_OF;
    struct {
      char *name;
      struct __alc_ast_t *type;
      struct __alc_ast_t *attribute_list;
    } VAR_DECL;
    struct {
      char *name;
      struct __alc_ast_t *type;
      struct __alc_ast_t *expression;
      struct __alc_ast_t *attribute_list;
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
      struct __alc_ast_t *array;
      struct __alc_ast_t *index_expression;
    } EXPR_OPERAND_ARRAY_ELEMENT;
    struct {
      struct __alc_ast_t *type;
      struct __alc_ast_t *expression;
    } EXPR_OPERAND_CAST_TO;
    struct {
      char *callee_name;
      struct __alc_ast_t **arguments;
      usize arguments_num;
    } EXPR_OPERAND_CALL;
    struct {
      char *callee_name;
      struct __alc_ast_t *generic_type_list;
      struct __alc_ast_t **arguments;
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
      struct __alc_ast_t *from;
      struct __alc_ast_t *what;
    } EXPR_OPERAND_ACCESS_MEMBER;
    struct {
      struct __alc_ast_t *type;
    } EXPR_OPERAND_SIZE_OF;
    struct {
      struct __alc_ast_t *expression;
    } EXPR_OPERAND_ALIGN_OF;
    struct {
      struct __alc_ast_t **entries;
      usize entries_num;
    } INITLIST;
    struct {
      struct __alc_ast_t *expression;
    } INITLIST_ENTRY;
    struct {
      char *field_name;
      struct __alc_ast_t *expression;
    } INITLIST_ENTRY_EXPLICIT;
    struct {
      struct __alc_ast_t **index_expressions;
      usize index_expressions_num;
      struct __alc_ast_t *expression;
    } INITLIST_ENTRY_EXPLICIT_ARRAY_ELEMENT;
    struct {
      char *name;
      struct __alc_ast_t *generic_placeholder_type_list;
      struct __alc_ast_t **children;
      usize children_num;
      struct __alc_ast_t *attribute_list;
      alc_ast_struct_kind_t kind;
    } GENERIC_STRUCT;
    struct {
      char *name;
      alc_ast_function_kind_t kind;
      struct __alc_ast_t *generic_placeholder_type_list;
      struct __alc_ast_t *argument_list;
      struct __alc_ast_t *return_type;
      struct __alc_ast_t *body;
      struct __alc_ast_t *attribute_list;
    } GENERIC_FUNC;
    struct {
      struct __alc_ast_t **generic_placeholder_types;
      usize generic_placeholder_types_num;
    } GENERIC_PLACEHOLDER_TYPE_LIST;
    struct {
      char *name;
      struct __alc_ast_t *default_type;
    } GENERIC_PLACEHOLDER_TYPE;
    struct {
      struct __alc_ast_t **generic_types;
      usize generic_types_num;
    } GENERIC_TYPE_LIST;
    struct {
      char *name;
      struct __alc_ast_t *generic_type_list;
    } GENERIC_TYPE;
    struct {
      char *name;
      struct __alc_ast_t *generic_type_list;
      struct __alc_ast_t *subobject;
    } GENERIC_NAMESPACE;
  } data;

  usize pos;
  alc_ast_kind_t kind;
} alc_ast_t;

ALC_API void alc_ast_print(const alc_ast_t *ast);

#endif // __ALC_AST_H__
