#ifndef __ALC_AST_H__
#define __ALC_AST_H__

#include <alc/defs.h>

// #define ALC_AST_KIND_X(_name, _str_name)
#define ALC_AST_KINDS                                                                  \
  ALC_AST_KIND_X(ROOT, "root")                                                         \
  ALC_AST_KIND_X(EXPR, "expression")                                                   \
  ALC_AST_KIND_X(PREFIX_EXPR, "prefix expression")                                     \
  ALC_AST_KIND_X(MODULE, "module")                                                     \
  ALC_AST_KIND_X(IMPORT, "import")                                                     \
  ALC_AST_KIND_X(TYPEDEF, "type definition")                                           \
  ALC_AST_KIND_X(EXTERN_FUNC, "external function declaration")                         \
  ALC_AST_KIND_X(EXTERN_VARDECL, "external variable declaration")                      \
  ALC_AST_KIND_X(QUALIFIER, "qualifier")                                               \
  ALC_AST_KIND_X(NONE, "none")                                                         \
  ALC_AST_KIND_X(VARIADIC, "variadic argument")                                        \
  ALC_AST_KIND_X(STRUCT, "structure definition")                                       \
  ALC_AST_KIND_X(UNION, "union definition")                                            \
  ALC_AST_KIND_X(ENUM, "enum definition")                                              \
  ALC_AST_KIND_X(ENUM_ELEMENT, "enum element")                                         \
  ALC_AST_KIND_X(FUNC, "function definition")                                          \
  ALC_AST_KIND_X(ARGUMENT_LIST, "argument list")                                       \
  ALC_AST_KIND_X(NAMESPACE, "namespace")                                               \
  ALC_AST_KIND_X(SCOPE, "scope")                                                       \
  ALC_AST_KIND_X(CASE_CHAIN, "case chain")                                             \
  ALC_AST_KIND_X(CASE, "case")                                                         \
  ALC_AST_KIND_X(DEFAULT, "default")                                                   \
  ALC_AST_KIND_X(ATTRIBUTE, "attribute")                                               \
  ALC_AST_KIND_X(ATTRIBUTE_LIST, "attribute list")                                     \
  ALC_AST_KIND_X(EXPLICIT_CALL_ARGUMENT, "explicit call argument")                     \
  ALC_AST_KIND_X(LABEL, "label")                                                       \
  ALC_AST_KIND_X(STMT_BLOCK, "statement block")                                        \
  ALC_AST_KIND_X(STMT_RETURN, "return statement")                                      \
  ALC_AST_KIND_X(STMT_GOTO, "GOTO statement")                                          \
  ALC_AST_KIND_X(STMT_LABEL, "label")                                                  \
  ALC_AST_KIND_X(STMT_BREAK, "break statement")                                        \
  ALC_AST_KIND_X(STMT_CONTINUE, "continue statement")                                  \
  ALC_AST_KIND_X(STMT_FALLTHROUGH, "fallthrough statement")                            \
  ALC_AST_KIND_X(STMT_WHILE, "while loop")                                             \
  ALC_AST_KIND_X(STMT_FOR, "for loop")                                                 \
  ALC_AST_KIND_X(STMT_DO_WHILE, "do-while loop")                                       \
  ALC_AST_KIND_X(STMT_LOOP, "loop")                                                    \
  ALC_AST_KIND_X(STMT_FOREACH, "for-each loop")                                        \
  ALC_AST_KIND_X(STMT_EXPR, "expression statement")                                    \
  ALC_AST_KIND_X(STMT_SWITCH, "switch statement")                                      \
  ALC_AST_KIND_X(STMT_DEFER, "defer statement")                                        \
  ALC_AST_KIND_X(STMT_IF, "if statement")                                              \
  ALC_AST_KIND_X(STMT_ELSE, "else statement")                                          \
  ALC_AST_KIND_X(TYPE_PLAIN, "plain type")                                             \
  ALC_AST_KIND_X(TYPE_POINTER, "pointer type")                                         \
  ALC_AST_KIND_X(TYPE_ARRAY, "array type")                                             \
  ALC_AST_KIND_X(TYPE_FUNCTION_POINTER, "function pointer type")                       \
  ALC_AST_KIND_X(TYPE_TUPLE, "tuple")                                                  \
  ALC_AST_KIND_X(TYPE_TYPE_OF, "type of")                                              \
  ALC_AST_KIND_X(VAR_DECL, "variable declaration")                                     \
  ALC_AST_KIND_X(VAR_DEF, "variable definition")                                       \
  ALC_AST_KIND_X(EXPR_OPERAND_IDENTIFIER, "identifier operand")                        \
  ALC_AST_KIND_X(EXPR_OPERAND_NUMBER, "number operand")                                \
  ALC_AST_KIND_X(EXPR_OPERAND_NUMBER_FLOAT, "floating-point number operand")           \
  ALC_AST_KIND_X(EXPR_OPERAND_ARRAY_ELEMENT, "array element operand")                  \
  ALC_AST_KIND_X(EXPR_OPERAND_CAST_TO, "cast-to operand")                              \
  ALC_AST_KIND_X(EXPR_OPERAND_CALL, "call operand")                                    \
  ALC_AST_KIND_X(EXPR_OPERAND_GENERIC_CALL, "generic call operand")                    \
  ALC_AST_KIND_X(EXPR_OPERAND_STRING, "string operand")                                \
  ALC_AST_KIND_X(EXPR_OPERAND_SYMBOL, "symbol operand")                                \
  ALC_AST_KIND_X(EXPR_OPERAND_ACCESS_MEMBER, "access member operand")                  \
  ALC_AST_KIND_X(EXPR_OPERAND_ACCESS_MEMBER_TUPLE, "access member in tuple operand")   \
  ALC_AST_KIND_X(EXPR_OPERAND_SIZE_OF, "size of operand")                              \
  ALC_AST_KIND_X(EXPR_OPERAND_ALIGN_OF, "align of operand")                            \
  ALC_AST_KIND_X(EXPR_OPERAND_OFFSET_OF, "offset of field in structure")               \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_ADD, "binary addition operator")                 \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_SUB, "binary subtraction operator")              \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_MUL, "binary multiplication operator")           \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_DIV, "binary division operator")                 \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_MOD, "binary module operator")                   \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_SHL, "binary shift-left operator")               \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_SHR, "binary shift-right operator")              \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_AND, "binary AND operator")                      \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_OR, "binary OR operator")                        \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_XOR, "binary XOR operator")                      \
  ALC_AST_KIND_X(EXPR_OPERATOR_COMPARE_EQ, "compare equals operator")                  \
  ALC_AST_KIND_X(EXPR_OPERATOR_COMPARE_NOTEQ, "compare not-equals operator")           \
  ALC_AST_KIND_X(EXPR_OPERATOR_COMPARE_LTHAN, "compare less-than operator")            \
  ALC_AST_KIND_X(EXPR_OPERATOR_COMPARE_GTHAN, "compare greater-than operator")         \
  ALC_AST_KIND_X(EXPR_OPERATOR_COMPARE_LTHANEQ, "compare less-than-equal operator")    \
  ALC_AST_KIND_X(EXPR_OPERATOR_COMPARE_GTHANEQ, "compare greater-than-equal operator") \
  ALC_AST_KIND_X(EXPR_OPERATOR_BOOLEAN_AND, "boolean AND operator")                    \
  ALC_AST_KIND_X(EXPR_OPERATOR_BOOLEAN_OR, "boolean OR operator")                      \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_EQ, "assign equals operator")                    \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_ADDEQ, "assign add-equals operator")             \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_SUBEQ, "assign sub-equals operator")             \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_MULEQ, "assign mul-equals operator")             \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_DIVEQ, "assign div-equals operator")             \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_MODEQ, "assign mod-equals operator")             \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_SHLEQ, "assign shift-left-equals operator")      \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_SHREQ, "assign shift-right-equals operator")     \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_ANDEQ, "assign AND-equals operator")             \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_OREQ, "assign OR-equals operator")               \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_XOREQ, "assign XOR-equals operator")             \
  ALC_AST_KIND_X(EXPR_OPERATOR_PREFIX_NOT, "prefix NOT operator")                      \
  ALC_AST_KIND_X(EXPR_OPERATOR_PREFIX_BOOLEAN_NOT, "prefix boolean NOT operator")      \
  ALC_AST_KIND_X(EXPR_OPERATOR_PREFIX_NEGATIVE, "prefix negative operator")            \
  ALC_AST_KIND_X(EXPR_OPERATOR_PREFIX_DEREFERENCE, "prefix dereference operator")      \
  ALC_AST_KIND_X(EXPR_OPERATOR_PREFIX_ADDRESS, "prefix take-address operator")         \
  ALC_AST_KIND_X(INITLIST, "initialization list")                                      \
  ALC_AST_KIND_X(INITLIST_ENTRY, "initialization list entry")                          \
  ALC_AST_KIND_X(INITLIST_ENTRY_EXPLICIT, "initialization list explicit entry")        \
  ALC_AST_KIND_X(INITLIST_ENTRY_EXPLICIT_ARRAY_ELEMENT,                                \
                 "initialization list explicit array element entry")                   \
  ALC_AST_KIND_X(GENERIC_STRUCT, "generic structure definition")                       \
  ALC_AST_KIND_X(GENERIC_FUNC, "generic function definition")                          \
  ALC_AST_KIND_X(GENERIC_PLACEHOLDER_TYPE_LIST, "generic placeholder type list")       \
  ALC_AST_KIND_X(GENERIC_PLACEHOLDER_TYPE, "generic placeholder type")                 \
  ALC_AST_KIND_X(GENERIC_TYPE_LIST, "generic type list")                               \
  ALC_AST_KIND_X(GENERIC_TYPE, "generic type")                                         \
  ALC_AST_KIND_X(GENERIC_NAMESPACE, "generic namespace")

#define ALC_AST_KIND_FULL_NAME(_name) ALC_AST_KIND_##_name

typedef enum {
#define ALC_AST_KIND_X(_name, _str_name) ALC_AST_KIND_FULL_NAME(_name),
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
      b8 is_enum_flags;
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
      struct __Alc_Ast **types;
      usize types_num;
    } TYPE_TUPLE;
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
      u64 index;
      struct __Alc_Ast *tuple;
    } EXPR_OPERAND_ACCESS_MEMBER_TUPLE;
    struct {
      struct __Alc_Ast *type;
    } EXPR_OPERAND_SIZE_OF;
    struct {
      struct __Alc_Ast *expression;
    } EXPR_OPERAND_ALIGN_OF;
    struct {
      struct __Alc_Ast *base_structure;
      struct __Alc_Ast *field_expression;
    } EXPR_OPERAND_OFFSET_OF;
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
      struct __Alc_Ast *generic_placeholder_type_list;
      struct __Alc_Ast *argument_list;
      struct __Alc_Ast *return_type;
      struct __Alc_Ast *body;
      struct __Alc_Ast *attribute_list;
      Alc_Ast_Function_Kind kind;
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
  };

  usize pos;
  Alc_Ast_Kind kind;
} Alc_Ast;

ALC_API void alc_ast_print(const Alc_Ast *ast);

#endif // __ALC_AST_H__
