#ifndef __ALC_AST_H__
#define __ALC_AST_H__

#include <alc/defs.h>

// #define ALC_AST_KIND_X(_name, _str_name)
#define ALC_AST_KINDS                                                                  \
  ALC_AST_KIND_X(ROOT, "Root")                                                         \
  ALC_AST_KIND_X(EXPR, "Expression")                                                   \
  ALC_AST_KIND_X(PREFIX_EXPR, "Prefix expression")                                     \
  ALC_AST_KIND_X(MODULE, "Module")                                                     \
  ALC_AST_KIND_X(IMPORT, "Import")                                                     \
  ALC_AST_KIND_X(TYPEDEF, "Type definition")                                           \
  ALC_AST_KIND_X(EXTERN_FUNC, "External function declaration")                         \
  ALC_AST_KIND_X(EXTERN_VARDECL, "Extern variable declaration")                        \
  ALC_AST_KIND_X(QUALIFIER, "Qualifier")                                               \
  ALC_AST_KIND_X(NONE, "None")                                                         \
  ALC_AST_KIND_X(VARIADIC, "Variadic argument")                                        \
  ALC_AST_KIND_X(STRUCT, "Structure definition")                                       \
  ALC_AST_KIND_X(UNION, "Union definition")                                            \
  ALC_AST_KIND_X(ENUM, "Enum definition")                                              \
  ALC_AST_KIND_X(ENUM_ELEMENT, "Enum element")                                         \
  ALC_AST_KIND_X(FUNC, "Function definition")                                          \
  ALC_AST_KIND_X(ARGUMENT_LIST, "Argument list")                                       \
  ALC_AST_KIND_X(NAMESPACE, "Namespace")                                               \
  ALC_AST_KIND_X(SCOPE, "Scope")                                                       \
  ALC_AST_KIND_X(CASE_CHAIN, "Case chain")                                             \
  ALC_AST_KIND_X(CASE, "Case")                                                         \
  ALC_AST_KIND_X(DEFAULT, "Default")                                                   \
  ALC_AST_KIND_X(ATTRIBUTE, "Attribute")                                               \
  ALC_AST_KIND_X(ATTRIBUTE_LIST, "Attribute list")                                     \
  ALC_AST_KIND_X(EXPLICIT_CALL_ARGUMENT, "Explicit call argument")                     \
  ALC_AST_KIND_X(LABEL, "Label")                                                       \
  ALC_AST_KIND_X(STMT_BLOCK, "Statement block")                                        \
  ALC_AST_KIND_X(STMT_RETURN, "Return statement")                                      \
  ALC_AST_KIND_X(STMT_GOTO, "GOTO statement")                                          \
  ALC_AST_KIND_X(STMT_LABEL, "Label")                                                  \
  ALC_AST_KIND_X(STMT_BREAK, "Break statement")                                        \
  ALC_AST_KIND_X(STMT_CONTINUE, "Continue statement")                                  \
  ALC_AST_KIND_X(STMT_FALLTHROUGH, "Fallthrough statement")                            \
  ALC_AST_KIND_X(STMT_WHILE, "While loop")                                             \
  ALC_AST_KIND_X(STMT_FOR, "For loop")                                                 \
  ALC_AST_KIND_X(STMT_DO_WHILE, "Do-while loop")                                       \
  ALC_AST_KIND_X(STMT_LOOP, "Loop")                                                    \
  ALC_AST_KIND_X(STMT_FOREACH, "For-each loop")                                        \
  ALC_AST_KIND_X(STMT_EXPR, "Expression statement")                                    \
  ALC_AST_KIND_X(STMT_SWITCH, "Switch statement")                                      \
  ALC_AST_KIND_X(STMT_DEFER, "Defer statement")                                        \
  ALC_AST_KIND_X(STMT_IF, "If statement")                                              \
  ALC_AST_KIND_X(STMT_ELSE, "Else statement")                                          \
  ALC_AST_KIND_X(TYPE_PLAIN, "Plain type")                                             \
  ALC_AST_KIND_X(TYPE_POINTER, "Pointer type")                                         \
  ALC_AST_KIND_X(TYPE_ARRAY, "Array type")                                             \
  ALC_AST_KIND_X(TYPE_FUNCTION_POINTER, "Function pointer type")                       \
  ALC_AST_KIND_X(TYPE_TYPE_OF, "Type of")                                              \
  ALC_AST_KIND_X(VAR_DECL, "Variable declaration")                                     \
  ALC_AST_KIND_X(VAR_DEF, "Variable definition")                                       \
  ALC_AST_KIND_X(EXPR_OPERAND_IDENTIFIER, "Identifier operand")                        \
  ALC_AST_KIND_X(EXPR_OPERAND_NUMBER, "Number operand")                                \
  ALC_AST_KIND_X(EXPR_OPERAND_NUMBER_FLOAT, "Floating-point number operand")           \
  ALC_AST_KIND_X(EXPR_OPERAND_ARRAY_ELEMENT, "Array element operand")                  \
  ALC_AST_KIND_X(EXPR_OPERAND_CAST_TO, "Cast-to operand")                              \
  ALC_AST_KIND_X(EXPR_OPERAND_CALL, "Call operand")                                    \
  ALC_AST_KIND_X(EXPR_OPERAND_GENERIC_CALL, "Generic call operand")                    \
  ALC_AST_KIND_X(EXPR_OPERAND_STRING, "String operand")                                \
  ALC_AST_KIND_X(EXPR_OPERAND_SYMBOL, "Symbol operand")                                \
  ALC_AST_KIND_X(EXPR_OPERAND_ACCESS_MEMBER, "Access member operand")                  \
  ALC_AST_KIND_X(EXPR_OPERAND_SIZE_OF, "Size of operand")                              \
  ALC_AST_KIND_X(EXPR_OPERAND_ALIGN_OF, "Align of operand")                            \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_ADD, "Binary addition operator")                 \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_SUB, "Binary subtraction operator")              \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_MUL, "Binary multiplication operator")           \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_DIV, "Binary division operator")                 \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_MOD, "Binary module operator")                   \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_SHL, "Binary shift-left operator")               \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_SHR, "Binary shift-right operator")              \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_AND, "Binary AND operator")                      \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_OR, "Binary OR operator")                        \
  ALC_AST_KIND_X(EXPR_OPERATOR_BINARY_XOR, "Binary XOR operator")                      \
  ALC_AST_KIND_X(EXPR_OPERATOR_COMPARE_EQ, "Compare equals operator")                  \
  ALC_AST_KIND_X(EXPR_OPERATOR_COMPARE_NOTEQ, "Compare not-equals operator")           \
  ALC_AST_KIND_X(EXPR_OPERATOR_COMPARE_LTHAN, "Compare less-than operator")            \
  ALC_AST_KIND_X(EXPR_OPERATOR_COMPARE_GTHAN, "Compare greater-than operator")         \
  ALC_AST_KIND_X(EXPR_OPERATOR_COMPARE_LTHANEQ, "Compare less-than-equal operator")    \
  ALC_AST_KIND_X(EXPR_OPERATOR_COMPARE_GTHANEQ, "Compare greater-than-equal operator") \
  ALC_AST_KIND_X(EXPR_OPERATOR_BOOLEAN_AND, "Boolean AND operator")                    \
  ALC_AST_KIND_X(EXPR_OPERATOR_BOOLEAN_OR, "Boolean OR operator")                      \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_EQ, "Assign equals operator")                    \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_ADDEQ, "Assign add-equals operator")             \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_SUBEQ, "Assign sub-equals operator")             \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_MULEQ, "Assign mul-equals operator")             \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_DIVEQ, "Assign div-equals operator")             \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_MODEQ, "Assign mod-equals operator")             \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_SHLEQ, "Assign shift-left-equals operator")      \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_SHREQ, "Assign shift-right-equals operator")     \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_ANDEQ, "Assign AND-equals operator")             \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_OREQ, "Assign OR-equals operator")               \
  ALC_AST_KIND_X(EXPR_OPERATOR_ASSIGN_XOREQ, "Assign XOR-equals operator")             \
  ALC_AST_KIND_X(EXPR_OPERATOR_PREFIX_NOT, "Prefix NOT operator")                      \
  ALC_AST_KIND_X(EXPR_OPERATOR_PREFIX_BOOLEAN_NOT, "Prefix boolean NOT operator")      \
  ALC_AST_KIND_X(EXPR_OPERATOR_PREFIX_NEGATIVE, "Prefix negative operator")            \
  ALC_AST_KIND_X(EXPR_OPERATOR_PREFIX_DEREFERENCE, "Prefix dereference operator")      \
  ALC_AST_KIND_X(EXPR_OPERATOR_PREFIX_ADDRESS, "Prefix take-address operator")         \
  ALC_AST_KIND_X(INITLIST, "Initialization list")                                      \
  ALC_AST_KIND_X(INITLIST_ENTRY, "Initialization list entry")                          \
  ALC_AST_KIND_X(INITLIST_ENTRY_EXPLICIT, "Initialization list explicit entry")        \
  ALC_AST_KIND_X(INITLIST_ENTRY_EXPLICIT_ARRAY_ELEMENT,                                \
                 "Initialization list explicit array element entry")                   \
  ALC_AST_KIND_X(GENERIC_STRUCT, "Generic structure definition")                       \
  ALC_AST_KIND_X(GENERIC_FUNC, "Generic function definition")                          \
  ALC_AST_KIND_X(GENERIC_PLACEHOLDER_TYPE_LIST, "Generic placeholder type list")       \
  ALC_AST_KIND_X(GENERIC_PLACEHOLDER_TYPE, "Generic placeholder type")                 \
  ALC_AST_KIND_X(GENERIC_TYPE_LIST, "Generic type list")                               \
  ALC_AST_KIND_X(GENERIC_TYPE, "Generic type")                                         \
  ALC_AST_KIND_X(GENERIC_NAMESPACE, "Generic namespace")

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
