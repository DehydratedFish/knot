#pragma once

#include "list.h"
#include "hash_table.h"


struct SyntaxElement;
struct SyntaxStruct;
struct SyntaxLambda;
struct Type;


struct IntegerType {
    b32 is_signed;
};

struct StructType {
    SyntaxStruct *decl;
};


struct BuiltinLambdaInfo {
    Array<Type> params;
    Array<Type> returns;
};

struct LambdaType {
    // TODO: Unionize.
    SyntaxLambda *decl;
    BuiltinLambdaInfo *builtin_info;
};

enum TypeKind {
    TYPE_UNDEFINED,
    TYPE_SPECIFIER,

    TYPE_INTEGER,
    TYPE_BOOL,
    TYPE_STRING,
    TYPE_STRUCT,
    TYPE_LAMBDA,
    TYPE_UNRESOLVED_OVERLOAD_SET,
};

enum TypeFlags {
    TYPE_FLAG_BUILTIN  = 1 << 0,
    TYPE_FLAG_CONSTANT = 1 << 1,
};

struct Type {
    TypeKind kind;
    u32 flags;

    String name;

    s32 pointer_depth;

    union {
        IntegerType integer;
        StructType  structure;
        LambdaType  lambda;
        Array<Type*> overloads;
    } as;
};


enum IdentifierKind {
    IDENTIFIER_UNDEFINED,

    IDENTIFIER_COMPILE_TIME_VALUE,
    IDENTIFIER_VARIABLE,
    IDENTIFIER_TYPE,
    IDENTIFIER_LAMBDA,
};

struct Identifier {
    IdentifierKind kind;
    String name;

    // TODO: Make a union?
    Type *type;
    SyntaxElement *element;
    List<Type*> lambda_set;
};


struct SourceLocation {
    s64 pos;
    s32 line;
    s32 column;
};


// NOTE: In order of precedence.
enum SyntaxOperator {
    OP_CALL,

    OP_EQUAL,

    OP_DOT,

    OP_REFERENCE,

    OP_MUL,
    OP_DIV,

    OP_ADD,
    OP_SUB,

    OP_COUNT,
};

inline String enum_string(SyntaxOperator op) {
    assert(op < OP_COUNT);

    String lookup[OP_COUNT] = {
        "==",
        ".",
        "&",
        "*",
        "/",
        "+",
        "-",
    };

    return lookup[op];
};

enum SyntaxKind {
    SYNTAX_KIND_NONE,
    
    SYNTAX_SCOPE,

    SYNTAX_IDENTIFIER,
    SYNTAX_INTEGER_LITERAL,

    SYNTAX_CALL,

    SYNTAX_REFERENCE,

    SYNTAX_BINARY_OPERATOR,
    SYNTAX_DOT,

    SYNTAX_SYMBOL_DECL,
    SYNTAX_VARIABLE_DECL,

    SYNTAX_STRUCT_DECL,
    SYNTAX_STRUCT_FIELD,

    SYNTAX_LAMBDA_DECL,
    SYNTAX_LAMBDA_PARAMETER,
    SYNTAX_RETURN,

    SYNTAX_KIND_COUNT,
};

inline String enum_string(SyntaxKind kind) {
    String lookup[] = {
        "SYNTAX_KIND_NONE",

        "SYNTAX_SCOPE",

        "SYNTAX_IDENTIFIER",
        "SYNTAX_INTEGER_LITERAL",

        "SYNTAX_CALL",

        "SYNTAX_REFERENCE",

        "SYNTAX_BINARY_OPERATOR",
        "SYNTAX_DOT",

        "SYNTAX_SYMBOL_DECL",
        "SYNTAX_VARIABLE_DECL",

        "SYNTAX_STRUCT_DECL",
        "SYNTAX_STRUCT_FIELD",

        "SYNTAX_LAMBDA_DECL",
        "SYNTAX_LAMBDA_PARAMETER",
        "SYNTAX_RETURN",

        "SYNTAX_KIND_COUNT",
    };

    return lookup[kind];
}


struct SyntaxNode {
    List<SyntaxElement*> elements;
    SyntaxElement *tree;
};


struct SyntaxElement {
    SyntaxKind kind;
    SourceLocation loc;
    Type type;
};

struct SyntaxScope : SyntaxElement {
    SyntaxScope *parent;
    List<SyntaxElement*> elements;

    HashTable<String, Identifier> identifier_table;
};

Identifier *resolve_identifier(SyntaxScope *scope, String name);

struct SyntaxIdentifier : SyntaxElement {
    String name;

    // TODO: Maybe store a pointer to the Identifier struct after type checking?
    //       That would mean no second lookup in codegen but also that they need
    //       to be stored as pointers in the scope HashTable as well. Which
    //       is kinda awkward to use.
};

struct SyntaxIntegerLiteral : SyntaxElement {
    String value;
};


struct SyntaxStructMember : SyntaxElement {
    SyntaxIdentifier ident;
    s32 offset;
    s32 padding;
};

struct SyntaxStruct : SyntaxElement {
    SyntaxIdentifier name;
    Array<SyntaxStructMember> members;
};

struct SyntaxReference : SyntaxElement {
    SyntaxElement *thing;
};

struct SyntaxBinaryOperator : SyntaxElement {
    SyntaxOperator operator_kind;
    String text;

    SyntaxElement *lhs;
    SyntaxElement *rhs;
};

struct SyntaxDotOperator : SyntaxElement {
    SyntaxIdentifier *lhs;
    SyntaxIdentifier *rhs;
};

struct SyntaxSymbolDeclaration : SyntaxElement {
    Array<SyntaxIdentifier*> symbols;
    Array<SyntaxElement*>    elements;
};

struct SyntaxVariableDeclaration : SyntaxElement {
    Array<SyntaxIdentifier*> variables;
    Array<SyntaxElement*> expressions;
};

struct SyntaxLambdaParameter : SyntaxElement {
    String name;
    SourceLocation name_loc; // NOTE: Location of the name for better type check errors.
};
struct SyntaxLambda : SyntaxElement {
    Array<SyntaxLambdaParameter> params;
    Array<SyntaxElement> returns;

    SyntaxScope scope;
};

struct SyntaxCall : SyntaxElement {
    SyntaxElement *caller;
    Array<SyntaxElement*> args;
};

struct SyntaxReturn : SyntaxElement {
    Array<SyntaxElement*> returns;
};

