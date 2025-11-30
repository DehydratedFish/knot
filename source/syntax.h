#pragma once

#include "list.h"
#include "hash_table.h"



struct SyntaxElement;
struct SyntaxStruct;


struct IntegerType {
    b32 is_signed;
};

struct StructType {
    SyntaxStruct *decl;
};

enum TypeKind {
    TYPE_UNDEFINED,
    TYPE_SPECIFIER,

    TYPE_INTEGER,
    TYPE_BOOL,
    TYPE_STRING,
    TYPE_STRUCT,
};

struct Type {
    TypeKind kind;
    u32 flags;

    String name;

    s32 pointer_depth;

    union {
        IntegerType integer;
        StructType  structure;
    } as;
};


enum IdentifierKind {
    IDENTIFIER_UNDEFINED,

    IDENTIFIER_SYMBOL,
    IDENTIFIER_TYPE,
};

struct Identifier {
    IdentifierKind kind;
    String name;

    // TODO: Make a union?
    Type *type;
    SyntaxElement *element;
};


struct SourceLocation {
    s64 pos;
    s32 line;
    s32 column;
};


// NOTE: In order of precedence.
enum SyntaxOperator {
    OP_EQUAL,

    OP_DOT,

    OP_REFERENCE,

    OP_MULTIPLY,
    OP_DIVIDE,

    OP_PLUS,
    OP_MINUS,

    OP_COUNT,
};

inline String enum_string(SyntaxOperator op) {
    assert(op < OP_COUNT);

    String lookup[OP_COUNT] = {
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
    SYNTAX_LAMBDA_ARGUMENT,
    SYNTAX_RETURN,

    SYNTAX_KIND_COUNT,
};

inline String enum_string(SyntaxKind kind) {
    String lookup[] = {
        "SYNTAX_KIND_NONE",

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
        "SYNTAX_LAMBDA_ARGUMENT",
        "SYNTAX_RETURN",

        "SYNTAX_KIND_COUNT",
    };

    return lookup[kind];
}


struct SyntaxNode {
    List<SyntaxElement*> elements;
    SyntaxElement *tree;
};


enum SyntaxFlags {
    SYNTAX_FLAG_CONSTANT = 1 << 0,
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

struct SyntaxIdentifier : SyntaxElement {
    String name;
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

struct SyntaxReturn : SyntaxElement {
    Array<SyntaxElement*> returns;
};

