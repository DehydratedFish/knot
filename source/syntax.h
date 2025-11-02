#pragma once

#include "list.h"



struct SyntaxElement;


struct SourceLocation {
    s64 pos;
    s32 line;
    s32 column;
};


// NOTE: In order of precedence.
enum SyntaxOperator {
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

    SYNTAX_REFERENCE,

    SYNTAX_BINARY_OPERATOR,

    SYNTAX_EXPRESSION_LIST,

    SYNTAX_SYMBOL_DECL,

    SYNTAX_KIND_COUNT,
};


struct SyntaxNode {
    List<SyntaxElement*> elements;
    SyntaxElement *tree;
};


struct SyntaxElement {
    SyntaxKind kind;
    SourceLocation loc;
};

struct SyntaxScope : SyntaxElement {
    List<SyntaxNode> nodes;
};

struct SyntaxIdentifier : SyntaxElement {
    String name;
};

struct SyntaxIntegerLiteral : SyntaxElement {
    String value;
};

struct SyntaxStruct : SyntaxElement {
    // TODO: Put outside of the namespace?
    struct Member {
        SyntaxIdentifier name;
        //Type type;
        s32 offset;
        s32 padding;
    };

    SyntaxIdentifier name;
    Array<Member>    members;
};

struct SyntaxReference : SyntaxElement {
    SyntaxElement *thing;
};

struct SyntaxBinaryOperator : SyntaxElement {
    SyntaxOperator operator_kind;

    SyntaxElement *lhs;
    SyntaxElement *rhs;
};

struct SyntaxExpressionList : SyntaxElement {
    Array<SyntaxElement*> elements;
};

struct SyntaxSymbolDeclaration : SyntaxElement {
    Array<SyntaxIdentifier*> symbols;
    Array<SyntaxElement*>    elements;
};

