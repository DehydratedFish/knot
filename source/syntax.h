#pragma once

#include "knot.h"
#include "list.h"



struct SyntaxElement;


// NOTE: In order of precedence.
enum SyntaxOperator {
    OP_REFERENCE,

    OP_MULTIPLY,
    OP_DIVIDE,

    OP_PLUS,
    OP_MINUS,

    OP_COUNT,
};

enum SyntaxKind {
    SYNTAX_KIND_NONE,

    SYNTAX_IDENTIFIER,
    SYNTAX_INTEGER_LITERAL,

    SYNTAX_REFERENCE,

    SYNTAX_BINARY_OPERATOR,

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

struct SyntaxScope {
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

struct Environment {
    String filename;

    List<DiagnosticMessage> diagnostic;

    SyntaxScope root;
};

