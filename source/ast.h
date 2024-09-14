#pragma once

#include "definitions.h"
#include "string2.h"
#include "io.h"

#include "knot.h"


struct AstNode;
struct Field;
struct Type;


enum TypeKind {
    TYPE_NONE,
    TYPE_UNDEFINED,
    TYPE_INTEGER,
    TYPE_STRING,
    TYPE_LAMBDA,
    TYPE_STRUCT,
    TYPE_MULTI,
};

struct IntegerType {
    b32 is_signed;
    u64 upper_bound;
    s64 lower_bound;
};

struct ArrayType {
    Type *type;
    s64   size;
};

struct StringType {
};

struct StructType {
    Array<Field> fields;
};

struct LambdaType {
    Array<Field> params;
    Array<Type>  return_types;
};

struct Type {
    AstNode *decl; // NOTE: If this is null the Type is pre defined.
    String name;

    s32 pointer_depth;
    s64 array_size;
    s32 size_of;

    u32 flags;

    TypeKind kind;
    union {
        IntegerType integer;
        StringType  string;
        LambdaType  lambda;
        StructType  struct_; // NOTE: Ugly...
        Array<Type> types;
    } as;
};


enum IdentifierKind {
    IDENTIFIER_UNDEFINED,
    IDENTIFIER_TYPE,
    IDENTIFIER_VARIABLE,
    IDENTIFIER_CONSTANT,
    IDENTIFIER_BINDING,
};
struct Identifier {
    String name;

    IdentifierKind kind;
    union {
        Type *type;
        AstNode *thing;
    };
};

// CREDIT: https://nullprogram.com/blog/2022/08/08/
struct SymbolTable {
    Allocator allocator;

    u32 used;
    u32 exponent;
    u32 max_load;
    Identifier *entries;
};

void init(SymbolTable *table, u32 exponent, Allocator alloc = default_allocator());
Identifier *find(SymbolTable *table, String name, Allocator alloc = default_allocator());

struct Scope {
    Scope *parent;
    struct AstLambda *lambda;

    DArray<AstNode*> nodes;
    SymbolTable symbols;
};

#define AST_KIND_LABLES \
X(AST_ERROR) \
\
X(AST_REFERENCE) \
X(AST_DEREFERENCE) \
X(AST_NEGATE) \
\
X(AST_EXPRESSION_TUPLE) \
X(AST_IDENTIFIER) \
X(AST_STRING) \
X(AST_INTEGER_LITERAL) \
X(AST_FLOAT_LITERAL) \
X(AST_CALL) \
\
X(AST_BINARY_OPERATOR) \
X(AST_RANGE) \
\
X(AST_ASSIGNMENT) \
X(AST_SYMBOL_DECLARATION) \
X(AST_MULTI_SYMBOL_DECLARATION) \
X(AST_LAMBDA) \
X(AST_STRUCT) \
X(AST_VARIABLE_DECLARATION) \
X(AST_ARRAY_DECLARATION) \
\
X(AST_RETURN) \
\
X(AST_TYPE_COUNT)

#define X(label) label,
enum AstNodeKind {
    AST_KIND_LABLES
};
#undef X

#define X(label) #label,
INTERNAL String AST_KindNames[] = {
    AST_KIND_LABLES
};
#undef X

enum {
    AST_STAGE_TYPE_CHECK,
    AST_STAGE_BYTECODE,
};
struct AstNode {
    AstNodeKind kind;
    u32 stage;

    SourceLocation location;
    Type type;
};

struct AstExpression : AstNode {
};

struct AstTypeSpecifier : AstNode {
};

struct AstIdentifier : AstExpression {
    String name;
};

struct Field {
    AstIdentifier ident;
    AstTypeSpecifier type_spec;
    s32 offset;
};

struct AstParameter : AstNode {
    AstIdentifier ident;
    AstTypeSpecifier type_spec;
    u32 position;
};

enum {
    BINARY_OP_ADD,
    BINARY_OP_SUB,
    BINARY_OP_MUL,
    BINARY_OP_DIV,
};
struct AstBinaryOperator : AstExpression {
    u32 op;
    AstExpression *lhs;
    AstExpression *rhs;
};

struct AstAssignment : AstExpression {
    AstExpression *thing;
    AstExpression *value;
};

struct AstRange : AstExpression {
    AstExpression *start;
    AstExpression *end;
};

struct AstReference : AstExpression {
    AstExpression *expr;
};

struct AstDereference : AstExpression {
    AstExpression *expr;
};

struct AstNegate : AstExpression {
    AstExpression *expr;
};

struct AstIntegerLiteral : AstExpression {
    u64 value;
};

struct AstFloatLiteral : AstExpression {
    r64 value;
};

struct AstString : AstExpression {
    String content;
};

struct AstArrayDeclaration : AstExpression {
    Array<AstExpression*> elements;
};

struct AstCall : AstExpression {
    AstExpression *functor;
    Array<AstExpression*> arguments;
};

struct AstVariableDeclaration : AstExpression {
    Array<AstExpression> names;
};

struct AstReturn : AstNode {
    Array<AstExpression*> values;
};

struct AstStruct : AstNode {
    Array<Field> fields;
};

struct AstSymbolDeclaration : AstNode {
    b32 variable;
    Array<AstIdentifier*> symbols;
    Array<AstTypeSpecifier> types;
    Array<AstNode*> things;
};

struct AstLambda : AstExpression {
    Array<AstParameter> params;
    Array<AstTypeSpecifier> return_types;

    Scope body;
};

struct UndeclaredIdentifier {
    String name;
    SourceLocation location;
    AstNode *node;
};

s32 const ENV_MAX_DIAG_COUNT = 20;
struct Environment {
    String filename;

    Scope root;
    Scope *current_scope;

    AstNode *parent_node;

    DArray<UndeclaredIdentifier> undeclared_identifiers;

    s32 diag_count;
    DiagnosticMessage diags[ENV_MAX_DIAG_COUNT];
};

inline String full_type_name(Type *type) {
    StringBuilder builder = {};

    append(&builder, type->name);

    if (type->array_size) {
        format(&builder, "[%d]", type->array_size);
    }

    for (u16 i = 0; i < type->pointer_depth; i += 1) {
        append(&builder, '&');
    }

    return to_allocated_string(&builder, temporary_allocator());
}

