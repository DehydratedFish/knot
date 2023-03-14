#ifndef INCLUDE_GUARD_KNOT_AST_H
#define INCLUDE_GUARD_KNOT_AST_H

#include "definitions.h"
#include "string2.h"
#include "array.h"



struct AstNode;

struct Scope {
	Array<AstNode*> statements;
};

enum {
	AST_ERROR,

	AST_REFERENCE,
	AST_DEREFERENCE,

	AST_IDENTIFIER,
	AST_NUMERIC_LITERAL,

	AST_BINARY_OPERATOR,

	AST_VARIABLE_DECLARATION,
	AST_ARRAY_DECLARATION,
	AST_STRUCT_DECLARATION,
	AST_FUNCTION_DECLARATION,

	AST_RETURN,

	AST_TYPE_COUNT
};

struct TypeSpecifier {
	String name;
	u32 id;
	u32 array_size;
	u8 pointer_depth;
};

struct AstNode {
	u32 kind;
	TypeSpecifier type;
};

struct AstExpression : AstNode {
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

struct AstReference : AstExpression {
	AstExpression *expr;
};

struct AstDereference : AstExpression {
	AstExpression *expr;
};

struct AstNumericLiteral : AstExpression {
	String value;
};

struct AstIdentifier : AstExpression {
	String name;
};

struct AstArrayDeclaration : AstExpression {
	Array<AstExpression*> elements;
};

struct AstVariableDeclaration : AstNode {
	String name;
	String declared_type;
	AstExpression *expr;
};

struct AstStructMember {
	String name;
	TypeSpecifier type;
	s32 offset;
};

struct AstStructDeclaration : AstNode {
	String name;
	Array<AstStructMember> members;
};

struct AstParameter : AstNode {
	String name;
	u32 position;
};

struct AstReturn : AstNode {
	Array<AstExpression*> values;
};

struct AstFuncitonDeclaration : AstNode {
	String name;
	Array<AstParameter> params;
	Array<TypeSpecifier> return_types;

	Scope body;
};

#endif // INCLUDE_GUARD_KNOT_AST_H

