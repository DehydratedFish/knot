#ifndef INCLUDE_GUARD_KNOT_AST_H
#define INCLUDE_GUARD_KNOT_AST_H

#include "definitions.h"
#include "string2.h"
#include "array.h"
#include "hash_table.h"

#include "knot.h"


struct AstNode;
struct TypeInfo;


struct Identifier {
	String name;
	SourceLocation location;
};

struct TypeSpecifier {
	TypeInfo *type;
	String name;
	u32 array_size;
	u8 pointer_depth;
};

struct StructMember {
	String name;
	TypeSpecifier type;
	s32 offset;
};

enum {
	TYPE_BASIC,
	TYPE_STRUCT,
	TYPE_ENUM,
};
struct TypeInfo {
	u32 kind;
	u32 type_id;
	// annotations
	s32 member_count;
	StructMember *members;
};

enum {
	IDENTIFIER_UNDEFINED,
	IDENTIFIER_CONSTANT,
	IDENTIFIER_VARIABLE,
	IDENTIFIER_TYPE,

	IDENTIFIER_TYPE_COUNT
};
struct IdentifierInfo {
	u32 kind;

	union {
		TypeInfo type_info;
	};
};


struct Scope {
	Scope *parent;

	Array<AstNode*> statements;
	HashTable<String, IdentifierInfo, hash> symbols;
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

struct AstNode {
	TypeSpecifier type;
	SourceLocation location;
	u32 kind;
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

enum {
	ANL_INTEGER,
	ANL_FLOAT,
};
struct AstNumericLiteral : AstExpression {
	String value;
	u32 numeric_kind;
};

struct AstIdentifier : AstExpression {
	String name;
};

struct AstArrayDeclaration : AstExpression {
	Array<AstExpression*> elements;
};

struct AstVariableDeclaration : AstNode {
	Identifier identifier;
	AstExpression *expr;
};

struct AstStructDeclaration : AstNode {
	Identifier identifier;
	Array<StructMember> members;
};

struct AstParameter : AstNode {
	String name;
	u32 position;
};

struct AstReturn : AstNode {
	Array<AstExpression*> values;
};

struct AstFuncitonDeclaration : AstNode {
	Identifier identifier;
	Array<AstParameter> params;
	Array<TypeSpecifier> return_types;

	Scope body;
};

struct AbstractSyntaxTree {
	String filename;
	Scope global_scope;
	Scope *current_scope;
};

#endif // INCLUDE_GUARD_KNOT_AST_H

