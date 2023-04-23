#ifndef INCLUDE_GUARD_KNOT_AST_H
#define INCLUDE_GUARD_KNOT_AST_H

#include "definitions.h"
#include "string2.h"
#include "array.h"
#include "hash_table.h"

#include "knot.h"


struct AstNode;
struct TypeField;


enum {
	TYPE_UNDEFINED,
	TYPE_INTEGER,
	TYPE_FLOAT,
	TYPE_RANGE,
	TYPE_STRUCT,
	TYPE_ENUM,
};
struct TypeDeclaration {
	u32 kind;

	String filename;
	SourceLocation location;

	bool is_signed;
	s32 size;
	u64 int_max;
	s64 int_min;

	Array<TypeField> fields;
	// TODO: annotations
};

enum {
	TYPE_FLAG_ARRAY = 0x01,
	TYPE_FLAG_CONSTANT = 0x02,
	TYPE_FLAG_RANGE = 0x04,
};

struct TypeInfo {
	TypeDeclaration *decl;
	String name;

	u16 flags;
	u16 pointer_depth;

	s32 array_size;
};

struct TypeField {
	String name;
	TypeInfo type;
	SourceLocation location;
	s32 offset;
};

enum {
	IDENTIFIER_UNDEFINED,
	IDENTIFIER_VARIABLE,
	IDENTIFIER_TYPE,

	IDENTIFIER_TYPE_COUNT
};
struct Identifier {
	u32 kind;

	union {
		TypeDeclaration *decl;
		TypeInfo *type;
	};
};


struct Scope {
	Scope *parent;

	Array<AstNode*> statements;
	HashTable<String, Identifier, hash> symbols;
};

enum {
	AST_ERROR,

	AST_REFERENCE,
	AST_DEREFERENCE,
	AST_NEGATE,

	AST_IDENTIFIER,
	AST_INTEGER_LITERAL,
	AST_FLOAT_LITERAL,

	AST_BINARY_OPERATOR,
	AST_RANGE,

	AST_VARIABLE_DECLARATION,
	AST_ARRAY_DECLARATION,
	AST_TYPE_DECLARATION,
	AST_FUNCTION_DECLARATION,

	AST_RETURN,

	AST_TYPE_COUNT
};

struct AstNode {
	SourceLocation location;
	u32 kind;
};

struct AstExpression : AstNode {
	TypeInfo type;
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

struct AstRange : AstExpression {
	AstExpression *start;
	AstExpression *end;

	TypeDeclaration range_decl;
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

struct AstIdentifier : AstExpression {
	String name;
};

struct AstArrayDeclaration : AstExpression {
	Array<AstExpression*> elements;
};

struct AstVariableDeclaration : AstNode {
	String name;
	TypeInfo type;
	AstExpression *expr;
};

enum {
	TYPE_DECL_BASIC,
	TYPE_DECL_STRUCT
};
struct AstTypeDeclaration : AstNode {
	u32 decl_kind;
	String name;
	AstExpression *expr;
	TypeDeclaration declaration;
};

struct AstParameter : AstExpression {
	String name;
	u32 position;
};

struct AstReturn : AstNode {
	Array<AstExpression*> values;
};

struct AstFunctionDeclaration : AstNode {
	String name;
	Array<AstParameter> params;
	Array<TypeInfo> return_types;

	Scope body;
};

struct AbstractSyntaxTree {
	String filename;
	Scope global_scope;
	Scope *current_scope;

	AstFunctionDeclaration *current_function;
};

struct Environment {

	Array<Identifier*> exported_symbols;
};

#endif // INCLUDE_GUARD_KNOT_AST_H

