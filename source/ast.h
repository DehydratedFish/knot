#ifndef INCLUDE_GUARD_KNOT_AST_H
#define INCLUDE_GUARD_KNOT_AST_H

#include "definitions.h"
#include "string2.h"
#include "array.h"


struct AstNode {
	u32 type;
};

struct AstExpression : AstNode{

};

struct AstVariableDeclaration : AstNode {
	String name;
	String declared_type;
	AstExpression expr;
};

struct AstStructMember {
	String name;
	String declared_type;
	s32 offset;
};

struct AstStructDeclaration : AstNode {
	String name;
	Array<AstStructMember> members;
};


#endif // INCLUDE_GUARD_KNOT_AST_H

