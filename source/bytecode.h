#ifndef INCLUDE_GUARD_KNOT_BYTECODE_H
#define INCLUDE_GUARD_KNOT_BYTECODE_H

#include "ast.h"


enum BytecodeInstruction {
	BYTECODE_NOOP,

	BYTECODE_INTEGER_CONSTANT,

	BYTECODE_ADD_INTEGER_S32,

	BYTECODE_PRINT
};


void convert_to_bytecode(AbstractSyntaxTree *tree);

#endif // INCLUDE_GUARD_KNOT_BYTECODE_H

