#include "bytecode.h"

#include "platform.h" // NOTE: for print as of now


struct Interpreter {
	String code;
};


void instruction(Interpreter *interpreter, BytecodeInstruction instruction) {
	put(&interpreter->code, instruction);
}

void push_s32(Interpreter *interpreter, s32 value) {
	put(&interpreter->code, BYTECODE_ADD_INTEGER_S32);

	append(&interpreter->code, (u8*)&value, sizeof(value));
}

void node_to_bytecode(Interpreter *interpreter, AstNode *node) {
	switch (node->kind) {
	case AST_BINARY_OPERATOR: {
		AstBinaryOperator *op = (AstBinaryOperator*)node;

		print("Location: %d:%d\n", op->location.line, op->location.column);

		instruction(interpreter, BYTECODE_ADD_INTEGER_S32);

		node_to_bytecode(interpreter, op->lhs);
		node_to_bytecode(interpreter, op->rhs);
	} break;

	case AST_INTEGER_LITERAL: {
		AstIntegerLiteral *literal = (AstIntegerLiteral*)node;

		push_s32(interpreter, (s32)literal->value);
	} break;

#if 0
	case AST_FUNCTION_DECLARATION: {
		AstFunctionDeclaration *decl = (AstFunctionDeclaration*)node;

		for (s32 i = 0; i < decl->body.statements.size; i += 1) {
			node_to_bytecode(interpreter, decl->body.statements[i]);
		}
	} break;

	case AST_VARIABLE_DECLARATION: {
		AstVariableDeclaration *decl = (AstVariableDeclaration*)node;

		if (decl->expr) {
			node_to_bytecode(interpreter, decl->expr);
		}
	} break;
#endif
	}
}

void run(Interpreter *interpreter) {
	String code = interpreter->code;

	u8 instruction;
	while (get(&code, &instruction)) {
		switch (instruction) {
		case BYTECODE_ADD_INTEGER_S32: {
			s32 lhs;
			get(&code, &lhs);
			print("lhs: %d\n", lhs);

			s32 rhs;
			get(&code, &rhs);
			print("rhs: %d\n", rhs);

			print("%d\n", lhs + rhs);
			push_s32(interpreter, lhs + rhs);
		}
		}
	}
}

//void convert_to_bytecode(AbstractSyntaxTree *tree) {
//	Interpreter interpreter = {};
//	prealloc(&interpreter.code, MEGABYTES(4));
//
//	for (s32 i = 0; i < tree->global_scope.nodes.size; i += 1) {
//		node_to_bytecode(&interpreter, tree->global_scope.nodes[i]);
//	}
//
//
//	run(&interpreter);
//}

