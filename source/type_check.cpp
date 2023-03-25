#include "type_check.h"

#include "platform.h"
#include "hash_table.h"


enum {
	TYPE_ID_ERROR,

	TYPE_ID_BUILTIN_S8,
	TYPE_ID_BUILTIN_S16,
	TYPE_ID_BUILTIN_S32,
	TYPE_ID_BUILTIN_S64,

	TYPE_ID_BUILTIN_U8,
	TYPE_ID_BUILTIN_U16,
	TYPE_ID_BUILTIN_U32,
	TYPE_ID_BUILTIN_U64,

	TYPE_ID_BUILTIN_R32,
	TYPE_ID_BUILTIN_R64,

	TYPE_ID_BUILTIN_STRING,
	TYPE_ID_BUILTIN_ARRAY,

	TYPE_ID_BUILTIN_COUNT
};

struct TypeCheckContext {
	Scope global_scope;
	Scope *current_scope;
};


INTERNAL u32 CurrentID = TYPE_ID_BUILTIN_COUNT;
INTERNAL u32 gen_type_id() {
	return CurrentID++;
}

INTERNAL IdentifierInfo make_builtin_type(u32 id) {
	IdentifierInfo identifier = {};
	identifier.kind = IDENTIFIER_TYPE;

	identifier.type_info.kind = TYPE_BASIC;
	identifier.type_info.type_id = id;

	return identifier;
}

INTERNAL void init_builtin_types(AbstractSyntaxTree *tree) {
	init(&tree->global_scope.symbols, 256);

	insert(&tree->global_scope.symbols, String("s8"),  make_builtin_type(TYPE_ID_BUILTIN_S8));
	insert(&tree->global_scope.symbols, String("s16"), make_builtin_type(TYPE_ID_BUILTIN_S16));
	insert(&tree->global_scope.symbols, String("s32"), make_builtin_type(TYPE_ID_BUILTIN_S32));
	insert(&tree->global_scope.symbols, String("s64"), make_builtin_type(TYPE_ID_BUILTIN_S64));

	insert(&tree->global_scope.symbols, String("u8"),  make_builtin_type(TYPE_ID_BUILTIN_U8));
	insert(&tree->global_scope.symbols, String("u16"), make_builtin_type(TYPE_ID_BUILTIN_U16));
	insert(&tree->global_scope.symbols, String("u32"), make_builtin_type(TYPE_ID_BUILTIN_U32));
	insert(&tree->global_scope.symbols, String("u64"), make_builtin_type(TYPE_ID_BUILTIN_U64));

	insert(&tree->global_scope.symbols, String("r32"), make_builtin_type(TYPE_ID_BUILTIN_R32));
	insert(&tree->global_scope.symbols, String("r64"), make_builtin_type(TYPE_ID_BUILTIN_R64));
}

INTERNAL TypeInfo *type_info(Scope *scope, String name, SourceLocation location) {
	IdentifierInfo *identifier = find(&scope->symbols, name);
	if (!identifier) {
		report_diagnostic(DIAGNOSTIC_ERROR, location, "[placeholder]", format("Undeclared identifier %S.", pr(name)));
		return 0;
	}

	if (identifier->kind != IDENTIFIER_TYPE) {
		report_diagnostic(DIAGNOSTIC_ERROR, location, "[placeholder]", format("Identifier %S is not a type.", pr(name)));
		return 0;
	}

	return &identifier->type_info;
}

INTERNAL bool declare(Scope *scope, Identifier identifier, IdentifierInfo info) {
	IdentifierInfo *found = find(&scope->symbols, identifier.name);
	if (found) {
		report_diagnostic(DIAGNOSTIC_ERROR, identifier.location, "[placeholder]", format("Identifier %S is already defined.", pr(identifier.name)));
		return false;
	}

	insert(&scope->symbols, identifier.name, info);

	return true;
}

INTERNAL bool check(AbstractSyntaxTree *tree, AstNode *node, TypeSpecifier *expected);
INTERNAL TypeSpecifier infer(AbstractSyntaxTree *tree, AstNode *node) {
	TypeSpecifier type = {};

	switch (node->kind) {
	case AST_ERROR: {
		print("Node type not set.");
	} break;

	case AST_VARIABLE_DECLARATION: {
		AstVariableDeclaration *decl = (AstVariableDeclaration*)node;

		if (decl->type.name.size == 0) {
			if (decl->expr == 0) {
				// TODO: proper error reporting
				print("Can't infer type without expression.");
				return type;
			}
			decl->type = infer(tree, decl->expr);
			return decl->type;
		}

		decl->type.type = type_info(&tree->global_scope, decl->type.name, decl->location);
		if (decl->type.type == 0) return type;
		if (decl->expr) {
			check(tree, decl->expr, &decl->type);
		}
	} break;

	case AST_STRUCT_DECLARATION: {
		AstStructDeclaration *decl = (AstStructDeclaration*)node;

		IdentifierInfo info = {};
		info.kind = IDENTIFIER_TYPE;
		info.type_info.kind = TYPE_STRUCT;
		info.type_info.type_id = CurrentID++;
		info.type_info.member_count = decl->members.size;
		info.type_info.members = decl->members.data;

		// TODO: declare struct before members
		if (!declare(tree->current_scope, decl->identifier, info)) return type;
	} break;

	//case AST_FUNCTION_DECLARATION: {
	//	AstFuncitonDeclaration *decl = (AstFuncitonDeclaration*)node;
	//} break;

	//case AST_RETURN: {
	//	AstReturn *ret = (AstReturn*)node;
	//} break;

	default:
		report_diagnostic(DIAGNOSTIC_ERROR, node->location, tree->filename, format("Can't check ast node with type %d\n", node->kind));
	}

	return type;
}

INTERNAL bool check(AbstractSyntaxTree *tree, AstNode *node, TypeSpecifier *expected) {
	switch (node->kind) {
	case AST_ERROR: {
		print("Node type not set.");
	} break;

	case AST_NUMERIC_LITERAL: {
		AstNumericLiteral *literal = (AstNumericLiteral*)node;

		if (literal->numeric_kind == ANL_INTEGER) {
			if (expected->type->type_id < TYPE_ID_BUILTIN_S8 || expected->type->type_id > TYPE_ID_BUILTIN_U64) {
				report_diagnostic(DIAGNOSTIC_ERROR, literal->location, tree->filename, format("Can't convert literal to %S.\n", pr(expected->name)));

				return false;
			}

			return true;
		}

		report_diagnostic(DIAGNOSTIC_ERROR, literal->location, tree->filename, "Bad literal.\n");
		// TODO: check if integer fits into specified type
	} break;

	//case AST_VARIABLE_DECLARATION: {
	//	AstVariableDeclaration *decl = (AstVariableDeclaration*)node;
	//} break;

	//case AST_FUNCTION_DECLARATION: {
	//	AstFuncitonDeclaration *decl = (AstFuncitonDeclaration*)node;
	//} break;

	//case AST_RETURN: {
	//	AstReturn *ret = (AstReturn*)node;
	//} break;

	default:
		report_diagnostic(DIAGNOSTIC_ERROR, node->location, tree->filename, format("Can't check ast node with type %d\n", node->kind));
	}

	return false;
}


void type_check_ast(AbstractSyntaxTree *tree) {
	init_builtin_types(tree);
	tree->current_scope = &tree->global_scope;

	for (s32 i = 0; i < tree->global_scope.statements.size; i += 1) {
		infer(tree, tree->global_scope.statements[i]);
	}
}

