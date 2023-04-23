#include "type_check.h"

#include "platform.h"
#include "hash_table.h"


enum {
	TYPE_ID_UNDEFINED,

	TYPE_ID_BUILTIN_S8,
	TYPE_ID_BUILTIN_U8,

	TYPE_ID_BUILTIN_S16,
	TYPE_ID_BUILTIN_U16,

	TYPE_ID_BUILTIN_S32,
	TYPE_ID_BUILTIN_U32,

	TYPE_ID_BUILTIN_S64,
	TYPE_ID_BUILTIN_U64,

	TYPE_ID_BUILTIN_R32,
	TYPE_ID_BUILTIN_R64,

	TYPE_ID_BUILTIN_COUNT
};

INTERNAL u32 CurrentID = TYPE_ID_BUILTIN_COUNT;


INTERNAL bool is_same_type(TypeInfo *lhs, TypeInfo *rhs) {
	if (lhs->decl != rhs->decl) return false;
	if (lhs->array_size != rhs->array_size) return false;
	if (lhs->pointer_depth != rhs->pointer_depth) return false;

	return true;
}

INTERNAL bool convert_type_to(TypeInfo *expected, TypeInfo *info) {
	switch (expected->decl->kind) {
	case TYPE_INTEGER: {
		if (info->decl->kind == TYPE_INTEGER) {
			if (expected->array_size != info->array_size) return false;
			if (expected->pointer_depth != info->pointer_depth) return false;

			if (info->decl->int_min < expected->decl->int_min) return false;
			if (info->decl->int_max > expected->decl->int_max) return false;

			*info = *expected;

			return true;
		}
	} break;
	}

	return false;
}

INTERNAL Identifier make_builtin_type(TypeDeclaration *decl) {
	Identifier identifier = {};
	identifier.kind = IDENTIFIER_TYPE;

	identifier.decl = decl;

	return identifier;
}

INTERNAL TypeDeclaration Type_s8  = {TYPE_INTEGER, {}, {}, true, 1, 127, -128};
INTERNAL TypeDeclaration Type_s16 = {TYPE_INTEGER, {}, {}, true, 2, 32767, -32768};
INTERNAL TypeDeclaration Type_s32 = {TYPE_INTEGER, {}, {}, true, 4, 2147483647, -2147483648};
INTERNAL TypeDeclaration Type_s64 = {TYPE_INTEGER, {}, {}, true, 8, 9223372036854775807, -1 - 9223372036854775807};

INTERNAL TypeDeclaration Type_u8  = {TYPE_INTEGER, {}, {}, false, 1, 255, 0};
INTERNAL TypeDeclaration Type_u16 = {TYPE_INTEGER, {}, {}, false, 2, 65535, 0};
INTERNAL TypeDeclaration Type_u32 = {TYPE_INTEGER, {}, {}, false, 4, 4294967295, 0};
INTERNAL TypeDeclaration Type_u64 = {TYPE_INTEGER, {}, {}, false, 8, 18446744073709551615ULL, 0};


INTERNAL TypeInfo TypeInfo_s8  = {&Type_s8,  String("s8")};
INTERNAL TypeInfo TypeInfo_s16 = {&Type_s16, String("s16")};
INTERNAL TypeInfo TypeInfo_s32 = {&Type_s32, String("s32")};
INTERNAL TypeInfo TypeInfo_s64 = {&Type_s64, String("s64")};

INTERNAL TypeInfo TypeInfo_u8  = {&Type_u8,  String("u8")};
INTERNAL TypeInfo TypeInfo_u16 = {&Type_u16, String("u16")};
INTERNAL TypeInfo TypeInfo_u32 = {&Type_u32, String("u32")};
INTERNAL TypeInfo TypeInfo_u64 = {&Type_u64, String("u64")};


INTERNAL void init_builtin_types(AbstractSyntaxTree *tree) {
	init(&tree->global_scope.symbols, 256);

	insert(&tree->global_scope.symbols, String("s8"),  make_builtin_type(&Type_s8));
	insert(&tree->global_scope.symbols, String("s16"), make_builtin_type(&Type_s16));
	insert(&tree->global_scope.symbols, String("s32"), make_builtin_type(&Type_s32));
	insert(&tree->global_scope.symbols, String("s64"), make_builtin_type(&Type_s64));

	insert(&tree->global_scope.symbols, String("u8"),  make_builtin_type(&Type_u8));
	insert(&tree->global_scope.symbols, String("u16"), make_builtin_type(&Type_u16));
	insert(&tree->global_scope.symbols, String("u32"), make_builtin_type(&Type_u32));
	insert(&tree->global_scope.symbols, String("u64"), make_builtin_type(&Type_u64));

	//insert(&tree->global_scope.symbols, String("r32"), make_builtin_type(TYPE_ID_BUILTIN_R32));
	//insert(&tree->global_scope.symbols, String("r64"), make_builtin_type(TYPE_ID_BUILTIN_R64));
}

INTERNAL Identifier *resolve_identifier(Scope *scope, String name) {
	Scope *search = scope;

	Identifier *identifier = find(&search->symbols, name);
	while (!identifier && search->parent) {
		search = search->parent;
		identifier = find(&search->symbols, name);
	}

	return identifier;
}

u32 const IDENTIFIER_NOT_FOUND  = 0x01;
u32 const IDENTIFIER_NOT_A_TYPE = 0x02;
INTERNAL u32 fill_type_info(TypeInfo *info, Scope *scope) {
	Scope *search = scope;

	Identifier *identifier = find(&search->symbols, info->name);
	while (!identifier && search->parent) {
		search = search->parent;
		identifier = find(&search->symbols, info->name);
	}

	if (identifier == 0) return IDENTIFIER_NOT_FOUND;

	if (identifier->kind == IDENTIFIER_TYPE) {
		info->decl = identifier->decl;
		return 0;
	}

	return IDENTIFIER_NOT_A_TYPE;
}

INTERNAL TypeDeclaration *find_type_declaration(Scope *scope, String name) {
	Scope *search = scope;

	Identifier *identifier = find(&search->symbols, name);
	while (!identifier && search->parent) {
		search = search->parent;
		identifier = find(&search->symbols, name);
	}

	if (identifier == 0) return 0;

	if (identifier->kind == IDENTIFIER_TYPE) {
		return identifier->decl;
	}

	return 0;
}

INTERNAL void push_scope(AbstractSyntaxTree *tree, Scope *scope) {
	scope->parent = tree->current_scope;
	tree->current_scope = scope;
}

INTERNAL void pop_scope(AbstractSyntaxTree *tree) {
	assert(tree->current_scope->parent);

	tree->current_scope = tree->current_scope->parent;
}

INTERNAL bool declared_as_type(Scope *scope, String name) {
	Scope *search = scope;

	Identifier *identifier = find(&search->symbols, name);
	while (!identifier && search->parent) {
		search = search->parent;
		identifier = find(&search->symbols, name);
	}

	if (!identifier) {
		return false;
	}

	if (identifier->kind != IDENTIFIER_TYPE) {
		return false;
	}

	return true;
}

INTERNAL bool declare(Scope *scope, String name) {
	Identifier *found = find(&scope->symbols, name);
	if (found) return false;

	insert(&scope->symbols, name, {});

	return true;
}

INTERNAL void define(Scope *scope, String name, Identifier ident) {
	Identifier *found = find(&scope->symbols, name);
	if (!found) {
		insert(&scope->symbols, name, ident);
	} else {
		*found = ident;
	}
}

INTERNAL Identifier make_identifier(TypeInfo *info) {
	Identifier ident = {};
	ident.kind = IDENTIFIER_VARIABLE;
	ident.type = info;

	return ident;
}

INTERNAL bool check(AbstractSyntaxTree *tree, AstNode *node, TypeInfo *expected);
INTERNAL bool infer(AbstractSyntaxTree *tree, AstNode *node) {
	switch (node->kind) {
	case AST_ERROR: {
		print("Node type not set.");
	} break;

	case AST_INTEGER_LITERAL: {
		AstIntegerLiteral *literal = (AstIntegerLiteral*)node;

		if (literal->value < 2147483648) {
			literal->type = TypeInfo_s32;
		} else if (literal->value < 9223372036854775807) {
			literal->type = TypeInfo_s64;
		} else {
			literal->type = TypeInfo_u64;
		}

		literal->type.flags |= TYPE_FLAG_CONSTANT;

		return true;
	} break;

	case AST_IDENTIFIER: {
		AstIdentifier *ident = (AstIdentifier*)node;

		Identifier *identifier = resolve_identifier(tree->current_scope, ident->name);
		if (!identifier) {
			report_diagnostic(DIAGNOSTIC_ERROR, ident->location, tree->filename, format("Undeclared identifier %S.", pr(ident->name)));
			return false;
		}
		if (identifier->kind == IDENTIFIER_TYPE) {
			report_diagnostic(DIAGNOSTIC_ERROR, ident->location, tree->filename, format("Identifier %S is a type.", pr(ident->name)));
			return false;
		}

		ident->type = *identifier->type;

		return true;
	} break;

	case AST_RANGE: {
		AstRange *range = (AstRange*)node;

		if (!infer(tree, range->start)) return false;
		if (!infer(tree, range->end)) return false;

		if (range->start->type.decl->kind != TYPE_INTEGER) {
			report_diagnostic(DIAGNOSTIC_ERROR, range->start->location, tree->filename, "Expression is not an integer.");
		}

		if (range->end->type.decl->kind != TYPE_INTEGER) {
			report_diagnostic(DIAGNOSTIC_ERROR, range->start->location, tree->filename, "Expression is not an integer.");
		}

		if (range->start->type.flags & TYPE_FLAG_CONSTANT && range->end->type.flags & TYPE_FLAG_CONSTANT) {
			range->type.flags |= TYPE_FLAG_CONSTANT;
		}

		range->type.name = "<Range>"; // TODO: make proper range type
		range->type.flags |= TYPE_FLAG_RANGE;
	} break;

	case AST_VARIABLE_DECLARATION: {
		AstVariableDeclaration *decl = (AstVariableDeclaration*)node;

		if (!declare(tree->current_scope, decl->name)) {
			report_diagnostic(DIAGNOSTIC_ERROR, decl->location, tree->filename, "Identifier already defined.");
			return false;
		}

		if (decl->type.name.size == 0) {
			if (decl->expr == 0) {
				report_diagnostic(DIAGNOSTIC_ERROR, decl->location, tree->filename, "Can't infer type without expression.");
				return false;
			}
			if (!infer(tree, decl->expr)) return false;
			decl->type = decl->expr->type;
		} else if (decl->expr) {
			u32 status = fill_type_info(&decl->type, tree->current_scope);
			if (status == IDENTIFIER_NOT_FOUND) {
				report_diagnostic(DIAGNOSTIC_ERROR, decl->location, tree->filename, format("Identifier %S is not declared.", pr(decl->type.name)));
				return false;
			} else if (status == IDENTIFIER_NOT_A_TYPE) {
				report_diagnostic(DIAGNOSTIC_ERROR, decl->location, tree->filename, format("Identifier %S is not a type.", pr(decl->type.name)));
				return false;
			}
			check(tree, decl->expr, &decl->type);
		}

		Identifier ident = {0};
		ident.kind = IDENTIFIER_VARIABLE;
		ident.type = &decl->type;

		define(tree->current_scope, decl->name, ident);

		return true;
	} break;

	case AST_TYPE_DECLARATION: {
		AstTypeDeclaration *decl = (AstTypeDeclaration*)node;

		if (!declare(tree->current_scope, decl->name)) {
			report_diagnostic(DIAGNOSTIC_ERROR, decl->location, tree->filename, "Identifier already defined.");
			return false;
		}

		declare(tree->current_scope, decl->name);

		if (decl->decl_kind == TYPE_DECL_STRUCT) {
			decl->declaration.kind = TYPE_STRUCT;
			decl->declaration.filename = tree->filename;
			decl->declaration.location = decl->location;

			for (s32 i = 0; i < decl->declaration.fields.size; i += 1) {
				TypeField *field = &decl->declaration.fields[i];

				u32 status = fill_type_info(&field->type, tree->current_scope);
				if (status == IDENTIFIER_NOT_FOUND) {
					report_diagnostic(DIAGNOSTIC_ERROR, field->location, tree->filename, format("Identifier %S is not declared.", pr(field->type.name)));
					return false;
				} else if (status == IDENTIFIER_NOT_A_TYPE) {
					report_diagnostic(DIAGNOSTIC_ERROR, field->location, tree->filename, format("Identifier %S is not a type.", pr(field->type.name)));
					return false;
				}

				// TODO: fill offset
			}
			// TODO: calculate size
		} else if (decl->decl_kind == TYPE_DECL_BASIC) {
			assert(decl->expr);

			if (decl->expr->kind == AST_RANGE) {
				AstRange *range = (AstRange*)decl->expr;
				infer(tree, range);

				if (!(range->type.flags & TYPE_FLAG_CONSTANT)) {
					report_diagnostic(DIAGNOSTIC_ERROR, decl->expr->location, tree->filename, "Range must be constant.");
					return false;
				}
			} else {
				report_diagnostic(DIAGNOSTIC_ERROR, decl->expr->location, tree->filename, "Can't declare type from expression.");
			}
		}

		Identifier ident = {};
		ident.kind = IDENTIFIER_TYPE;
		ident.decl = &decl->declaration;

		define(tree->current_scope, decl->name, ident);

		return true;
	} break;

	case AST_FUNCTION_DECLARATION: {
		AstFunctionDeclaration *decl = (AstFunctionDeclaration*)node;

		if (find(&tree->current_scope->symbols, decl->name)) {
			report_diagnostic(DIAGNOSTIC_ERROR, decl->location, tree->filename, format("Identifier %S already defined.", pr(decl->name)));
			return false;
		}

		push_scope(tree, &decl->body);
		if (tree->current_function) {
			report_diagnostic(DIAGNOSTIC_ERROR, decl->location, tree->filename, "Nested functions not allowed.");
			return false;
		}

		tree->current_function = decl;

		for (s32 i = 0; i < decl->params.size; i += 1) {
			AstParameter *param = &decl->params[i];

			if (!declared_as_type(tree->current_scope, param->type.name)) {
				report_diagnostic(DIAGNOSTIC_ERROR, param->location, tree->filename, format("Identifier %S is not a type.", pr(param->type.name)));
				return false;
			}

			define(tree->current_scope, param->name, make_identifier(&param->type));
		}

		for (s32 i = 0; i < decl->body.statements.size; i += 1) {
			infer(tree, decl->body.statements[i]);
		}

		tree->current_function = 0;
		pop_scope(tree);

		return true;
	} break;

	case AST_RETURN: {
		AstReturn *ret = (AstReturn*)node;
		assert(tree->current_function);
		AstFunctionDeclaration *func = tree->current_function;

		if (ret->values.size != func->return_types.size) {
			report_diagnostic(DIAGNOSTIC_ERROR, ret->location, tree->filename, "Return types mismatch.");
			return false;
		}

		for (s32 i = 0; i < func->return_types.size; i += 1) {
			TypeInfo *ret_type = &func->return_types[i];

			u32 status = fill_type_info(ret_type, tree->current_scope);
			if (status == IDENTIFIER_NOT_FOUND) {
				report_diagnostic(DIAGNOSTIC_ERROR, func->location, tree->filename, format("Identifier %S is not declared.", pr(ret_type->name)));
				return false;
			} else if (status == IDENTIFIER_NOT_A_TYPE) {
				report_diagnostic(DIAGNOSTIC_ERROR, func->location, tree->filename, format("Identifier %S is not a type.", pr(ret_type->name)));
				return false;
			}

			if (!declared_as_type(tree->current_scope, ret_type->name)) {
				report_diagnostic(DIAGNOSTIC_ERROR, func->location, tree->filename, format("Identifier %S is not a type.", pr(ret_type->name)));
				return false;
			}

			check(tree, ret->values[i], ret_type);
		}

		return true;
	} break;

	case AST_BINARY_OPERATOR: {
		AstBinaryOperator *op = (AstBinaryOperator*)node;

		if (!infer(tree, op->lhs)) return false;
		if (!infer(tree, op->rhs)) return false;

		if (!is_same_type(&op->lhs->type, &op->rhs->type)) {
			if (!convert_type_to(&op->lhs->type, &op->rhs->type)) {
				report_diagnostic(DIAGNOSTIC_ERROR, op->location, tree->filename, format("Can't convert %S to %S.", pr(op->lhs->type.name), pr(op->rhs->type.name)));
				return false;
			}
		}
	} break;

	default:
		report_diagnostic(DIAGNOSTIC_ERROR, node->location, tree->filename, format("Can't infer ast node with type %d\n", node->kind));
	}

	return false;
}

INTERNAL bool check(AbstractSyntaxTree *tree, AstNode *node, TypeInfo *expected) {
	switch (node->kind) {
	case AST_ERROR: {
		print("Node type not set.");
	} break;

	case AST_INTEGER_LITERAL: {
		AstIntegerLiteral *literal = (AstIntegerLiteral*)node;

		if (expected->decl->kind != TYPE_INTEGER) {
			report_diagnostic(DIAGNOSTIC_ERROR, literal->location, tree->filename, format("Can't convert integer literal to type %S.\n", pr(expected->name)));
			return false;
		}
		if (literal->value <= expected->decl->int_max) {
			literal->type = *expected;
		} else {
			report_diagnostic(DIAGNOSTIC_ERROR, literal->location, tree->filename, "Literal does not fit into type.\n");
			return false;
		}
	} break;

	case AST_IDENTIFIER: {
		AstIdentifier *identifier = (AstIdentifier*)node;

		Identifier *ident = find(&tree->current_scope->symbols, identifier->name);
		if (ident == 0) {
			report_diagnostic(DIAGNOSTIC_ERROR, identifier->location, tree->filename, format("Undeclared identifier %S.", pr(identifier->name)));
			return false;
		}

		if (ident->kind != IDENTIFIER_VARIABLE) {
			report_diagnostic(DIAGNOSTIC_ERROR, identifier->location, tree->filename, format("Identifier %S is not a value or constant.", pr(identifier->name)));
			return false;
		}

		u32 status = fill_type_info(ident->type, tree->current_scope);
		if (status == IDENTIFIER_NOT_FOUND) {
			report_diagnostic(DIAGNOSTIC_ERROR, identifier->location, tree->filename, format("Identifier %S is not declared.", pr(ident->type->name)));
			return false;
		} else if (status == IDENTIFIER_NOT_A_TYPE) {
			report_diagnostic(DIAGNOSTIC_ERROR, identifier->location, tree->filename, format("Identifier %S is not a type.", pr(ident->type->name)));
			return false;
		}

		if (!declared_as_type(tree->current_scope, ident->type->name)) {
			report_diagnostic(DIAGNOSTIC_ERROR, identifier->location, tree->filename, format("Identifier %S is not a type.", pr(ident->type->name)));
			return false;
		}


		if (!is_same_type(expected, ident->type)) {
			if (!convert_type_to(expected, ident->type)) {
				report_diagnostic(DIAGNOSTIC_ERROR, identifier->location, tree->filename, format("Can't convert %S to %S.", pr(expected->name), pr(ident->type->name)));
				return false;
			}
		}

		return true;
	} break;

	case AST_ARRAY_DECLARATION: {
		AstArrayDeclaration *array = (AstArrayDeclaration*)node;

		if (!(expected->flags & TYPE_FLAG_ARRAY)) {
			report_diagnostic(DIAGNOSTIC_ERROR, array->location, tree->filename, "Can't assign array to scalar value.");
			return false;
		}
		if (expected->array_size && expected->array_size != array->elements.size) {
			report_diagnostic(DIAGNOSTIC_ERROR, array->location, tree->filename, "Array size mismatch.");
			return false;
		}

		TypeInfo element_type = *expected;
		element_type.array_size = 0;

		for (s32 i = 0; i < array->elements.size; i += 1) {
			check(tree, array->elements[i], &element_type);
		}
	} break;

	case AST_REFERENCE: {
		AstReference *ref = (AstReference*)node;

		if (expected->pointer_depth == 0) {
			report_diagnostic(DIAGNOSTIC_ERROR, ref->location, tree->filename, "Can't assign reference to non pointer type.");
		}
		TypeInfo ptr = *expected;
		ptr.pointer_depth -= 1;

		return check(tree, ref->expr, &ptr);
	} break;

	case AST_BINARY_OPERATOR: {
		AstBinaryOperator *op = (AstBinaryOperator*)node;

		check(tree, op->lhs, expected);
		check(tree, op->rhs, expected);

		// TODO: check if types can be used with the operator
	} break;

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

