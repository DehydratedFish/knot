#pragma once

#include "knot.h"
#include "list.h"
#include "hash_table.h"


struct SourceItem;
struct Type;
struct Identifier;
struct Operand;
struct Lambda;

enum Instruction {
	NOOP,

    STATEMENT,

    IDENTIFIER,
    SCOPE,

    INTEGER_LITERAL,
    STRING_LITERAL,

    ADD,
    SUB,
    MUL,
    DIV,

    TAKE_REFERENCE,

    DECLARE_VARIABLES,
    DECLARE_SYMBOLS,

    STRUCT_DECLARATION,
    FUNCTION_DECLARATION,

    RETURN,
};

enum TypeKind {
    TYPE_UNDECLARED,

    TYPE_INTEGER_LITERAL,

    TYPE_CORE,

    TYPE_ARRAY,

    TYPE_STRUCT,
};

enum CoreTypeKind {
    TYPE_CORE_S8,
    TYPE_CORE_S16,
    TYPE_CORE_S32,
    TYPE_CORE_S64,

    TYPE_CORE_U8,
    TYPE_CORE_U16,
    TYPE_CORE_U32,
    TYPE_CORE_U64,
};
struct CoreType {
    CoreTypeKind kind;
};

struct ArrayType {
    Type *element_type;
    s64 size;
};

struct StructType {
    Array<SourceItem*> fields;
};

struct Type {
    TypeKind kind;

    String name;

    s64 size;
    s64 align;

    s64 pointer_depth;

    union {
        CoreType   core_type;
        ArrayType  array_type;
        StructType struct_type;
    };
};

struct SourceItem {
    String name;
    SourceLocation loc;
    Type type;
};

struct Scope {
    s64 parent; // NOTE: Index so Scopes don't need to be allocated seperately.
    Lambda *lambda;

    HashTable<String, Identifier> symbols;
    Array<u8> instructions;
};

struct StructDeclaration {
    Array<SourceItem> fields;
};

struct Lambda {
    Array<SourceItem> args;
    Array<Type>  returns;
    Scope body;
};

struct TypeDeclaration {
    String name;

    String file;
    SourceLocation loc;

    s32 size_of;

    TypeKind kind;
    union {
        StructDeclaration struc; // TODO: Ugly name.
    };
};


enum OperandKind {
    OPERAND_IDENTIFIER,

    OPERAND_INTEGER_LITERAL,

    OPERAND_SIGNED_INT,

    OPERAND_LAMBDA,

    OPERAND_TYPE_DECLARATION,
};

enum OperandFlags {
    OPERAND_CONSTANT = 0x01,
};

struct Operand {
    OperandKind kind;
    SourceLocation loc;
    Type type;
    u32 flags;

    union {
        String string;
        u64 integer;

        Lambda lambda;
        TypeDeclaration type_decl;
    };
};

struct OverloadSet {
    List<Lambda> lambdas;
};

enum DeclarationFlags {
    DECL_CONSTANT = 0x01,
};

enum IdentifierKind {
    IDENT_UNDECLARED,
    IDENT_SYMBOL,
    IDENT_TYPE,
};
enum IdentifierFlags {
    IDENT_CONSTANT = 0x01,
};
struct Identifier {
    IdentifierKind kind;
    SourceLocation loc;
    u32 flags;

    Type *type;
};


enum TypingKind {
    TYPING_VOID,
    TYPING_UNARY,
    TYPING_BINARY,
};

enum InferResult {
    INFER_DONE,
    INFER_UNDECLARED_IDENTIFIER,
    INFER_ERROR,
};

struct TypingResult {
    TypingKind kind;

    union {
        Operand op;
        struct {
            Operand lhs;
            Operand rhs;
        };
    };
};

struct TypingInfo {
    OperandKind kind;
    SourceItem *item; // TODO: Is this safe or does the pointer change at this stage?
};

struct PausedTypecheck {
    s32 scope;
    s64 ip;
    SourceItem *item;

    Array<TypingInfo> saved_stack;
};

struct TypingContext {
    InferResult last_infer;
    
    Environment *env;

    s64 ip;
    s64 last_instruction;
    s64 next_statement;

    s32 scope;

    List<TypingInfo> typings;
    List<PausedTypecheck> paused;
};



s32 const ENV_MAX_DIAG_COUNT = 20;
struct Environment {
    String filename;

    s32 diag_count;
    DiagnosticMessage diags[ENV_MAX_DIAG_COUNT];

    List<u8> instructions;

    List<Scope> scopes;
    List<SourceItem> source_items;
};

void init(Environment *env);


struct BackpatchIndex {
    s64 value;
};

// TODO: This could be a template so the writes to the memory location
//       would always be correct, but I don't want another template mess
//       here...
struct Backpatch {
    Environment *env;
    s64 index;
    s32 size;
};

// NOTE: The bytecode is not intended to be shared after parsing.
//       So emiting the values in platform endianess should be fine.
template<class Type>
void emit_value(Environment *env, Type value) {
    append(&env->instructions, {(u8*)&value, sizeof(value)});
}

void emit_instruction(Environment *env, Instruction instruction);

void emit_identifier(Environment *env, String ident, SourceLocation loc);

void emit_integer_literal(Environment *env, String value, SourceLocation loc);
void emit_string_literal(Environment *env, String str, SourceLocation loc);

void emit_source_item(Environment *env, SourceItem *item);
void emit_source_item(Environment *env, String name, SourceLocation loc);

Backpatch emit_scope(Environment *env);
void end_scope(Environment *env, Backpatch *scope);

Backpatch emit_backpatch(Environment *env, s32 size);
void fill_backpatch(Backpatch *bp, void *data, s32 size);


b32 type_check(Environment *env);

// NOTE: Evaluate the entire bytecode.
b32 interpret(Environment *env, Array<u8> instructions);

// NOTE: Evaluate only one statement/expression and advance the instruction array.
b32 evaluate(Environment *env, Array<u8> *instructions);

