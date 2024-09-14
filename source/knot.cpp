#include "platform.h"

#include "knot.h"
#include "io.h"
#include "parser.h"
#include "type_check.h"
#include "bytecode.h"


void report_diagnostic(Environment *env, DiagnosticKind kind, SourceLocation location, String message) {
	DiagnosticMessage msg = {
		{},
		env->filename,
		location,
		kind
	};

	assert(location.ptr != 0);
	if (env->diag_count < ENV_MAX_DIAG_COUNT) {
		env->diags[env->diag_count] = msg;
		env->diags[env->diag_count].message = (String&&)message;
		env->diag_count += 1;
	}
}

String find_line(SourceLocation loc, String source) {
	u8 *line_begin = loc.ptr - (loc.column - 1);
	s32 length = loc.column;

	s64 source_left = source.size - (loc.ptr - source.data);
	for (; source_left && line_begin[length] != '\n' && line_begin[length] != '\r'; length += 1, source_left -= 1);

	return String(line_begin, length);
}

INTERNAL s32 trim_indentation(String *line) {
    s32 removed = 0;

    for (s64 i = 0; i < line->size; i += 1) {
        if (line->data[i] != ' ' && line->data[i] != '\t') break;

        removed += 1;
    }

    *line = shrink_front(*line, removed);

    return removed;
}


INTERNAL void print_diagnostics(Environment *env, String source) {
	for (s32 i = 0; i < env->diag_count; i += 1) {
		DiagnosticMessage *diag = &env->diags[i];

		String line = find_line(diag->location, source);
		s32 line_start = trim_indentation(&line);

        String prefix = {};
        if (diag->kind == DIAGNOSTIC_NOTE) {
            prefix = "Note: ";
        } else if (diag->kind == DIAGNOSTIC_WARNING) {
            prefix = "Warning: ";
        } else if (diag->kind == DIAGNOSTIC_ERROR) {
            prefix = "Error: ";
        } else {
            assert(false);
        }

		print("%S%S:%d:%d: %S\n", prefix, diag->file, diag->location.line, diag->location.column, diag->message);
		print("%S\n", line);

		s32 spaces = diag->location.column - line_start - 1;
		for (s32 i = 0; i < spaces; i += 1) {
			print(" ");
		}
		print("^\n\n");
	}

    if (env->diag_count == ENV_MAX_DIAG_COUNT) {
        print("More than %d diagnostics encountered.", ENV_MAX_DIAG_COUNT);
    }
}

s32 application_main(Array<String> args) {
	String file_to_parse;
#if DEVELOPER
	file_to_parse = "../../../examples/develop.knot";
#else
	if (args.size < 2) {
		print("Missing source file in arguments to compiler.\n");
		return -1;
	}

	file_to_parse = args[1];
#endif // DEVELOPER

	PlatformReadResult file_result = platform_read_entire_file(file_to_parse);
    if (file_result.status != PLATFORM_READ_OK) {
        print("Could not open or read file %S.\n", file_to_parse);

        return -1;
    }

    String source = file_result.content;
	Parser parser = init_parser(file_to_parse, source);

    print("DEBUG: Parsing\n");
	Environment env = {};
	if (!parse_as_knot_code(&parser, &env)) {
		print_diagnostics(&env, source);
		if (env.diag_count == ENV_MAX_DIAG_COUNT) {
			print("More than %d diagnostics encountered.", ENV_MAX_DIAG_COUNT);
		}
		print("Compiler encountered errors.");

		return -1;
	}

	print("DEBUG: Type checking.\n");
	if (!type_check_ast(&env)) {
		print_diagnostics(&env, source);
		print("Compiler encountered %d errors.", env.diag_count);

		return -1;
	}

	print("\nCompilation finished.\n");

	//print("Contructed following AST:\n\n");
	//print_ast(&tree);
	
	//convert_to_bytecode(&tree);

	return 0;
}



// TODO: Move this away. I don't know where else to put it right now.
INTERNAL u32 lookup(u64 hash, u32 exponent, u32 index) {
    u32 mask = ((u32)1 << exponent) - 1;
    u32 step = (u32)(hash >> (64 - exponent)) | 1;

    return (index + step) & mask;
}

INTERNAL u64 symbol_hash(String str) {
    u64 h = 0x100;

    for (s64 i = 0; i < str.size; i += 1) {
        h ^= str.data[i] & 255;
        h *= 1111111111111111111;
    }

    return h ^ h >> 32;
}

void init(SymbolTable *table, u32 exponent, Allocator alloc) {
    assert(exponent >= 0 && exponent < 32);

    if (table->used) {
        DEALLOC(table->allocator, table->entries, sizeof(*table->entries));
    }

    u32 entry_count = 1 << exponent;
    r32 load_factor = 0.5f;

    INIT_STRUCT(table);
    table->allocator = alloc;
    table->entries  = ALLOC(table->allocator, Identifier, entry_count);
    table->exponent = exponent;
    table->max_load = (u32)(entry_count * load_factor);
}

Identifier *find(SymbolTable *table, String name, Allocator alloc) {
    u32 const default_exponent = 6;
    if (table->exponent == 0) init(table, default_exponent, alloc);

    u64 h = symbol_hash(name);
    for (u32 i = (u32)h;;) {
        i = lookup(h, table->exponent, i);

        Identifier *entry = &table->entries[i];
        if (entry->kind == IDENTIFIER_UNDEFINED) {
            if (table->max_load == table->used) {
                SymbolTable new_table = {};
                init(&new_table, table->exponent += 1, table->allocator);

                u32 entry_count = 1 << table->exponent;

                for (u32 i = 0; i < entry_count; i += 1) {
                    if (table->entries[i].kind != IDENTIFIER_UNDEFINED) {
                        *find(&new_table, table->entries[i].name) = table->entries[i];
                    }
                }

                DEALLOC(table->allocator, table->entries, entry_count);
                *table = new_table;
            }

            entry->name = name;
            table->used += 1;

            return entry;
        } else if (entry->name == name) {
            return entry;
        }
    }
}

