#include "knot.h"

#include "platform.h"

#include "parser.h"
#include "string2.h"

#include "list.h"
#include "hash_table.h"
#include "arena.h"

#include "type_check.h"
#include "codegen.h"


INTERNAL void developer_print(String msg) {
#ifdef DEVELOPER
    print(msg);
#endif // DEVELOPER
}


void report_diagnostic(Environment *env, DiagnosticKind kind, SourceLocation location, String message) {
    DiagnosticMessage msg = {
        allocate_string(message),
        env->filename,
        location,
        kind
    };

    append(&env->diagnostics, msg);

    if (kind == DIAGNOSTIC_ERROR) env->has_errors = true;
}

void report_error(Environment *env, SourceLocation location, String message) {
    report_diagnostic(env, DIAGNOSTIC_ERROR, location, message);
}

String find_line(SourceLocation loc, String source) {
    u8 *line_begin = &source[loc.pos - (loc.column - 1)];
    s32 length = loc.column;

    s64 source_left = source.size - (loc.pos - (s64)source.data);
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


INTERNAL void print_diagnostics(Environment *env) {
    FOR (env->diagnostics, diag) {
        String prefix = {};
        if (diag->kind == DIAGNOSTIC_NOTE) {
            prefix = "Note: ";
        } else if (diag->kind == DIAGNOSTIC_WARNING) {
            prefix = "Warning: ";
        } else if (diag->kind == DIAGNOSTIC_ERROR) {
            prefix = "Error: ";
        } else {
            die("Unknown DiagnosticKind.");
        }

        if (diag->location.pos == -1) {
            print("%S%S: %S\n", prefix, diag->file, diag->message);
        } else {
            String line = find_line(diag->location, env->source);
            s32 line_start = trim_indentation(&line);

            print("%S%S:%d:%d: %S\n", prefix, diag->file, diag->location.line, diag->location.column, diag->message);
            print("%S\n", line);

            s32 spaces = diag->location.column - line_start - 1;
            for (s32 i = 0; i < spaces; i += 1) {
                print(" ");
            }
            print("^\n\n");
        }
    }
}


// TODO: I don't like the intermediate array very much but for now it must suffice.
//       Also the Arena should be growable. What about long strings? Probably should not be allowed.
INTERNAL HashTable<String, Atom> AtomLookup;
INTERNAL List<String>            AtomList;
INTERNAL MemoryArena             AtomStorage;

INTERNAL void init_atom_storage() {
    init(&AtomStorage, MEGABYTES(1));
}

INTERNAL void destroy_atom_storage() {
    destroy(&AtomStorage);
    destroy(&AtomList);
    destroy(&AtomLookup);
}

Atom generate_atom(String str) {
    auto *found = find(&AtomLookup, str);
    if (found) return *found;

    String new_atom = {};
    new_atom.data = (u8*)allocate_from_arena(&AtomStorage, str.size, 0, 0);
    new_atom.size = str.size;

    Atom atom = {AtomList.size};
    append(&AtomList, new_atom);

    return *insert(&AtomLookup, new_atom, atom);
}

String get_string(Atom atom) {
    BOUNDS_CHECK(0, AtomList.size, atom.handle, "Atom lookup out of bounds.");

    return AtomList[atom.handle];
}

s32 application_main(Array<String> args) {
    String file_to_parse;

    if (args.size < 2) {
        print("Missing source file in arguments to compiler.\n");
        return -1;
    }

    init_atom_storage();
    DEFER(destroy_atom_storage());

    // TODO: Change backslashes to slashes.
    file_to_parse = args[1];

    developer_print("DEBUG: Parsing\n");
    Environment env = parse_knot_file(file_to_parse);
    if (env.has_errors) {
        print_diagnostics(&env);
        print("Compiler encountered errors.\n");

        return -1;
    }

    /*
    developer_print("DEBUG: Tree\n");
    FOR (env.root.nodes, node) {
        print_syntax_tree(node);
    }
    */

    developer_print("\nDEBUG: Type checking.\n");
    if (!type_check(&env)) {
        print_diagnostics(&env);
        print("Compiler encountered errors.\n");

        return -1;
    }

    /*
       print("\nDEBUG: Interpreting\n");
       Array<u8> bytecode = {env.bytecode_instructions.data, env.bytecode_instructions.size};
       if (!interpret(&env, bytecode)) {
       print_diagnostics(&env, source);
       if (env.diag_count == ENV_MAX_DIAG_COUNT) {
       print("More than %d diagnostics encountered.", ENV_MAX_DIAG_COUNT);
       }
       print("Compiler encountered errors.");

       return -1;
       }
    */


    developer_print("\nDEBUG: Codegen.\n");
    codegen_llvm(&env);

    print("\nCompilation finished.\n");

    return 0;
}

