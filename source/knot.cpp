#include "list.h"
#include "platform.h"

#include "knot.h"
#include "parser.h"
#include "string2.h"


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


INTERNAL void print_diagnostics(Environment *env, String source) {
    FOR (env->diagnostics, diag) {
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
}

s32 application_main(Array<String> args) {
    String file_to_parse;

    if (args.size < 2) {
        print("Missing source file in arguments to compiler.\n");
        return -1;
    }

    // TODO: Change backslashes to slashes.
    file_to_parse = args[1];

    PlatformReadResult file_result = platform_read_entire_file(file_to_parse);
    if (file_result.error != PLATFORM_READ_OK) {
        print("Could not open or read file %S.\n", file_to_parse);

        return -1;
    }

    String source = file_result.content;
    Parser parser = init_parser(file_to_parse, source);

    developer_print("DEBUG: Parsing\n");
    Environment env = {};
    if (!parse_as_knot_code(&parser, &env)) {
        print_diagnostics(&env, source);
        print("Compiler encountered errors.\n");

        return -1;
    }

    developer_print("DEBUG: Tree\n");
    FOR (env.root.nodes, node) {
        print_syntax_tree(node);
    }

    /*
    print("\nDEBUG: Type checking.\n");
    if (!type_check(&env)) {
        print_diagnostics(&env, source);
        print("Compiler encountered errors.");

        return -1;
    }
    */

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

    print("\nCompilation finished.\n");

    return 0;
}

