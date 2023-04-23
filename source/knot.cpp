#include "platform.h"

#include "knot.h"
#include "io.h"
#include "parser.h"
#include "type_check.h"
#include "bytecode.h"


INTERNAL Array<DiagnosticMessage> Diagnostics;
void report_diagnostic(u32 kind, SourceLocation location, String file, String message) {
	DiagnosticMessage msg = {
		{},
		file,
		location,
		kind
	};

	assert(location.ptr != 0);
	append(&Diagnostics, msg);
	Diagnostics[Diagnostics.size - 1].message = (String&&)message;
}

String find_line(SourceLocation loc, String source) {
	u8 *line_begin = loc.ptr - (loc.column - 1);
	s32 length = loc.column;

	s32 source_left = source.size - (loc.ptr - source.data);
	for (; source_left && line_begin[length] != '\n' && line_begin[length] != '\r'; length += 1, source_left -= 1);

	return String(line_begin, length);
}

s32 count_characters(String string, u8 c) {
	s32 count = 0;

	for (s32 i = 0; i < string.size; i += 1) {
		if (string.data[i] == c) count += 1;
	}

	return count;
}


PrintRef DiagnosticKindLookup[] = {
	pr("Note:"),
	pr("Warning:"),
	pr("Error:"),
};

INTERNAL bool print_diagnostics(String source) {
	bool encountered_errors = false;

	if (Diagnostics.size) {
		for (s32 i = 0; i < Diagnostics.size; i += 1) {
			DiagnosticMessage *diag = &Diagnostics[i];

			String line = find_line(diag->location, source);
			s32 tab_count = count_characters(line, '\t');
			print("%S %S:%d:%d: %S\n", DiagnosticKindLookup[diag->kind], pr(diag->file), diag->location.line, diag->location.column, pr(diag->message));
			print("%S\n", pr(line));

			// TODO: proper tab handling
			s32 spaces = diag->location.column - 1 + (tab_count * 7);
			for (s32 i = 0; i < spaces; i += 1) {
				print(" ");
			}
			print("^\n\n");

			if (diag->kind == DIAGNOSTIC_ERROR) encountered_errors = true;
		}
	}

	return encountered_errors;
}

s32 application_main(Array<String> args) {
	String file_to_parse;
#if DEVELOPER
	file_to_parse = "../examples/develop.knot";
#else
	if (args.size < 2) {
		print("Missing source file in arguments to compiler.\n");
		return -1;
	}

	file_to_parse = args[1];
#endif // DEVELOPER


	String source = platform_read_entire_file(file_to_parse);
	Parser parser = init_parser(file_to_parse, source);

	AbstractSyntaxTree tree = parse_as_knot_code(&parser);
	if (print_diagnostics(source)) {
		print("Compiler finished with errors.");
		return -1;
	}

	print("Type checking.\n");
	type_check_ast(&tree);
	if (print_diagnostics(source)) {
		print("Compiler finished with errors.");
		return -1;
	}

	print("\nCompiler finished.\n");

	//print("Contructed following AST:\n\n");
	//print_ast(&tree);
	
	//convert_to_bytecode(&tree);

	return 0;
}

