#include "platform.h"

#include "parser.h"


String find_line(u8 *position, SourceLocation loc, String source) {
	u8 *line_begin = position - (loc.column - 1);
	s32 length = loc.column;

	s32 source_left = (position - source.data);
	for (; length < source_left && line_begin[length] != '\n' && line_begin[length] != '\r'; length += 1);

	return String(line_begin, length);
}

s32 count_characters(String string, u8 c) {
	s32 count = 0;

	for (s32 i = 0; i < string.size; i += 1) {
		if (string.data[i] == c) count += 1;
	}

	return count;
}

s32 application_main(Array<String> args) {
	String file_to_parse;
#if DEVELOPER
	file_to_parse = "test.knot";
#else
	if (args.size < 2) {
		print("Missing source file in arguments to compiler.\n");
		return -1;
	}

	file_to_parse = args[1];
#endif // DEVELOPER
       
	String source = platform_read_entire_file(file_to_parse);
	Parser parser = init_parser(file_to_parse, source);

	ParseResult tree = parse_as_knot_code(&parser);

	if (parser.errors.size) {
		print("Encountered %d error(s).\n", parser.errors.size);
		for (s32 i = 0; i < parser.errors.size; i += 1) {
			SyntaxError *error = &parser.errors[i];

			String line = find_line(error->position, error->location, source);
			s32 tab_count = count_characters(line, '\t');
			print("%S:%d:%d: %S\n", pr(error->file), error->location.line, error->location.column, pr(error->message));
			print("%S\n", pr(line));

			// TODO: proper tab handling
			s32 spaces = error->location.column - 1 + (tab_count * 7);
			for (s32 i = 0; i < spaces; i += 1) {
				print(" ");
			}
			print("^\n\n");
		}

		print("Compiler finished with errors.\n");

		return -1;
	}

	print("Compiler finished.\n");

	print("Contructed following AST:\n\n");
	print_ast(&tree);

	return 0;
}

