#include "platform.h"

#include "parser.h"


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

	String source = platform_read_entire_file("test.knot");
	Parser parser = init_parser(source);

	ParseResult tree = parse_as_knot_code(&parser);

	if (parser.errors.size) {
		for (s32 i = 0; i < parser.errors.size; i += 1) {
			SyntaxError *error = &parser.errors[i];
			print("%S\nline: %d, column: %d\n", error->message, error->location.line, error->location.column);
		}

		return -1;
	}

	return 0;
}

