#ifndef INCLUDE_GUARD_KNOT_H
#define INCLUDE_GUARD_KNOT_H

#include "definitions.h"
#include "string2.h"


struct SourceLocation {
	u8 *ptr;
	s32 line;
	s32 column;
};

enum {
	DIAGNOSTIC_NOTE,
	DIAGNOSTIC_WARNING,
	DIAGNOSTIC_ERROR,

	DIAGNOSTIC_COUNT
};
struct DiagnosticMessage {
	String message;
	String file;
	SourceLocation location;
	u32 kind;
};


void report_diagnostic(u32 kind, SourceLocation location, String file, String message);

#endif // INCLUDE_GUARD_KNOT_H

