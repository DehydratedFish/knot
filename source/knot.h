#pragma once

#include "definitions.h"


struct SourceLocation {
    s64 pos;
	s32 line;
	s32 column;
};

enum DiagnosticKind {
	DIAGNOSTIC_NOTE,
	DIAGNOSTIC_WARNING,
	DIAGNOSTIC_ERROR,
};
struct DiagnosticMessage {
	String message;
	String file;
	SourceLocation location;
	DiagnosticKind kind;
};

void report_diagnostic(struct Environment *env, DiagnosticKind kind, SourceLocation location, String message);
void report_error(Environment *env, SourceLocation location, String message);

