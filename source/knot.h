#pragma once

#include "definitions.h"
#include "syntax.h"


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


struct Environment {
    String filename;

    List<DiagnosticMessage> diagnostics;

    SyntaxScope root;
};

