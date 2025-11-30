#pragma once

#include "definitions.h"
#include "syntax.h"

struct Environment;


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

void report_diagnostic(Environment *env, DiagnosticKind kind, SourceLocation location, String message);
void report_error(Environment *env, SourceLocation location, String message);


struct Atom {
    s64 handle;
};

Atom   generate_atom(String str);
String get_string(Atom atom);


struct Environment {
    String filename;
    String source;

    List<DiagnosticMessage> diagnostics;
    b32 has_errors;

    SyntaxScope  root;
    SyntaxScope *current_scope;

    SyntaxLambda *current_lambda;
};

