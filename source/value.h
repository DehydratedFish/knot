#pragma once

#include "definitions.h"


struct Type;


struct Value {
    Type *type;
    // TODO: Flags for signedness info?

    union {
        u64 integer;
        String string;
    };
};

