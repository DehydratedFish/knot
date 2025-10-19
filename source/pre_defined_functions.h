#pragma once

#include "bytecode.h"


inline void operator_plus(Environment *env) {
    emit_instruction(env, FUNCTION_DECLARATION);
    emit_source_location(env, {});

    s32 const args = 2;
    emit_value(env, args);

}

