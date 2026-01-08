#include "ksu_runtime.h"
#include <stdarg.h>

void runtime_error(const char* msg) {
    fprintf(stderr, "Runtime error: %s\n", msg);
    exit(1);
}

ClosureEnv MakeEnv(int count, ...) {
    if (count == 0) {
        return NULL;
    }

    EnvEntry* env = (EnvEntry*)malloc(sizeof(EnvEntry) * (count + 1));
    if (!env) {
        runtime_error("failed to allocate environment");
    }

    va_list args;
    va_start(args, count);

    for (int i = 0; i < count; i++) {
        char* name = va_arg(args, char*);
        Value* val_ptr = va_arg(args, Value*);
        env[i].name = name;
        env[i].val = val_ptr;
    }

    env[count].name = NULL;

    va_end(args);
    return env;
}

Value* EnvRef(ClosureEnv env, const char* id) {
    if (!env) {
        runtime_error("unbound variable");
    }

    for (int i = 0; env[i].name != NULL; i++) {
        if (strcmp(env[i].name, id) == 0) {
            return env[i].val;
        }
    }

    runtime_error("unbound variable");
    return NULL; // unreachable
}

bool is_true(Value* v) {
    if (v->t == BOOLEAN) {
        return v->boolean.value;
    }
    return true;  // Non-false values are truthy
}
