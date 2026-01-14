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
        fprintf(stderr, "EnvRef: NULL environment for variable '%s'\n", id);
        runtime_error("unbound variable");
    }

    for (int i = 0; env[i].name != NULL; i++) {
        if (strcmp(env[i].name, id) == 0) {
            if (env[i].val == NULL) {
                fprintf(stderr, "EnvRef: variable '%s' has NULL value\n", id);
                runtime_error("variable has NULL value");
            }
            return env[i].val;
        }
    }

    fprintf(stderr, "EnvRef: unbound variable '%s'\n", id);
    runtime_error("unbound variable");
    return NULL; // unreachable
}

bool is_true(Value* v) {
    if (v == NULL) {
        fprintf(stderr, "is_true: NULL value\n");
        runtime_error("is_true called with NULL");
    }
    if (v->t == BOOLEAN) {
        return v->boolean.value;
    }
    return true;  // Non-false values are truthy
}

Value* Trampoline(Thunk t) {
    while (t.func != NULL) {
        Value** argv = t.argv;
        t = t.func(t.env, t.argc, argv);
        // Free argv after calling (allocated by MakeThunk)
        if (argv != NULL) {
            free(argv);
        }
    }
    return t.result;
}

Thunk MakeComputedThunk(Value* v) {
    Thunk t;
    t.func = NULL;
    t.env = NULL;
    t.argc = 0;
    t.argv = NULL;
    t.result = v;
    return t;
}

Thunk MakeThunk(Lambda_t func, ClosureEnv env, int argc, Value** argv) {
    Thunk t;
    t.func = func;
    t.env = env;
    t.argc = argc;
    // Copy argv to heap to avoid stack issues
    if (argc > 0 && argv != NULL) {
        Value** argv_copy = (Value**)malloc(sizeof(Value*) * argc);
        if (!argv_copy) {
            runtime_error("MakeThunk: malloc failed for argv");
        }
        for (int i = 0; i < argc; i++) {
            argv_copy[i] = argv[i];
        }
        t.argv = argv_copy;
    } else {
        t.argv = NULL;
    }
    t.result = NULL;
    return t;
}

