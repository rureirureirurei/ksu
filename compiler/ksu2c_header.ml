let header = {|
#include <stdio.h>
#include <stdlib.h>

static void runtime_error(const char* message);

enum Tag { INT, BOOL, STRING, LAMBDA, PAIR, NIL, CLOSURE, ENV };

union Value;
typedef union Value Value;
typedef union Value (*Lambda)();

struct Closure {
    enum Tag t;
    Lambda lam;
    void* env;
};

struct Env {
    enum Tag t;
    void* env;
};

struct Int {
    enum Tag t;
    int value;
};

struct Bool {
    enum Tag t;
    int value;
};

struct String {
    enum Tag t;
    const char* str;
};

struct Pair {
    enum Tag t;
    union Value* fst;
    union Value* snd;
};
typedef struct Pair Pair;

union Value {
    enum Tag t;
    struct Int z;
    struct Bool b;
    struct String s;
    struct Closure clo;
    struct Env env;
    struct Pair pair;
};

static Value MakeInt(int n) {
    Value v;
    v.t = INT;
    v.z.value = n;
    return v;
}

static Value MakeBool(int b) {
    Value v;
    v.t = BOOL;
    v.b.value = b;
    return v;
}

static Value MakeString(const char* s) {
    Value v;
    v.t = STRING;
    v.s.str = s;
    return v;
}

static Value MakeNil() {
    Value v;
    v.t = NIL;
    return v;
}

static Value __attribute__((const)) MakeClosure(Lambda lam, Value env) {
    Value v;
    v.t = CLOSURE;
    v.clo.lam = lam;
    v.clo.env = env.env.env;
    return v;
}

static Value MakeEnv(void* env) {
    Value v ;
    v.env.t = ENV ;
    v.env.env = env ;
    return v ;
}

static Value EnvRef(Value env, Value var_name) {
    if (env.t != ENV) {
        runtime_error("EnvRef expects an environment");
    }
    // This is a simplified implementation - in practice, you'd need to look up the variable
    // in the environment structure based on the variable name
    // For now, we'll return a placeholder
    return MakeInt(0);
}

static Value MakePrimitive(Lambda prim) {
    Value v ;
    v.clo.t = CLOSURE ;
    v.clo.lam = prim ;
    v.clo.env = NULL ;
    return v ;
}

static void runtime_error(const char* message) {
    fprintf(stderr, "Runtime error: %s\n", message);
    exit(1);
}

// =============== BUILTINS ===============

static Value __builtin_car(Value list) {
    if (list.t != PAIR) {
        runtime_error("car expects a pair");
    }
    return *(list.pair.fst);
}

static Value __builtin_cdr(Value list) {
    if (list.t != PAIR) {
        runtime_error("cdr expects a pair");
    }
    return *(list.pair.snd);
}

static Value __builtin_cons(Value fst, Value snd) {
    Value *fstptr = malloc(sizeof(Value));
    Value *sndptr = malloc(sizeof(Value));
    *fstptr = fst;
    *sndptr = snd;
    
    Value v;
    v.t = PAIR;
    v.pair.fst = fstptr;
    v.pair.snd = sndptr;
    return v;
}

static Value __builtin_is_cons(Value v) {
    return MakeBool(v.t == PAIR);
}

static Value __builtin_is_null(Value v) {
    return MakeBool(v.t == NIL);
}

static Value __builtin_is_bool(Value v) {
    return MakeBool(v.t == BOOL);
}

static Value __builtin_is_int(Value v) {
    return MakeBool(v.t == INT);
}

static Value __builtin_is_lambda(Value v) {
    return MakeBool(v.t == LAMBDA);
}

static Value __builtin_is_list(Value v) {
    while (v.t == PAIR) {
        v = *(v.pair.snd);
    }
    return MakeBool(v.t == NIL);
}

static Value __builtin_eq(Value a, Value b) {
    if (a.t != b.t) return MakeBool(0);
    switch (a.t) {
        case INT: return MakeBool(a.z.value == b.z.value);
        case BOOL: return MakeBool(a.b.value == b.b.value);
        default: runtime_error("Can only compare ints and bools");
    }
}

static Value __builtin_list_ref(Value list, Value idx) {
    if (idx.t != INT) {
        runtime_error("list-ref expects an integer index");
    }
    int n = idx.z.value;
    Value cur = list;
    while (n > 0) {
        if (cur.t != PAIR) {
            runtime_error("list-ref index out of range or not a list");
        }
        cur = *(cur.pair.snd);
        n--;
    }
    if (cur.t != PAIR) {
        runtime_error("list-ref expects a pair at the final position");
    }
    return *(cur.pair.fst);
}

static void ensure_int_pair(Value a, Value b, const char* op) {
    if (a.t != INT || b.t != INT) {
        runtime_error(op);
    }
}

static Value __builtin_add(Value a, Value b) {
    ensure_int_pair(a, b, "+ expects two integers");
    return MakeInt(a.z.value + b.z.value);
}

static Value __builtin_sub(Value a, Value b) {
    ensure_int_pair(a, b, "- expects two integers");
    return MakeInt(a.z.value - b.z.value);
}

static Value __builtin_mul(Value a, Value b) {
    ensure_int_pair(a, b, "* expects two integers");
    return MakeInt(a.z.value * b.z.value);
}

static Value __builtin_div(Value a, Value b) {
    ensure_int_pair(a, b, "/ expects two integers");
    if (b.z.value == 0) {
        runtime_error("division by zero");
    }
    return MakeInt(a.z.value / b.z.value);
}

static Value __builtin_lt(Value a, Value b) {
    ensure_int_pair(a, b, "< expects two integers");
    return MakeBool(a.z.value < b.z.value);
}

static Value __builtin_gt(Value a, Value b) {
    ensure_int_pair(a, b, "> expects two integers");
    return MakeBool(a.z.value > b.z.value);
}

static Value __builtin_le(Value a, Value b) {
    ensure_int_pair(a, b, "<= expects two integers");
    return MakeBool(a.z.value <= b.z.value);
}

static Value __builtin_ge(Value a, Value b) {
    ensure_int_pair(a, b, ">= expects two integers");
    return MakeBool(a.z.value >= b.z.value);
}

static Value __builtin_and(Value a, Value b) {
    if (a.t != BOOL || b.t != BOOL) {
        runtime_error("and expects two booleans");
    }
    return MakeBool(a.b.value && b.b.value);
}

static Value __builtin_or(Value a, Value b) {
    if (a.t != BOOL || b.t != BOOL) {
        runtime_error("or expects two booleans");
    }
    return MakeBool(a.b.value || b.b.value);
}

static Value __builtin_not(Value a) {
    if (a.t != BOOL) {
        runtime_error("not expects a boolean");
    }
    return MakeBool(!a.b.value);
}

static int __builtin_is_true(Value a) {
    if (a.t != BOOL) {
        runtime_error("is_true expects a boolean");
    }
    return a.b.value;
}

static Value __builtin_print(Value a) {
    switch (a.t) {
        case INT:
            printf("%d\n", a.z.value);
            break;
        case BOOL:
            printf("%s\n", a.b.value ? "#t" : "#f");
            break;
        case STRING:
            printf("%s\n", a.s.str);
            break;
        default:
            runtime_error("print expects int, bool, or string");
    }
    return a;
}

static Value __builtin_ne(Value a, Value b) {
    if (a.t != b.t) return MakeBool(1);
    switch (a.t) {
        case INT: return MakeBool(a.z.value != b.z.value);
        case BOOL: return MakeBool(a.b.value != b.b.value);
        default: runtime_error("Can only compare ints and bools");
    }
}
|}