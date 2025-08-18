let header = {|
#include <stdio.h>
#include <stdlib.h>

enum Tag { INT, BOOL, LAMBDA, PAIR, NIL, CLOSURE, ENV };

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

static Value MakePrimitive(Lambda prim) {
    Value v ;
    v.clo.t = CLOSURE ;
    v.clo.lam = prim ;
    v.clo.env = NULL ;
    return v ;
}
// =============== BUILTINS ===============

static void runtime_error(const char* message) {
    fprintf(stderr, "Runtime error: %s\n", message);
    exit(1);
}

static Value car(Value list) {
    if (list.t != PAIR) {
        runtime_error("car expects a pair");
    }
    return *(list.pair.fst);
}

static Value cdr(Value list) {
    if (list.t != PAIR) {
        runtime_error("cdr expects a pair");
    }
    return *(list.pair.snd);
}

static Value cons(Value fst, Value snd) {
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

static Value is_cons(Value v) {
    return MakeBool(v.t == PAIR);
}

static Value is_null(Value v) {
    return MakeBool(v.t == NIL);
}

static Value is_bool(Value v) {
    return MakeBool(v.t == BOOL);
}

static Value is_int(Value v) {
    return MakeBool(v.t == INT);
}

static Value is_lambda(Value v) {
    return MakeBool(v.t == LAMBDA);
}

static Value is_list(Value v) {
    while (v.t == PAIR) {
        v = *(v.pair.snd);
    }
    return MakeBool(v.t == NIL);
}

static Value eq(Value a, Value b) {
    if (a.t != b.t) return MakeBool(0);
    switch (a.t) {
        case INT: return MakeBool(a.z.value == b.z.value);
        case BOOL: return MakeBool(a.b.value == b.b.value);
        default: runtime_error("Can only compare ints and bools");
    }
}

static Value list_ref(Value list, Value idx) {
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

static Value Add(Value a, Value b) {
    ensure_int_pair(a, b, "+ expects two integers");
    return MakeInt(a.z.value + b.z.value);
}

static Value Sub(Value a, Value b) {
    ensure_int_pair(a, b, "- expects two integers");
    return MakeInt(a.z.value - b.z.value);
}

static Value Mul(Value a, Value b) {
    ensure_int_pair(a, b, "* expects two integers");
    return MakeInt(a.z.value * b.z.value);
}

static Value Div(Value a, Value b) {
    ensure_int_pair(a, b, "/ expects two integers");
    if (b.z.value == 0) {
        runtime_error("division by zero");
    }
    return MakeInt(a.z.value / b.z.value);
}

static Value Lt(Value a, Value b) {
    ensure_int_pair(a, b, "< expects two integers");
    return MakeBool(a.z.value < b.z.value);
}

static Value Gt(Value a, Value b) {
    ensure_int_pair(a, b, "> expects two integers");
    return MakeBool(a.z.value > b.z.value);
}

static Value Le(Value a, Value b) {
    ensure_int_pair(a, b, "<= expects two integers");
    return MakeBool(a.z.value <= b.z.value);
}

static Value Ge(Value a, Value b) {
    ensure_int_pair(a, b, ">= expects two integers");
    return MakeBool(a.z.value >= b.z.value);
}
|}