let header = {|
#include <stdio.h>
#include <stdlib.h>

enum Tag { INT, BOOL, LAMBDA, PAIR, NIL };

union Value;
typedef union Value Value;

struct Int {
    enum Tag t;
    int value;
};

struct Bool {
    enum Tag t;
    int value;
};

struct Lambda {
    enum Tag t;
    void* ptr;
};

struct Pair {
    enum Tag t;
    union Value* valueptr;
    union Value* restptr;
};
typedef struct Pair Pair;

union Value {
    enum Tag t;
    struct Int z;
    struct Bool b;
    struct Lambda lam;
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

static Value MakePair(Value a, Value rest) {
    Value *aprim = malloc(sizeof(Value));
    *aprim = a;
    Value *rprim = malloc(sizeof(Value));
    *rprim = rest;
    
    Value v;
    v.t = PAIR;
    v.pair.valueptr = aprim;
    v.pair.restptr = rprim;
    return v;
}

static Value MakeLambda(void* funptr) {
    Value v;
    v.t = LAMBDA;
    v.lam.ptr = funptr;
    return v;
}

static void runtime_error(const char* message) {
    fprintf(stderr, "Runtime error: %s\n", message);
    exit(1);
}

static Value Car(Value list) {
    if (list.t != PAIR) {
        runtime_error("car expects a pair");
    }
    return *(list.pair.valueptr);
}

static Value Cdr(Value list) {
    if (list.t != PAIR) {
        runtime_error("cdr expects a pair");
    }
    return *(list.pair.restptr);
}

static Value is_cons(Value v) {
    return MakeBool(v.t == PAIR);
}

static Value is_null(Value v) {
    return MakeBool(v.t == NIL);
}

static Value Cond(Value c, Value yt, Value nt) {
    if (c.t != BOOL) {
        runtime_error("if expects a boolean");
    }
    return (c.b.value ? yt : nt);
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
        v = *(v.pair.restptr);
    }
    return MakeBool(v.t == NIL);
}

static Value Eq(Value a, Value b) {
    if (a.t != b.t) return MakeBool(0);
    switch (a.t) {
        case INT: return MakeBool(a.z.value == b.z.value);
        case BOOL: return MakeBool(a.b.value == b.b.value);
        case NIL: return MakeBool(1);
        case LAMBDA: return MakeBool(a.lam.ptr == b.lam.ptr);
        case PAIR: return MakeBool(a.pair.valueptr == b.pair.valueptr && a.pair.restptr == b.pair.restptr);
        default: return MakeBool(0);
    }
}

static Value ListRef(Value list, Value idx) {
    if (idx.t != INT) {
        runtime_error("list-ref expects an integer index");
    }
    int n = idx.z.value;
    Value cur = list;
    while (n > 0) {
        if (cur.t != PAIR) {
            runtime_error("list-ref index out of range or not a list");
        }
        cur = *(cur.pair.restptr);
        n--;
    }
    if (cur.t != PAIR) {
        runtime_error("list-ref expects a pair at the final position");
    }
    return *(cur.pair.valueptr);
}
|}