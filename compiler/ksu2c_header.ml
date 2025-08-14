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
|}