let header = {|
#include <stdio.h>
#include <stdlib.h>

enum Tag { INT, BOOL, LAMBDA, PAIR };

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
    struct Pair* nextptr;
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

static Value MakePair(Value a, Pair *rest) {
    Value *aprim = malloc(sizeof(Value));
    *aprim = a;
    
    Value v;
    v.t = PAIR;
    v.pair.valueptr = aprim;
    v.pair.nextptr = rest;
    return v;
}

static Value MakeLambda(void* funptr) {
    Value v;
    v.t = LAMBDA;
    v.lam.ptr = funptr;
    return v;
}
|}