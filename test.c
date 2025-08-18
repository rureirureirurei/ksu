
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

static Value MakeClosure(Lambda lam, Value env) {
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


struct Env_20 {

};

Value alloc_Env_20() {
  return MakeEnv(NULL);
}

struct Env_19 {

};

Value alloc_Env_19() {
  return MakeEnv(NULL);
}

struct Env_18 {

};

Value alloc_Env_18() {
  return MakeEnv(NULL);
}

struct Env_17 {

};

Value alloc_Env_17() {
  return MakeEnv(NULL);
}

struct Env_16 {

};

Value alloc_Env_16() {
  return MakeEnv(NULL);
}

struct Env_15 {

};

Value alloc_Env_15() {
  return MakeEnv(NULL);
}

struct Env_14 {

};

Value alloc_Env_14() {
  return MakeEnv(NULL);
}

struct Env_13 {

};

Value alloc_Env_13() {
  return MakeEnv(NULL);
}

struct Env_12 {

};

Value alloc_Env_12() {
  return MakeEnv(NULL);
}

struct Env_11 {

};

Value alloc_Env_11() {
  return MakeEnv(NULL);
}

struct Env_10 {

};

Value alloc_Env_10() {
  return MakeEnv(NULL);
}

struct Env_9 {

};

Value alloc_Env_9() {
  return MakeEnv(NULL);
}

struct Env_8 {

};

Value alloc_Env_8() {
  return MakeEnv(NULL);
}

struct Env_7 {

};

Value alloc_Env_7() {
  return MakeEnv(NULL);
}

struct Env_6 {

};

Value alloc_Env_6() {
  return MakeEnv(NULL);
}

struct Env_5 {

};

Value alloc_Env_5() {
  return MakeEnv(NULL);
}

struct Env_4 {

};

Value alloc_Env_4() {
  return MakeEnv(NULL);
}

struct Env_3 {

};

Value alloc_Env_3() {
  return MakeEnv(NULL);
}

struct Env_2 {

};

Value alloc_Env_2() {
  return MakeEnv(NULL);
}

struct Env_1 {

};

Value alloc_Env_1() {
  return MakeEnv(NULL);
}

Value Lambda_1(Value $env, Value a0, Value a1) {
return Add(a0, a1);
}

Value _plus_ = MakeClosure((Lambda)Lambda_1, alloc_Env_1());

Value Lambda_2(Value $env, Value a0, Value a1) {
return Mul(a0, a1);
}

Value _star_ = MakeClosure((Lambda)Lambda_2, alloc_Env_2());

Value Lambda_3(Value $env, Value a0, Value a1) {
return Sub(a0, a1);
}

Value _minus_ = MakeClosure((Lambda)Lambda_3, alloc_Env_3());

Value Lambda_4(Value $env, Value a0, Value a1) {
return Div(a0, a1);
}

Value _slash_ = MakeClosure((Lambda)Lambda_4, alloc_Env_4());

Value Lambda_5(Value $env, Value a0, Value a1) {
return mod(a0, a1);
}

Value mod = MakeClosure((Lambda)Lambda_5, alloc_Env_5());

Value Lambda_6(Value $env, Value a0, Value a1) {
return eq(a0, a1);
}

Value _equal_ = MakeClosure((Lambda)Lambda_6, alloc_Env_6());

Value Lambda_7(Value $env, Value a0, Value a1) {
return ne(a0, a1);
}

Value _bang__equal_ = MakeClosure((Lambda)Lambda_7, alloc_Env_7());

Value Lambda_8(Value $env, Value a0, Value a1) {
return Lt(a0, a1);
}

Value _lt_ = MakeClosure((Lambda)Lambda_8, alloc_Env_8());

Value Lambda_9(Value $env, Value a0, Value a1) {
return Gt(a0, a1);
}

Value _gt_ = MakeClosure((Lambda)Lambda_9, alloc_Env_9());

Value Lambda_10(Value $env, Value a0, Value a1) {
return Le(a0, a1);
}

Value _lt__equal_ = MakeClosure((Lambda)Lambda_10, alloc_Env_10());

Value Lambda_11(Value $env, Value a0, Value a1) {
return Ge(a0, a1);
}

Value _gt__equal_ = MakeClosure((Lambda)Lambda_11, alloc_Env_11());

Value Lambda_12(Value $env, Value a0, Value a1) {
return cons(a0, a1);
}

Value cons = MakeClosure((Lambda)Lambda_12, alloc_Env_12());

Value Lambda_13(Value $env, Value a0) {
return car(a0);
}

Value car = MakeClosure((Lambda)Lambda_13, alloc_Env_13());

Value Lambda_14(Value $env, Value a0) {
return cdr(a0);
}

Value cdr = MakeClosure((Lambda)Lambda_14, alloc_Env_14());

Value nil = MakeNil();

Value Lambda_15(Value $env, Value a0) {
return is_null(a0);
}

Value nil_quest_ = MakeClosure((Lambda)Lambda_15, alloc_Env_15());

Value Lambda_16(Value $env, Value a0) {
return is_cons(a0);
}

Value pair_quest_ = MakeClosure((Lambda)Lambda_16, alloc_Env_16());

Value Lambda_17(Value $env, Value a0) {
return is_int(a0);
}

Value number_quest_ = MakeClosure((Lambda)Lambda_17, alloc_Env_17());

Value Lambda_18(Value $env, Value a0, Value a1) {
return and(a0, a1);
}

Value and = MakeClosure((Lambda)Lambda_18, alloc_Env_18());

Value Lambda_19(Value $env, Value a0, Value a1) {
return or(a0, a1);
}

Value or = MakeClosure((Lambda)Lambda_19, alloc_Env_19());

Value Lambda_20(Value $env, Value a0) {
return not(a0);
}

Value not = MakeClosure((Lambda)Lambda_20, alloc_Env_20());

Value A = (({ Value tmp_2 = eq_quest_; tmp_2.clo.lam(tmp_2.clo.env, MakeInt(10), MakeInt(10)); }) ? _plus_ : _minus_);

int main() {
Value MainExpr_1 = ({ Value tmp_1 = A; tmp_1.clo.lam(tmp_1.clo.env, MakeInt(20), MakeInt(20)); });
}
