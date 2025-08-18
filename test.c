
#include <stdio.h>
#include <stdlib.h>

static void runtime_error(const char* message);

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

static Value __builtin_ne(Value a, Value b) {
    if (a.t != b.t) return MakeBool(1);
    switch (a.t) {
        case INT: return MakeBool(a.z.value != b.z.value);
        case BOOL: return MakeBool(a.b.value != b.b.value);
        default: runtime_error("Can only compare ints and bools");
    }
}


Value factorial;
Value not;
Value or;
Value and;
Value number_quest_;
Value pair_quest_;
Value nil_quest_;
Value nil;
Value cdr;
Value car;
Value cons;
Value _gt__equal_;
Value _lt__equal_;
Value _gt_;
Value _lt_;
Value _bang__equal_;
Value eq_quest_;
Value _equal_;
Value _slash_;
Value _minus_;
Value _star_;
Value _plus_;

struct Env_21 {
  Value _equal_;
  Value _minus_;
  Value _star_;
  Value factorial;
  Value n;
};

Value alloc_Env_21(Value _equal_, Value _minus_, Value _star_, Value factorial, Value n) {
  struct Env_21* t = malloc(sizeof(struct Env_21));
  t->_equal_ = _equal_;
  t->_minus_ = _minus_;
  t->_star_ = _star_;
  t->factorial = factorial;
  t->n = n;
  return MakeEnv(t);
}

struct Env_20 {
  Value a0;
};

Value alloc_Env_20(Value a0) {
  struct Env_20* t = malloc(sizeof(struct Env_20));
  t->a0 = a0;
  return MakeEnv(t);
}

struct Env_19 {
  Value a0;
  Value a1;
};

Value alloc_Env_19(Value a0, Value a1) {
  struct Env_19* t = malloc(sizeof(struct Env_19));
  t->a0 = a0;
  t->a1 = a1;
  return MakeEnv(t);
}

struct Env_18 {
  Value a0;
  Value a1;
};

Value alloc_Env_18(Value a0, Value a1) {
  struct Env_18* t = malloc(sizeof(struct Env_18));
  t->a0 = a0;
  t->a1 = a1;
  return MakeEnv(t);
}

struct Env_17 {
  Value a0;
};

Value alloc_Env_17(Value a0) {
  struct Env_17* t = malloc(sizeof(struct Env_17));
  t->a0 = a0;
  return MakeEnv(t);
}

struct Env_16 {
  Value a0;
};

Value alloc_Env_16(Value a0) {
  struct Env_16* t = malloc(sizeof(struct Env_16));
  t->a0 = a0;
  return MakeEnv(t);
}

struct Env_15 {
  Value a0;
};

Value alloc_Env_15(Value a0) {
  struct Env_15* t = malloc(sizeof(struct Env_15));
  t->a0 = a0;
  return MakeEnv(t);
}

struct Env_14 {
  Value a0;
};

Value alloc_Env_14(Value a0) {
  struct Env_14* t = malloc(sizeof(struct Env_14));
  t->a0 = a0;
  return MakeEnv(t);
}

struct Env_13 {
  Value a0;
};

Value alloc_Env_13(Value a0) {
  struct Env_13* t = malloc(sizeof(struct Env_13));
  t->a0 = a0;
  return MakeEnv(t);
}

struct Env_12 {
  Value a0;
  Value a1;
};

Value alloc_Env_12(Value a0, Value a1) {
  struct Env_12* t = malloc(sizeof(struct Env_12));
  t->a0 = a0;
  t->a1 = a1;
  return MakeEnv(t);
}

struct Env_11 {
  Value a0;
  Value a1;
};

Value alloc_Env_11(Value a0, Value a1) {
  struct Env_11* t = malloc(sizeof(struct Env_11));
  t->a0 = a0;
  t->a1 = a1;
  return MakeEnv(t);
}

struct Env_10 {
  Value a0;
  Value a1;
};

Value alloc_Env_10(Value a0, Value a1) {
  struct Env_10* t = malloc(sizeof(struct Env_10));
  t->a0 = a0;
  t->a1 = a1;
  return MakeEnv(t);
}

struct Env_9 {
  Value a0;
  Value a1;
};

Value alloc_Env_9(Value a0, Value a1) {
  struct Env_9* t = malloc(sizeof(struct Env_9));
  t->a0 = a0;
  t->a1 = a1;
  return MakeEnv(t);
}

struct Env_8 {
  Value a0;
  Value a1;
};

Value alloc_Env_8(Value a0, Value a1) {
  struct Env_8* t = malloc(sizeof(struct Env_8));
  t->a0 = a0;
  t->a1 = a1;
  return MakeEnv(t);
}

struct Env_7 {
  Value a0;
  Value a1;
};

Value alloc_Env_7(Value a0, Value a1) {
  struct Env_7* t = malloc(sizeof(struct Env_7));
  t->a0 = a0;
  t->a1 = a1;
  return MakeEnv(t);
}

struct Env_6 {
  Value a0;
  Value a1;
};

Value alloc_Env_6(Value a0, Value a1) {
  struct Env_6* t = malloc(sizeof(struct Env_6));
  t->a0 = a0;
  t->a1 = a1;
  return MakeEnv(t);
}

struct Env_5 {
  Value a0;
  Value a1;
};

Value alloc_Env_5(Value a0, Value a1) {
  struct Env_5* t = malloc(sizeof(struct Env_5));
  t->a0 = a0;
  t->a1 = a1;
  return MakeEnv(t);
}

struct Env_4 {
  Value a0;
  Value a1;
};

Value alloc_Env_4(Value a0, Value a1) {
  struct Env_4* t = malloc(sizeof(struct Env_4));
  t->a0 = a0;
  t->a1 = a1;
  return MakeEnv(t);
}

struct Env_3 {
  Value a0;
  Value a1;
};

Value alloc_Env_3(Value a0, Value a1) {
  struct Env_3* t = malloc(sizeof(struct Env_3));
  t->a0 = a0;
  t->a1 = a1;
  return MakeEnv(t);
}

struct Env_2 {
  Value a0;
  Value a1;
};

Value alloc_Env_2(Value a0, Value a1) {
  struct Env_2* t = malloc(sizeof(struct Env_2));
  t->a0 = a0;
  t->a1 = a1;
  return MakeEnv(t);
}

struct Env_1 {
  Value a0;
  Value a1;
};

Value alloc_Env_1(Value a0, Value a1) {
  struct Env_1* t = malloc(sizeof(struct Env_1));
  t->a0 = a0;
  t->a1 = a1;
  return MakeEnv(t);
}

Value Lambda_1(Value $env, Value a0, Value a1) {
return __builtin_add(EnvRef($env, a0), EnvRef($env, a1));
}

Value Lambda_2(Value $env, Value a0, Value a1) {
return __builtin_mul(EnvRef($env, a0), EnvRef($env, a1));
}

Value Lambda_3(Value $env, Value a0, Value a1) {
return __builtin_sub(EnvRef($env, a0), EnvRef($env, a1));
}

Value Lambda_4(Value $env, Value a0, Value a1) {
return __builtin_div(EnvRef($env, a0), EnvRef($env, a1));
}

Value Lambda_5(Value $env, Value a0, Value a1) {
return __builtin_eq(EnvRef($env, a0), EnvRef($env, a1));
}

Value Lambda_6(Value $env, Value a0, Value a1) {
return __builtin_eq(EnvRef($env, a0), EnvRef($env, a1));
}

Value Lambda_7(Value $env, Value a0, Value a1) {
return __builtin_ne(EnvRef($env, a0), EnvRef($env, a1));
}

Value Lambda_8(Value $env, Value a0, Value a1) {
return __builtin_lt(EnvRef($env, a0), EnvRef($env, a1));
}

Value Lambda_9(Value $env, Value a0, Value a1) {
return __builtin_gt(EnvRef($env, a0), EnvRef($env, a1));
}

Value Lambda_10(Value $env, Value a0, Value a1) {
return __builtin_le(EnvRef($env, a0), EnvRef($env, a1));
}

Value Lambda_11(Value $env, Value a0, Value a1) {
return __builtin_ge(EnvRef($env, a0), EnvRef($env, a1));
}

Value Lambda_12(Value $env, Value a0, Value a1) {
return __builtin_cons(EnvRef($env, a0), EnvRef($env, a1));
}

Value Lambda_13(Value $env, Value a0) {
return __builtin_car(EnvRef($env, a0));
}

Value Lambda_14(Value $env, Value a0) {
return __builtin_cdr(EnvRef($env, a0));
}

Value Lambda_15(Value $env, Value a0) {
return __builtin_is_null(EnvRef($env, a0));
}

Value Lambda_16(Value $env, Value a0) {
return __builtin_is_cons(EnvRef($env, a0));
}

Value Lambda_17(Value $env, Value a0) {
return __builtin_is_int(EnvRef($env, a0));
}

Value Lambda_18(Value $env, Value a0, Value a1) {
return __builtin_and(EnvRef($env, a0), EnvRef($env, a1));
}

Value Lambda_19(Value $env, Value a0, Value a1) {
return __builtin_or(EnvRef($env, a0), EnvRef($env, a1));
}

Value Lambda_20(Value $env, Value a0) {
return __builtin_not(EnvRef($env, a0));
}

Value Lambda_21(Value $env, Value n) {
return (__builtin_is_true(({ Value tmp_5 = EnvRef($env, _equal_); ((Value (*)(Value, Value, Value))tmp_5.clo.lam)(MakeEnv(tmp_5.clo.env), EnvRef($env, n), MakeInt(0)); })) ? MakeInt(1) : ({ Value tmp_2 = EnvRef($env, _star_); ((Value (*)(Value, Value, Value))tmp_2.clo.lam)(MakeEnv(tmp_2.clo.env), EnvRef($env, n), ({ Value tmp_3 = EnvRef($env, factorial); ((Value (*)(Value, Value))tmp_3.clo.lam)(MakeEnv(tmp_3.clo.env), ({ Value tmp_4 = EnvRef($env, _minus_); ((Value (*)(Value, Value, Value))tmp_4.clo.lam)(MakeEnv(tmp_4.clo.env), EnvRef($env, n), MakeInt(1)); })); })); }));
}

int main() {
_plus_ = MakeClosure((Lambda)Lambda_1, alloc_Env_1(a0,a1));
_star_ = MakeClosure((Lambda)Lambda_2, alloc_Env_2(a0,a1));
_minus_ = MakeClosure((Lambda)Lambda_3, alloc_Env_3(a0,a1));
_slash_ = MakeClosure((Lambda)Lambda_4, alloc_Env_4(a0,a1));
_equal_ = MakeClosure((Lambda)Lambda_5, alloc_Env_5(a0,a1));
eq_quest_ = MakeClosure((Lambda)Lambda_6, alloc_Env_6(a0,a1));
_bang__equal_ = MakeClosure((Lambda)Lambda_7, alloc_Env_7(a0,a1));
_lt_ = MakeClosure((Lambda)Lambda_8, alloc_Env_8(a0,a1));
_gt_ = MakeClosure((Lambda)Lambda_9, alloc_Env_9(a0,a1));
_lt__equal_ = MakeClosure((Lambda)Lambda_10, alloc_Env_10(a0,a1));
_gt__equal_ = MakeClosure((Lambda)Lambda_11, alloc_Env_11(a0,a1));
cons = MakeClosure((Lambda)Lambda_12, alloc_Env_12(a0,a1));
car = MakeClosure((Lambda)Lambda_13, alloc_Env_13(a0));
cdr = MakeClosure((Lambda)Lambda_14, alloc_Env_14(a0));
nil = MakeNil();
nil_quest_ = MakeClosure((Lambda)Lambda_15, alloc_Env_15(a0));
pair_quest_ = MakeClosure((Lambda)Lambda_16, alloc_Env_16(a0));
number_quest_ = MakeClosure((Lambda)Lambda_17, alloc_Env_17(a0));
and = MakeClosure((Lambda)Lambda_18, alloc_Env_18(a0,a1));
or = MakeClosure((Lambda)Lambda_19, alloc_Env_19(a0,a1));
not = MakeClosure((Lambda)Lambda_20, alloc_Env_20(a0));
factorial = MakeClosure((Lambda)Lambda_21, alloc_Env_21(_equal_,_minus_,_star_,factorial,n));
Value MainExpr_1 = ({ Value tmp_1 = factorial; ((Value (*)(Value, Value))tmp_1.clo.lam)(MakeEnv(tmp_1.clo.env), MakeInt(5)); });
}
