#include "ksu_runtime.h"

// ============ FORWARD DECLARATIONS ============
Value* deep_copy(Value* v);

// ============ CONSTRUCTORS ============
Value* MakeInt(int x) {
    Value* ptr = malloc(sizeof(Value));
    ptr->integer.t = NUMBER;
    ptr->integer.value = x;
    return ptr;
}

Value* MakeBool(bool x) {
    Value* ptr = malloc(sizeof(Value));
    ptr->boolean.t = BOOLEAN;
    ptr->boolean.value = x;
    return ptr;
}

Value* MakeString(const char* x) {
    Value* ptr = malloc(sizeof(Value));
    ptr->string.t = STRING;
    ptr->string.value = (char*)x;
    return ptr;
}

Value* MakeNil(void) {
    Value* ptr = malloc(sizeof(Value));
    ptr->nil.t = NIL;
    return ptr;
}

Value* MakePair(Value* l, Value* r) {
    Value* ptr = malloc(sizeof(Value));
    ptr->pair.t = PAIR;
    ptr->pair.first = l;
    ptr->pair.second = r;
    return ptr;
}

Value* MakeClosure(Lambda_t f, ClosureEnv e) {
    Value* ptr = malloc(sizeof(Value));
    ptr->closure.t = CLOSURE;
    ptr->closure.lam = f;
    ptr->closure.env = e;
    return ptr;
}

Value* MakeBox(Value* v) {
    Value* ptr = malloc(sizeof(Value));
    ptr->box.t = BOX;
    ptr->box.ptr = deep_copy(v);
    return ptr;
}

// ============ DEEP COPY ============
Value* deep_copy(Value* v) {
    if (v == NULL) return NULL;
    switch (v->t) {
        case NUMBER:
            return MakeInt(v->integer.value);
        case BOOLEAN:
            return MakeBool(v->boolean.value);
        case STRING:
            return MakeString(strdup(v->string.value));
        case NIL:
            return MakeNil();
        case PAIR:
            return MakePair(deep_copy(v->pair.first), deep_copy(v->pair.second));
        case CLOSURE:
            return MakeClosure(v->closure.lam, v->closure.env);
        case BOX:
            runtime_error("please don't create box over box");
            return NULL;
        default:
            runtime_error("unknown type in deep_copy");
            return NULL;
    }
}

// ============ PAIR OPERATIONS ============
static Value* __builtin_fst(Value* v) {
    if (v->t != PAIR) runtime_error("fst expects a pair");
    if (v->pair.first == NULL) runtime_error("first element of pair is null");
    return v->pair.first;
}

static Value* __builtin_snd(Value* v) {
    if (v->t != PAIR) runtime_error("snd expects a pair");
    if (v->pair.second == NULL) runtime_error("second element of pair is null");
    return v->pair.second;
}

static Value* __builtin_pair(Value* l, Value* r) {
    return MakePair(l, r);
}

// ============ TYPE PREDICATES ============
static Value* __builtin_is_pair(Value* v) {
    return MakeBool(v->t == PAIR);
}

static Value* __builtin_is_nil(Value* v) {
    return MakeBool(v->t == NIL);
}

static Value* __builtin_is_bool(Value* v) {
    return MakeBool(v->t == BOOLEAN);
}

static Value* __builtin_is_number(Value* v) {
    return MakeBool(v->t == NUMBER);
}

static Value* __builtin_is_list(Value* v) {
    // A list is either nil or a pair whose second element is a list
    Value* cur = v;
    while (cur->t == PAIR) {
        cur = cur->pair.second;
    }
    return MakeBool(cur->t == NIL);
}

// ============ COMPARISON ============
static Value* __builtin_eq(Value* a, Value* b) {
    if (a->t != b->t) return MakeBool(false);
    switch (a->t) {
        case NUMBER:
            return MakeBool(a->integer.value == b->integer.value);
        case BOOLEAN:
            return MakeBool(a->boolean.value == b->boolean.value);
        default:
            runtime_error("eq: can only compare ints and bools");
            return NULL;
    }
}

static Value* __builtin_ne(Value* a, Value* b) {
    if (a->t != b->t) return MakeBool(true);
    switch (a->t) {
        case NUMBER:
            return MakeBool(a->integer.value != b->integer.value);
        case BOOLEAN:
            return MakeBool(a->boolean.value != b->boolean.value);
        default:
            runtime_error("ne: can only compare ints and bools");
            return NULL;
    }
}

// ============ ARITHMETIC ============
static void ensure_int_pair(Value* a, Value* b, const char* op) {
    if (a->t != NUMBER || b->t != NUMBER) {
        runtime_error(op);
    }
}

static Value* __builtin_add(Value* a, Value* b) {
    ensure_int_pair(a, b, "+ expects two integers");
    return MakeInt(a->integer.value + b->integer.value);
}

static Value* __builtin_sub(Value* a, Value* b) {
    ensure_int_pair(a, b, "- expects two integers");
    return MakeInt(a->integer.value - b->integer.value);
}

static Value* __builtin_mul(Value* a, Value* b) {
    ensure_int_pair(a, b, "* expects two integers");
    return MakeInt(a->integer.value * b->integer.value);
}

static Value* __builtin_div(Value* a, Value* b) {
    ensure_int_pair(a, b, "/ expects two integers");
    if (b->integer.value == 0) {
        runtime_error("division by zero");
    }
    return MakeInt(a->integer.value / b->integer.value);
}

static Value* __builtin_lt(Value* a, Value* b) {
    ensure_int_pair(a, b, "< expects two integers");
    return MakeBool(a->integer.value < b->integer.value);
}

static Value* __builtin_gt(Value* a, Value* b) {
    ensure_int_pair(a, b, "> expects two integers");
    return MakeBool(a->integer.value > b->integer.value);
}

static Value* __builtin_le(Value* a, Value* b) {
    ensure_int_pair(a, b, "<= expects two integers");
    return MakeBool(a->integer.value <= b->integer.value);
}

static Value* __builtin_ge(Value* a, Value* b) {
    ensure_int_pair(a, b, ">= expects two integers");
    return MakeBool(a->integer.value >= b->integer.value);
}

// ============ BOOLEAN OPERATIONS ============
static Value* __builtin_and(Value* a, Value* b) {
    if (a->t != BOOLEAN || b->t != BOOLEAN) {
        runtime_error("and expects two booleans");
    }
    return MakeBool(a->boolean.value && b->boolean.value);
}

static Value* __builtin_or(Value* a, Value* b) {
    if (a->t != BOOLEAN || b->t != BOOLEAN) {
        runtime_error("or expects two booleans");
    }
    return MakeBool(a->boolean.value || b->boolean.value);
}

static Value* __builtin_not(Value* a) {
    if (a->t != BOOLEAN) {
        runtime_error("not expects a boolean");
    }
    return MakeBool(!a->boolean.value);
}

// ============ I/O ============
static Value* __builtin_print(Value* a) {
    switch (a->t) {
        case NUMBER:
            printf("%d\n", a->integer.value);
            break;
        case BOOLEAN:
            printf("%s\n", a->boolean.value ? "#t" : "#f");
            break;
        case STRING:
            printf("%s\n", a->string.value);
            break;
        case NIL:
            printf("nil\n");
            break;
        case PAIR:
            printf("<pair>\n");
            break;
        case CLOSURE:
            printf("<closure>\n");
            break;
        case BOX:
            printf("<box>\n");
            break;
        default:
            runtime_error("print: unknown type");
    }
    return a;
}

// ============ BOX OPERATIONS ============
static Value* __builtin_box(Value* v) {
    return MakeBox(v);
}

static Value* __builtin_set(Value* box, Value* value) {
    if (box->t != BOX) runtime_error("set! expects a box");
    box->box.ptr = deep_copy(value);
    return box;
}

static Value* __builtin_unwrap(Value* box) {
    if (box->t != BOX) runtime_error("unwrap expects a box");
    return deep_copy(box->box.ptr);
}

static Value* __builtin_peek(Value* box) {
    if (box->t != BOX) runtime_error("peek expects a box");
    return box->box.ptr;
}
