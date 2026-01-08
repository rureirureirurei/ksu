#include "ksu_runtime.h"

// ============ LIST OPERATIONS ============

static Value __builtin_fst(Value v) {
    if (v.t != PAIR) runtime_error("fst expects a pair");
    if (v.pair.first == NULL) runtime_error("first element of pair is ptr null");
    return *(v.pair.first);
}

static Value __builtin_snd(Value v) {
    if (v.t != PAIR) runtime_error("snd expects a pair");
    if (v.pair.second == NULL) runtime_error("second element of pair is ptr null");
    return *(v.pair.second);
}

static Value __builtin_pair(Value l, Value r) {
    Value* l_ptr = (Value*)malloc(sizeof(Value));
    Value* r_ptr = (Value*)malloc(sizeof(Value));
    if (!l_ptr || !r_ptr) runtime_error("failed to allocate pair");
    *l_ptr = l;
    *r_ptr = r;
    return (Value){ .t = PAIR, .pair = { .t = PAIR, .first = l_ptr, .second = r_ptr } };
}

// ============ TYPE PREDICATES ============

static Value __builtin_is_pair(Value v) {
    return MakeBool(v.t == PAIR);
}

static Value __builtin_is_nil(Value v) {
    return MakeBool(v.t == NIL);
}

static Value __builtin_is_bool(Value v) {
    return MakeBool(v.t == BOOLEAN);
}

static Value __builtin_is_number(Value v) {
    return MakeBool(v.t == NUMBER);
}

static Value __builtin_is_list(Value v) {
    // todo check that last in the right chain is NIL
    return MakeBool(v.t == PAIR);
}

// ============ COMPARISON ============

static Value __builtin_eq(Value a, Value b) {
    if (a.t != b.t) return MakeBool(0);
    switch (a.t) {
        case NUMBER: return MakeBool(a.integer.value == b.integer.value);
        case BOOLEAN: return MakeBool(a.boolean.value == b.boolean.value);
        default: runtime_error("Can only compare ints and bools");
    }
}

static Value __builtin_ne(Value a, Value b) {
    if (a.t != b.t) return MakeBool(1);
    switch (a.t) {
        case NUMBER: return MakeBool(a.integer.value != b.integer.value);
        case BOOLEAN: return MakeBool(a.boolean.value != b.boolean.value);
        default: runtime_error("Can only compare ints and bools");
    }
}

static Value __builtin_list_ref(Value lst, Value idx) {
    if (idx.t != NUMBER) runtime_error("list-ref expects integer index");
    int n = idx.integer.value;
    Value cur = lst;
    while (n > 0) {
        if (cur.t != PAIR) runtime_error("list-ref index out of range");
        cur = *(cur.pair.second);
        n--;
    }
    if (cur.t != PAIR) runtime_error("list-ref index out of range");
    return *(cur.pair.first);
}

// ============ ARITHMETIC ============

static void ensure_int_pair(Value a, Value b, const char* op) {
    if (a.t != NUMBER || b.t != NUMBER) {
        runtime_error(op);
    }
}

static Value __builtin_add(Value a, Value b) {
    ensure_int_pair(a, b, "+ expects two integers");
    return MakeInt(a.integer.value + b.integer.value);
}

static Value __builtin_sub(Value a, Value b) {
    ensure_int_pair(a, b, "- expects two integers");
    return MakeInt(a.integer.value - b.integer.value);
}

static Value __builtin_mul(Value a, Value b) {
    ensure_int_pair(a, b, "* expects two integers");
    return MakeInt(a.integer.value * b.integer.value);
}

static Value __builtin_div(Value a, Value b) {
    ensure_int_pair(a, b, "/ expects two integers");
    if (b.integer.value == 0) {
        runtime_error("division by zero");
    }
    return MakeInt(a.integer.value / b.integer.value);
}

static Value __builtin_lt(Value a, Value b) {
    ensure_int_pair(a, b, "< expects two integers");
    return MakeBool(a.integer.value < b.integer.value);
}

static Value __builtin_gt(Value a, Value b) {
    ensure_int_pair(a, b, "> expects two integers");
    return MakeBool(a.integer.value > b.integer.value);
}

static Value __builtin_le(Value a, Value b) {
    ensure_int_pair(a, b, "<= expects two integers");
    return MakeBool(a.integer.value <= b.integer.value);
}

static Value __builtin_ge(Value a, Value b) {
    ensure_int_pair(a, b, ">= expects two integers");
    return MakeBool(a.integer.value >= b.integer.value);
}

// ============ BOOLEAN OPERATIONS ============

static Value __builtin_and(Value a, Value b) {
    if (a.t != BOOLEAN || b.t != BOOLEAN) {
        runtime_error("and expects two booleans");
    }
    return MakeBool(a.boolean.value && b.boolean.value);
}

static Value __builtin_or(Value a, Value b) {
    if (a.t != BOOLEAN || b.t != BOOLEAN) {
        runtime_error("or expects two booleans");
    }
    return MakeBool(a.boolean.value || b.boolean.value);
}

static Value __builtin_not(Value a) {
    if (a.t != BOOLEAN) {
        runtime_error("not expects a boolean");
    }
    return MakeBool(!a.boolean.value);
}

static int __builtin_is_true(Value a) {
    if (a.t != BOOLEAN) {
        runtime_error("is_true expects a boolean");
    }
    return a.boolean.value;
}

// ============ I/O ============

static Value __builtin_print(Value a) {
    switch (a.t) {
        case NUMBER:
            printf("%d\n", a.integer.value);
            break;
        case BOOLEAN:
            printf("%s\n", a.boolean.value ? "#t" : "#f");
            break;
        case STRING:
            printf("%s\n", a.string.value);
            break;
        default:
            runtime_error("print expects int, bool, or string");
    }
    return a;
}

// ============ MUTATION ============
// TODO: Proper cell/mutation support not implemented yet
static Value __builtin_set(Value cell, Value value) {
    (void)cell;
    (void)value;
    runtime_error("set! not yet implemented");
    return MakeInt(0);  // unreachable
}

// ============ BOX OPERATIONS ============
// TODO: Implement proper box/cell semantics
static Value __builtin_box(Value v) {
    (void)v;
    runtime_error("box not yet implemented");
    return MakeInt(0);  // unreachable
}

static Value __builtin_unwrap(Value box) {
    (void)box;
    runtime_error("unwrap not yet implemented");
    return MakeInt(0);  // unreachable
}

static Value __builtin_peek(Value box) {
    (void)box;
    runtime_error("peek not yet implemented");
    return MakeInt(0);  // unreachable
}
