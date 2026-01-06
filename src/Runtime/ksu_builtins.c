#include "ksu_runtime.h"

// ============ LIST OPERATIONS ============

static Value __builtin_car(Value v) {
    if (v.t != LIST) runtime_error("car expects a list");
    if (v.list.ptr == NULL) runtime_error("car of empty list");
    return v.list.ptr->v;
}

static Value __builtin_cdr(Value v) {
    if (v.t != LIST) runtime_error("cdr expects a list");
    if (v.list.ptr == NULL) runtime_error("cdr of empty list");
    return (Value){ .t = LIST, .list.ptr = v.list.ptr->next };
}

static Value __builtin_cons(Value elem, Value lst) {
    if (lst.t != LIST) runtime_error("cons expects list as second arg");
    ListItem *node = malloc(sizeof(ListItem));
    node->v = elem;
    node->next = lst.list.ptr;
    return (Value){ .t = LIST, .list.ptr = node };
}

// ============ TYPE PREDICATES ============

static Value __builtin_is_cons(Value v) {
    return MakeBool(v.t == LIST && v.list.ptr != NULL);
}

static Value __builtin_is_null(Value v) {
    return MakeBool(v.t == LIST && v.list.ptr == NULL);
}

static Value __builtin_is_bool(Value v) {
    return MakeBool(v.t == BOOLEAN);
}

static Value __builtin_is_int(Value v) {
    return MakeBool(v.t == NUMBER);
}

static Value __builtin_is_list(Value v) {
    return MakeBool(v.t == LIST);
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
    if (lst.t != LIST) runtime_error("list-ref expects a list");
    int n = idx.integer.value;
    ListItem* cur = lst.list.ptr;
    while (n > 0 && cur != NULL) {
        cur = cur->next;
        n--;
    }
    if (cur == NULL) runtime_error("list-ref index out of range");
    return cur->v;
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
