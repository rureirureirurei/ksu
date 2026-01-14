#include "ksu_runtime.h"

// ============ FORWARD DECLARATIONS ============
Value* deep_copy(Value* v);

static const char* type_to_string(ValueTag t) {
    switch (t) {
        case NUMBER: return "NUMBER";
        case BOOLEAN: return "BOOLEAN";
        case STRING: return "STRING";
        case NIL: return "NIL";
        case PAIR: return "PAIR";
        case CLOSURE: return "CLOSURE";
        case BOX: return "BOX";
        case SYMBOL: return "SYMBOL";
        default: return "UNKNOWN";
    }
}

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
    if (f == NULL) {
        fprintf(stderr, "MakeClosure: NULL lambda pointer\n");
        runtime_error("Cannot create closure with NULL lambda");
    }
    Value* ptr = malloc(sizeof(Value));
    if (ptr == NULL) {
        runtime_error("MakeClosure: malloc failed");
    }
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

Value* MakeSymbol(const char* name) {
    Value* ptr = malloc(sizeof(Value));
    ptr->symbol.t = SYMBOL;
    ptr->symbol.name = (char*)name;
    return ptr;
}

static Thunk __id_impl(ClosureEnv env, int argc, Value** argv) {
    if (argc != 1) runtime_error("id expects 1 argument");
    return MakeComputedThunk(argv[0]);
}

Value* id;

Thunk ApplyClosure(Value* f, int argc, Value** argv) {
    if (f == NULL) {
        fprintf(stderr, "ApplyClosure: NULL function pointer\n");
        runtime_error("ApplyClosure called with NULL");
    }
    if (f->t != CLOSURE) {
        fprintf(stderr, "ApplyClosure: expected CLOSURE, got %s\n", type_to_string(f->t));
        runtime_error("ApplyClosure expects a closure");
    }
    if (f->closure.lam == NULL) {
        fprintf(stderr, "ApplyClosure: closure has NULL lambda pointer\n");
        runtime_error("ApplyClosure: NULL lambda in closure");
    }
    return MakeThunk(f->closure.lam, f->closure.env, argc, argv);
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
        case SYMBOL:
            return MakeSymbol(strdup(v->symbol.name));
        default:
            runtime_error("unknown type in deep_copy");
            return NULL;
    }
}

// ============ PAIR OPERATIONS ============
static Thunk __builtin_fst(Value* v, Value* k) {
    if (v == NULL) {
        fprintf(stderr, "fst: NULL argument\n");
        runtime_error("fst expects a pair");
    }
    if (v->t != PAIR) {
        fprintf(stderr, "fst: expected PAIR, got type %d\n", v->t);
        runtime_error("fst expects a pair");
    }
    if (v->pair.first == NULL) {
        fprintf(stderr, "fst: first element is NULL\n");
        runtime_error("first element of pair is null");
    }
    return ApplyClosure(k, 1, (Value*[]){ v->pair.first });
}

static Thunk __builtin_snd(Value* v, Value* k) {
    if (v == NULL) {
        fprintf(stderr, "snd: NULL argument\n");
        runtime_error("snd expects a pair");
    }
    if (v->t != PAIR) {
        fprintf(stderr, "snd: expected PAIR, got type %d\n", v->t);
        runtime_error("snd expects a pair");
    }
    if (v->pair.second == NULL) {
        fprintf(stderr, "snd: second element is NULL\n");
        runtime_error("second element of pair is null");
    }
    return ApplyClosure(k, 1, (Value*[]){ v->pair.second });
}

static Thunk __builtin_pair(Value* l, Value* r, Value* k) {
    if (l == NULL || r == NULL) {
        fprintf(stderr, "pair: NULL argument (l=%p, r=%p)\n", (void*)l, (void*)r);
        runtime_error("pair expects non-NULL arguments");
    }
    return ApplyClosure(k, 1, (Value*[]){ MakePair(l, r) });
}

Value* nil;

// ============ TYPE PREDICATES ============
static Thunk __builtin_is_pair(Value* v, Value* k) {
    if (v == NULL) {
        fprintf(stderr, "pair?: NULL argument\n");
        runtime_error("pair? expects a value");
    }
    return ApplyClosure(k, 1, (Value*[]){ MakeBool(v->t == PAIR) });
}

static Thunk __builtin_is_nil(Value* v, Value* k) {
    if (v == NULL) {
        fprintf(stderr, "nil?: NULL argument\n");
        runtime_error("nil? expects a value");
    }
    return ApplyClosure(k, 1, (Value*[]){ MakeBool(v->t == NIL) });
}

static Thunk __builtin_is_bool(Value* v, Value* k) {
    if (v == NULL) {
        fprintf(stderr, "bool?: NULL argument\n");
        runtime_error("bool? expects a value");
    }
    return ApplyClosure(k, 1, (Value*[]){ MakeBool(v->t == BOOLEAN) });
}

static Thunk __builtin_is_number(Value* v, Value* k) {
    if (v == NULL) {
        fprintf(stderr, "number?: NULL argument\n");
        runtime_error("number? expects a value");
    }
    return ApplyClosure(k, 1, (Value*[]){ MakeBool(v->t == NUMBER) });
}

static Thunk __builtin_is_list(Value* v, Value* k) {
    if (v == NULL) {
        fprintf(stderr, "list?: NULL argument\n");
        runtime_error("list? expects a value");
    }
    // A list is either nil or a pair whose second element is a list
    Value* cur = v;
    while (cur->t == PAIR) {
        if (cur->pair.second == NULL) {
            fprintf(stderr, "list?: encountered NULL in pair chain\n");
            runtime_error("malformed list");
        }
        cur = cur->pair.second;
    }
    return ApplyClosure(k, 1, (Value*[]){ MakeBool(cur->t == NIL) });
}

// ============ COMPARISON ============
static Thunk __builtin_eq(Value* a, Value* b, Value* k) {
    if (a == NULL || b == NULL) {
        fprintf(stderr, "eq: NULL argument (a=%p, b=%p)\n", (void*)a, (void*)b);
        runtime_error("eq expects non-NULL arguments");
    }
    if (a->t != b->t) return ApplyClosure(k, 1, (Value*[]){ MakeBool(false) });
    switch (a->t) {
        case NUMBER:
            return ApplyClosure(k, 1, (Value*[]){ MakeBool(a->integer.value == b->integer.value) });
        case BOOLEAN:
            return ApplyClosure(k, 1, (Value*[]){ MakeBool(a->boolean.value == b->boolean.value) });
        case STRING:
            return ApplyClosure(k, 1, (Value*[]){ MakeBool(strcmp(a->string.value, b->string.value) == 0) });
        case SYMBOL:
            return ApplyClosure(k, 1, (Value*[]){ MakeBool(strcmp(a->symbol.name, b->symbol.name) == 0) });
        default:
            fprintf(stderr, "eq: can only compare ints and bools; got %s and %s\n",
                    type_to_string(a->t), type_to_string(b->t));
            runtime_error("eq: unsupported types");
            return MakeComputedThunk(NULL);
    }
}

static Thunk __builtin_ne(Value* a, Value* b, Value* k) {
    if (a == NULL || b == NULL) {
        fprintf(stderr, "ne: NULL argument (a=%p, b=%p)\n", (void*)a, (void*)b);
        runtime_error("ne expects non-NULL arguments");
    }
    if (a->t != b->t) return ApplyClosure(k, 1, (Value*[]){ MakeBool(true) });
    switch (a->t) {
        case NUMBER:
            return ApplyClosure(k, 1, (Value*[]){ MakeBool(a->integer.value != b->integer.value) });
        case BOOLEAN:
            return ApplyClosure(k, 1, (Value*[]){ MakeBool(a->boolean.value != b->boolean.value) });
        default:
            fprintf(stderr, "ne: can only compare ints and bools; got %s and %s\n",
                    type_to_string(a->t), type_to_string(b->t));
            runtime_error("ne: unsupported types");
            return MakeComputedThunk(NULL);
    }
}

// ============ ARITHMETIC ============
static void ensure_int_pair(Value* a, Value* b, const char* op) {
    if (a == NULL || b == NULL) {
        fprintf(stderr, "%s: NULL argument (a=%p, b=%p)\n", op, (void*)a, (void*)b);
        runtime_error(op);
    }
    if (a->t != NUMBER || b->t != NUMBER) {
        fprintf(stderr, "%s; got %s and %s\n", op, type_to_string(a->t), type_to_string(b->t));
        runtime_error(op);
    }
}

static Thunk __builtin_add(Value* a, Value* b, Value* k) {
    ensure_int_pair(a, b, "+ expects two integers");
    return ApplyClosure(k, 1, (Value*[]){ MakeInt(a->integer.value + b->integer.value) });
}

static Thunk __builtin_sub(Value* a, Value* b, Value* k) {
    ensure_int_pair(a, b, "- expects two integers");
    return ApplyClosure(k, 1, (Value*[]){ MakeInt(a->integer.value - b->integer.value) });
}

static Thunk __builtin_mul(Value* a, Value* b, Value* k) {
    ensure_int_pair(a, b, "* expects two integers");
    return ApplyClosure(k, 1, (Value*[]){ MakeInt(a->integer.value * b->integer.value) });
}

static Thunk __builtin_div(Value* a, Value* b, Value* k) {
    ensure_int_pair(a, b, "/ expects two integers");
    if (b->integer.value == 0) {
        fprintf(stderr, "division by zero: %d / 0\n", a->integer.value);
        runtime_error("division by zero");
    }
    return ApplyClosure(k, 1, (Value*[]){ MakeInt(a->integer.value / b->integer.value) });
}

static Thunk __builtin_lt(Value* a, Value* b, Value* k) {
    ensure_int_pair(a, b, "< expects two integers");
    return ApplyClosure(k, 1, (Value*[]){ MakeBool(a->integer.value < b->integer.value) });
}

static Thunk __builtin_gt(Value* a, Value* b, Value* k) {
    ensure_int_pair(a, b, "> expects two integers");
    return ApplyClosure(k, 1, (Value*[]){ MakeBool(a->integer.value > b->integer.value) });
}

static Thunk __builtin_le(Value* a, Value* b, Value* k) {
    ensure_int_pair(a, b, "<= expects two integers");
    return ApplyClosure(k, 1, (Value*[]){ MakeBool(a->integer.value <= b->integer.value) });
}

static Thunk __builtin_ge(Value* a, Value* b, Value* k) {
    ensure_int_pair(a, b, ">= expects two integers");
    return ApplyClosure(k, 1, (Value*[]){ MakeBool(a->integer.value >= b->integer.value) });
}

// ============ BOOLEAN OPERATIONS ============
static Thunk __builtin_and(Value* a, Value* b, Value* k) {
    if (a == NULL || b == NULL) {
        fprintf(stderr, "and: NULL argument (a=%p, b=%p)\n", (void*)a, (void*)b);
        runtime_error("and expects non-NULL arguments");
    }
    if (a->t != BOOLEAN || b->t != BOOLEAN) {
        fprintf(stderr, "and: expects booleans; got %s and %s\n",
                type_to_string(a->t), type_to_string(b->t));
        runtime_error("and expects two booleans");
    }
    return ApplyClosure(k, 1, (Value*[]){ MakeBool(a->boolean.value && b->boolean.value) });
}

static Thunk __builtin_or(Value* a, Value* b, Value* k) {
    if (a == NULL || b == NULL) {
        fprintf(stderr, "or: NULL argument (a=%p, b=%p)\n", (void*)a, (void*)b);
        runtime_error("or expects non-NULL arguments");
    }
    if (a->t != BOOLEAN || b->t != BOOLEAN) {
        fprintf(stderr, "or: expects booleans; got %s and %s\n",
                type_to_string(a->t), type_to_string(b->t));
        runtime_error("or expects two booleans");
    }
    return ApplyClosure(k, 1, (Value*[]){ MakeBool(a->boolean.value || b->boolean.value) });
}

static Thunk __builtin_not(Value* a, Value* k) {
    if (a == NULL) {
        fprintf(stderr, "not: NULL argument\n");
        runtime_error("not expects a value");
    }
    if (a->t != BOOLEAN) {
        fprintf(stderr, "not: expects boolean; got %s\n", type_to_string(a->t));
        runtime_error("not expects a boolean");
    }
    return ApplyClosure(k, 1, (Value*[]){ MakeBool(!a->boolean.value) });
}

// ============ I/O ============
static void print_value(Value* v) {
    if (v == NULL) {
        printf("NULL");
        return;
    }
    switch (v->t) {
        case NUMBER:
            printf("%d", v->integer.value);
            break;
        case BOOLEAN:
            printf("%s", v->boolean.value ? "#t" : "#f");
            break;
        case STRING:
            printf("\"%s\"", v->string.value);
            break;
        case NIL:
            printf("nil");
            break;
        case PAIR:
            print_value(v->pair.first);
            printf(" . ");
            print_value(v->pair.second);
            break;
        case CLOSURE:
            printf("<closure>");
            break;
        case BOX:
            printf("(box ");
            print_value(v->box.ptr);
            printf(")");
            break;
        case SYMBOL:
            printf("'%s", v->symbol.name);
            break;
        default:
            printf("<unknown-type-%d>", v->t);
    }
}

static Thunk __builtin_print(Value* a, Value* k) {
    print_value(a);
    printf("\n");
    return ApplyClosure(k, 1, (Value*[]){ MakeNil() });
}

// ============ BOX OPERATIONS ============
static Thunk __builtin_box(Value* v, Value* k) {
    if (v == NULL) {
        fprintf(stderr, "box: NULL argument\n");
        runtime_error("box expects a value");
    }
    return ApplyClosure(k, 1, (Value*[]){ MakeBox(v) });
}

static Thunk __builtin_set(Value* box, Value* value, Value* k) {
    if (box == NULL || value == NULL) {
        fprintf(stderr, "set!: NULL argument (box=%p, value=%p)\n", (void*)box, (void*)value);
        runtime_error("set! expects non-NULL arguments");
    }
    if (box->t != BOX) {
        fprintf(stderr, "set!: expects box; got %s\n", type_to_string(box->t));
        runtime_error("set! expects a box");
    }
    box->box.ptr = deep_copy(value);
    return ApplyClosure(k, 1, (Value*[]){ MakeNil() });
}

static Thunk __builtin_unwrap(Value* box, Value* k) {
    if (box == NULL) {
        fprintf(stderr, "unwrap: NULL argument\n");
        runtime_error("unwrap expects a box");
    }
    if (box->t != BOX) {
        fprintf(stderr, "unwrap: expects box; got %s\n", type_to_string(box->t));
        runtime_error("unwrap expects a box");
    }
    if (box->box.ptr == NULL) {
        fprintf(stderr, "unwrap: box contains NULL pointer\n");
        runtime_error("unwrap: box is empty");
    }
    return ApplyClosure(k, 1, (Value*[]){ deep_copy(box->box.ptr) });
}

static Thunk __builtin_peek(Value* box, Value* k) {
    if (box == NULL) {
        fprintf(stderr, "peek: NULL argument\n");
        runtime_error("peek expects a box");
    }
    if (box->t != BOX) {
        fprintf(stderr, "peek: expects box; got %s\n", type_to_string(box->t));
        runtime_error("peek expects a box");
    }
    if (box->box.ptr == NULL) {
        fprintf(stderr, "peek: box contains NULL pointer\n");
        runtime_error("peek: box is empty");
    }
    return ApplyClosure(k, 1, (Value*[]){ box->box.ptr });
}

static Thunk __builtin_string_to_symbol(Value* v, Value* k) {
    if (v == NULL) {
        fprintf(stderr, "string->symbol: NULL argument\n");
        runtime_error("string->symbol expects a string");
    }
    if (v->t != STRING) {
        fprintf(stderr, "string->symbol: expects string; got %s\n", type_to_string(v->t));
        runtime_error("string->symbol expects a string");
    }
    return ApplyClosure(k, 1, (Value*[]){ MakeSymbol(v->string.value) });
}

static Thunk __builtin_is_symbol(Value* v, Value* k) {
    if (v == NULL) {
        fprintf(stderr, "symbol?: NULL argument\n");
        runtime_error("symbol? expects a value");
    }
    return ApplyClosure(k, 1, (Value*[]){ MakeBool(v->t == SYMBOL) });
}

static Thunk __builtin_raise(Value* v, Value* k) {
    (void)k;
    fprintf(stderr, "Error: ");
    if (v == NULL) {
        fprintf(stderr, "(null)\n");
    } else if (v->t == STRING) {
        fprintf(stderr, "%s\n", v->string.value);
    } else if (v->t == SYMBOL) {
        fprintf(stderr, "%s\n", v->symbol.name);
    } else {
        fprintf(stderr, "<value of type %s>\n", type_to_string(v->t));
    }
    exit(1);
    return MakeComputedThunk(NULL);
}
