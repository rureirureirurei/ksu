let header = {|
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

static void runtime_error(const char* message);

enum Tag { INT, BOOL, STRING, PAIR, NIL, CLOSURE, CELL };


struct EnvEntry;

union Value;
typedef union Value Value;

// Lambda in C-land takes environment, number of arguments and arguments as array of Value's
typedef union Value (*Lambda)(struct EnvEntry*, int, union Value*);

struct Closure {
    enum Tag t;
    Lambda lam;
    struct EnvEntry* env;
};

struct Int {
    enum Tag t;
    int value;
};

struct Bool {
    enum Tag t;
    int value;
};

struct String {
    enum Tag t;
    const char* str;
};

struct Pair {
    enum Tag t;
    union Value* fst;
    union Value* snd;
};
typedef struct Pair Pair;

struct Cell {
    enum Tag t;
    union Value* addr;
};

union Value {
    enum Tag t;
    struct Int z;
    struct Bool b;
    struct String s;
    struct Closure clo;
    struct Pair pair;
    struct Cell cell;
};

static const char* tag_to_string(enum Tag t) {
    switch (t) {
        case INT: return "INT";
        case BOOL: return "BOOL";
        case STRING: return "STRING";
        case PAIR: return "PAIR";
        case NIL: return "NIL";
        case CLOSURE: return "CLOSURE";
        case CELL: return "CELL";
        default: return "UNKNOWN";
    }
}

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

static Value MakeString(const char* s) {
    Value v;
    v.t = STRING;
    v.s.str = s;
    return v;
}

static Value MakeNil() {
    Value v;
    v.t = NIL;
    return v;
}

static Value NewCell(Value initialValue) {
    Value v ;
    v.cell.t = CELL ;
    v.cell.addr = malloc(sizeof(Value)) ;
    *v.cell.addr = initialValue ;
    return v ;
}

static Value NewEmptyCell() {
    Value v;
    v.cell.t = CELL;
    v.cell.addr = malloc(sizeof(Value));
    if (!v.cell.addr) {
        runtime_error("Out of memory allocating Cell");
    }
    *v.cell.addr = MakeNil();
    return v;
}

static Value CellValue(Value cell) {
    if (cell.t != CELL) {
        return cell;
    }
    if (!cell.cell.addr) {
        runtime_error("Expected non NULL address in CellValue");
    }
    if ((cell.cell.addr)->t == CELL) {
        runtime_error("Are you sure we have Cell in Cell? Seems buggy");
    }
    return *(cell.cell.addr);
}

static Value SetCell(Value cell, Value value) {
    if (cell.t != CELL) {
        char msg[100];
        sprintf(msg, "SetCell expects a CELL but got %s", tag_to_string(cell.t));
        runtime_error(msg);
    }
    if (!cell.cell.addr) {
        runtime_error("SetCell on NULL address");
    }
    *(cell.cell.addr) = value;
    return cell;
}

// =============== CLOSURES ===============

struct EnvEntry {
    const char* name;
    Value value; // Implicitly, we expect all of those to be Cells
    struct EnvEntry* next;
};
typedef struct EnvEntry EnvEntry;

static EnvEntry* MakeEnv(int pair_count, ...) {
    va_list args;
    va_start(args, pair_count);

    EnvEntry* head = NULL;
    for (int i = 0; i < pair_count; i++) {
        const char* key = va_arg(args, const char*);
        Value value = va_arg(args, Value);

        EnvEntry* node = (EnvEntry*)malloc(sizeof(EnvEntry));
        if (!node) {
            runtime_error("Out of memory allocating EnvEntry");
        }
        node->name = key;
        node->value = value;
        node->next = head;
        head = node;
    }

    va_end(args);
    return head;
}

static Value EnvRef(EnvEntry* node, const char* key) {
    while (node) { if (strcmp(node->name, key) == 0) return node->value; node= node->next; }
    runtime_error("Unbound variable");
}

static Value MakeClosure(Lambda lam, EnvEntry* env) {
    Value v ;
    v.clo.t = CLOSURE ;
    v.clo.lam = lam ;
    v.clo.env = env ;
    return v ;
}

// =============== BUILTINS ===============

static void runtime_error(const char* message) {
    fprintf(stderr, "Runtime error: %s\n", message);
    exit(1);
}


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
    Value *sndptr = malloc(sizeof(Value));
    *sndptr = snd;
    Value *fstptr = malloc(sizeof(Value));
    *fstptr = fst;


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

static Value __builtin_print(Value a) {
    switch (a.t) {
        case INT:
            printf("%d\n", a.z.value);
            break;
        case BOOL:
            printf("%s\n", a.b.value ? "#t" : "#f");
            break;
        case STRING:
            printf("%s\n", a.s.str);
            break;
        default:
            runtime_error("print expects int, bool, or string");
    }
    return a;
}

static Value __builtin_ne(Value a, Value b) {
    if (a.t != b.t) return MakeBool(1);
    switch (a.t) {
        case INT: return MakeBool(a.z.value != b.z.value);
        case BOOL: return MakeBool(a.b.value != b.b.value);
        default: runtime_error("Can only compare ints and bools");
    }
}

static Value __builtin_set(Value cell, Value value) {
    return SetCell(cell, CellValue(value));
}
|}