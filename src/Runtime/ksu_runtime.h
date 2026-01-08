#ifndef KSU_RUNTIME_H
#define KSU_RUNTIME_H

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

// Forward declarations
union Value;
typedef union Value Value;
struct ListItem;
struct EnvEntry;

// ============ VALUE TAGS ============
typedef enum ValueTag {
    NUMBER,
    STRING,
    CLOSURE,
    BOOLEAN,
    LIST
} ValueTag;

// ============ PRIMITIVE VALUES ============
struct ValueInt {
    ValueTag t;
    int value;
};

struct ValueString {
    ValueTag t;
    char* value;
};

struct ValueBool {
    ValueTag t;
    bool value;
};

// ============ LISTS ============
struct ValueList {
    ValueTag t;
    struct ListItem* ptr;  // NULL = empty list
};

// ============ CLOSURES ============
typedef struct EnvEntry* ClosureEnv;
typedef Value (*Lambda_t)(ClosureEnv, int, Value*);

struct ValueClosure {
    ValueTag t;
    Lambda_t lam;
    ClosureEnv env;
};

// ============ VALUE UNION ============
union Value {
    ValueTag t;
    struct ValueInt integer;
    struct ValueString string;
    struct ValueBool boolean;
    struct ValueList list;
    struct ValueClosure closure;
};

// ============ NOW DEFINE COMPLETE STRUCTS ============
struct ListItem {
    Value v;
    struct ListItem* next;
};
typedef struct ListItem ListItem;

struct EnvEntry {
    char* name;
    Value* val;
};
typedef struct EnvEntry EnvEntry;

// ============ CONSTRUCTORS ============
#define MakeInt(x)        (Value){ .integer = { .t = NUMBER,  .value = (x) } }
#define MakeBool(x)       (Value){ .boolean = { .t = BOOLEAN, .value = (x) } }
#define MakeString(x)     (Value){ .string  = { .t = STRING,  .value = (x) } }
#define MakeEmptyList()   (Value){ .list    = { .t = LIST,    .ptr = NULL } }
#define MakeClosure(f, e) (Value){ .closure = { .t = CLOSURE, .lam = (f), .env = (e) } }

// ============ RUNTIME FUNCTIONS ============
void runtime_error(const char* msg);
ClosureEnv MakeEnv(int count, ...);
Value* EnvRef(ClosureEnv env, const char* id);
bool is_true(Value v);

#endif // KSU_RUNTIME_H
