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
    PAIR,
    NIL,
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

// ============ CLOSURES ============
typedef struct EnvEntry* ClosureEnv;
typedef Value (*Lambda_t)(ClosureEnv, int, Value*);

struct ValueClosure {
    ValueTag t;
    Lambda_t lam;
    ClosureEnv env;
};

// ============ PAIR (pair) ============
// (pair 1 2) would be   MakeInt(1) <- ValuePair -> MakeInt(2)
// (pair 1 (pair 2 3)) would be   MakeInt(1) <- ValuePair -> (MakeInt(2) <- ValuePair -> MakeInt(3))
// List (not pair) (1 2 3) would be 
// MakeInt(1) <- ValuePair -> (MakeInt(2) <- ValuePair -> (MakeInt(3) <- ValuePair -> MakeNil())
struct ValuePair {
    ValueTag t;
    Value* first;  
    Value* second;
};

struct ValueNil {
    ValueTag t;
};

// ============ VALUE UNION ============
union Value {
    ValueTag t;
    struct ValueInt integer;
    struct ValueString string;
    struct ValueBool boolean;
    struct ValuePair pair;
    struct ValuePair nil;
    struct ValueClosure closure;
};

// ============ NOW DEFINE COMPLETE STRUCTS ============
struct EnvEntry {
    char* name;
    Value* val;
};
typedef struct EnvEntry EnvEntry;

// ============ pairTRUCTORS ============
#define MakeInt(x)        (Value){ .integer = { .t = NUMBER,  .value = (x) } }
#define MakeBool(x)       (Value){ .boolean = { .t = BOOLEAN, .value = (x) } }
#define MakeString(x)     (Value){ .string  = { .t = STRING,  .value = (x) } }
#define MakeClosure(f, e) (Value){ .closure = { .t = CLOSURE, .lam = (f),   .env = (e) } }
#define MakePair(l, r)    (Value){ .pair    = { .t = PAIR,    .first = (l), .second = (r) } }
#define MakeNil()         (Value){ .nil     = { .t = NIL } }

// ============ RUNTIME FUNCTIONS ============
void runtime_error(const char* msg);
ClosureEnv MakeEnv(int count, ...);
Value* EnvRef(ClosureEnv env, const char* id);
bool is_true(Value v);

#endif // KSU_RUNTIME_H
