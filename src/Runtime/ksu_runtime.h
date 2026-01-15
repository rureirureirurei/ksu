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
struct Thunk;
typedef struct Thunk Thunk;
struct EnvEntry;

// ============ VALUE TAGS ============
typedef enum ValueTag {
    NUMBER,
    STRING,
    CLOSURE,
    BOOLEAN,
    PAIR,
    NIL,
    BOX,
    SYMBOL,
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
typedef Thunk (*Lambda_t)(ClosureEnv, int, Value**);

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

// ============ BOXES ============
struct ValueBox {
    ValueTag t;
    Value* ptr;
};

// ============ SYMBOLS ============
struct ValueSymbol {
    ValueTag t;
    char* name;
};

// ============ VALUE UNION ============
union Value {
    ValueTag t;
    struct ValueInt integer;
    struct ValueString string;
    struct ValueBool boolean;
    struct ValuePair pair;
    struct ValueNil nil;
    struct ValueClosure closure;
    struct ValueBox box;
    struct ValueSymbol symbol;
};

// ============ NOW DEFINE COMPLETE STRUCTS ============
struct EnvEntry {
    char* name;
    Value* val;
};
typedef struct EnvEntry EnvEntry;

// ============ RUNTIME FUNCTIONS ============
void runtime_error(const char* msg);
ClosureEnv MakeEnv(int count, ...);
Value* EnvRef(ClosureEnv env, const char* id);
bool is_true(Value* v);

// ============ TRAMPOLINE ============

struct Thunk {
    Lambda_t func;      // NULL means computed
    ClosureEnv env;     // closure environment
    int argc;           // argument count
    Value** argv;       // arguments for the call
    Value* result;      // final result (only valid when func == NULL)
};

Value* Trampoline(Thunk t);
Thunk DoneThunk(Value* v);
Thunk MakeThunk(
        Lambda_t f, 
        ClosureEnv env,
        int argc,
        Value** argv
    );

#endif // KSU_RUNTIME_H