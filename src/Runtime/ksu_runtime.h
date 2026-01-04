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
typedef struct ListItem {
    Value v;
    struct ListItem* next;  // NULL = end of list
} ListItem;

struct ValueList {
    ValueTag t;
    ListItem* ptr;  // NULL = empty list
};

// ============ CLOSURES ============
typedef Value** ClosureEnv;
typedef Value (*Lambda_t)(Value*, ClosureEnv);

struct ValueClosure {
    ValueTag t;
    Lambda_t fn;
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

// ============ CONSTRUCTORS ============
#define MakeInt(x)        (Value){ .t = NUMBER,  .integer.value = (x) }
#define MakeBool(x)       (Value){ .t = BOOLEAN, .boolean.value = (x) }
#define MakeString(x)     (Value){ .t = STRING,  .string.value = (x) }
#define MakeEmptyList()   (Value){ .t = LIST,    .list.ptr = NULL }
#define MakeClosure(e, f) (Value){ .t = CLOSURE, .closure.fn = (f), .closure.env = (e) }

// ============ RUNTIME FUNCTIONS ============
void runtime_error(const char* msg);

#endif // KSU_RUNTIME_H
