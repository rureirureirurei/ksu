// ************ STRUCTS AND TYPES  ************
union Value;
typedef union Value Value;
enum ValueTag { NUMBER, STRING, CLOSURE, BOOLEAN, LIST }
typedef enum ValueTag ValueTag;

// Primitive values
struct ValueInt { ValueTag t; int value; }
struct ValueString { ValueTag t; char* value; }
struct ValueBool { ValueTag t; bool value; }

// Lists
struct ListItem {
    Value v; 
    ListItem* next; // if equal to NULL, then this node is the last one
}
struct ValueList {
    ValueTag t;
    ListItem* ptr; // if equal to NULL, then list is empty
}

// Closures
// Lamda_t is function that takes 
// - list of arguments
// - enviroment of the captured variables
// both lists are zero-terminated, but it anyway shouldn't matter as we define their usage compile-time
typedef Value** ClosureEnv; // arguments are passed as values (copy), whilst env should point to 
typedef Value (Lamda_t*)(Value*, ClosureEnv);  

struct ValueClosure {
    ValueTag t;
    Lambda_t fn;
    ClosureEnv env;
}

// And finally, union
union Value {
    enum ValueTag t;
    struct ValueInt integer;
    struct ValueString string;
    struct ValueBool boolean;
    struct ValueList list;
    struct ValueClosure closure;
}

// ************ MEMORY ALLOCATION FUNCTIONS ************
#define MakeInt(x)        (Value){ .t = NUMBER,  .integer.value = (x) }
#define MakeBool(x)       (Value){ .t = BOOLEAN, .boolean.value = (x) }
#define MakeString(x)     (Value){ .t = STRING,  .string.value = (x) }
#define MakeEmptyList()   (Value){ .t = LIST,    .list.ptr = NULL }
#define MakeClosure(e, f) (Value){ .t = CLOSURE, .closure.fn = (f), .closure.env = (e) }