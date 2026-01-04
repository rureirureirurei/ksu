#include "ksu_runtime.h"

void runtime_error(const char* msg) {
    fprintf(stderr, "Runtime error: %s\n", msg);
    exit(1);
}
