#include "types.h"
#include <string.h>

int convert_to_type_enum(char *in) {
    if (strcmp(in, "int") == 0) {
        return TYPE_INT;        
    } else if (strcmp(in, "char") == 0) {
        return TYPE_CHAR;        
    } else if (strcmp(in, "string") == 0) {
        return TYPE_STRING;        
    } else if (strcmp(in, "bool") == 0) {
        return TYPE_BOOL;        
    }
    return -1;
}

