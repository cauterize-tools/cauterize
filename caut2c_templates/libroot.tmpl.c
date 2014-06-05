/* BUILD: clang -pedantic -std=c11 -Wall -Wextra {{libname}}.c -o {{libname}} */
#include "{{libname}}.h"

/* Definitions for packing functions. */
{{packDefinitions}}

/* Definitions for unpacking functions. */
{{unpackDefinitions}}
