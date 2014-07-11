/* BUILD: clang -pedantic -std=c11 -Wall -Wextra -c {{libname}}.c -o {{libname}}.o */
#include "{{templName}}.h"

{{#templTypes}}
{{#tyInfDecl}}
/* Type: {{tyInfName}} */
enum caut_status pack_{{tyInfName}}(struct caut_pack_iter * const iter, {{tyInfDecl}} const * const obj)
{
  /* BODY */
}

enum caut_status unpack_{{tyInfName}}(struct caut_unpack_iter * const iter, {{tyInfDecl}} * const obj)
{
  /* BODY */
}

size_t packed_size_{{tyInfName}}({{tyInfDecl}} const * const obj) {
  /* BODY */
}
{{/tyInfDecl}}

{{/templTypes}}
