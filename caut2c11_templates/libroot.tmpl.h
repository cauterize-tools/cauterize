#ifndef _CAUTERIZE_CAUT2C11_{{templName}}_
#define _CAUTERIZE_CAUT2C11_{{templName}}_

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#include <cauterize.h>

/*
 * Name: {{templName}}
 * Version: {{templVersion}}
 */

#define NAME_{{templName}} "{{templName}}"
#define VERSION_{{templName}} "{{templVersion}}"
#define MIN_SIZE_{{templName}} ({{templSize.rangeSizeMin}})
#define MIN_SIZE_{{templName}} ({{templSize.rangeSizeMax}})

const uint8_t SCHEMA_HASH_{{templName}}[] = {{templHash}}

/*
 * Forward Declarations
 */

{{#templTypes}}
/* Type: {{tyInfName}} */
const uint8_t TYPE_HASH_{{templName}}_{{tyInfName}}[] = {{tyInfHash}};
{{#tyInfConsts}}
#define TYPE_CONST_{{templName}}_{{tyInfName}}_{{constInfName}} ({{constInfValue}})
{{.}}
{{/tyInfConsts}}

#define TYPE_SIZE_MIN_{{templName}}_{{tyInfName}} ({{tyInfSize.rangeSizeMin}})
#define TYPE_SIZE_MAX_{{templName}}_{{tyInfName}} ({{tyInfSize.rangeSizeMax}})

{{#tyInfFwdDecls}}
{{.}}
{{/tyInfFwdDecls}}

{{/templTypes}}

/*
 * Type Declarations
 */

{{#templTypes}}
{{tyInfDeclBody}}
{{#tyInfDecl}}
enum caut_status pack_{{tyInfName}}(struct caut_pack_iter * iter, {{tyInfDecl}} const * const obj);
enum caut_status unpack_{{tyInfName}}(struct caut_unpack_iter * iter, {{tyInfDecl}} * obj);
size_t packed_size_{{tyInfName}}({{tyInfDecl}} const * const obj);
{{/tyInfDecl}}
{{^tyInfDecl}}
/* No packer, unpacker, or size checkers are defined for the type {{tyInfName}}. */
{{/tyInfDecl}}

{{/templTypes}}

#endif /* _CAUTERIZE_CAUT2C11_{{templName}}_ */
