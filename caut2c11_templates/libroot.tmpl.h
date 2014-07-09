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

uint8_t const SCHEMA_HASH_{{templName}}[] = {{templHash}}

/*
 * Forward Declarations and constants.
 */

{{#templTypes}}
/* Type: {{tyInfName}} */
{{#tyInfFwdDecls}}
{{.}}
{{/tyInfFwdDecls}}
uint8_t const TYPE_HASH_{{templName}}_{{tyInfName}}[] = {{tyInfHash}};
{{#tyInfConsts}}
#define TYPE_CONST_{{templName}}_{{tyInfName}}_{{constInfName}} ({{constInfValue}})
{{.}}
{{/tyInfConsts}}
#define TYPE_SIZE_MIN_{{templName}}_{{tyInfName}} ({{tyInfSize.rangeSizeMin}})
#define TYPE_SIZE_MAX_{{templName}}_{{tyInfName}} ({{tyInfSize.rangeSizeMax}})

{{/templTypes}}

{{#templTypes}}
{{tyInfDeclBody}}

{{/templTypes}}

{{#templTypes}}
{{#tyInfDecl}}
/* Type: {{tyInfName}} */
enum caut_status pack_{{tyInfName}}(struct caut_pack_iter * const iter, {{tyInfDecl}} const * const obj);
enum caut_status unpack_{{tyInfName}}(struct caut_unpack_iter * const iter, {{tyInfDecl}} * const obj);
size_t packed_size_{{tyInfName}}({{tyInfDecl}} const * const obj);
{{/tyInfDecl}}
{{^tyInfDecl}}
/* Type: {{tyInfName}} */
/* No packer, unpacker, or size checkers are defined for the type {{tyInfName}}. */
{{/tyInfDecl}}

{{/templTypes}}

#endif /* _CAUTERIZE_CAUT2C11_{{templName}}_ */
