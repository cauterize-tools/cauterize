#ifndef _CAUTERIZE_CAUT2C11_{{templName}}
#define _CAUTERIZE_CAUT2C11_{{templName}}

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

/*
 * Name: {{templName}}
 * Version: {{templVersion}}
 */

#define NAME_{{templName}} "{{templName}}"
#define VERSION_{{templName}} "{{templVersion}}"
#define MIN_SIZE_{{templName}} ({{templSize.rangeSizeMin}})
#define MIN_SIZE_{{templName}} ({{templSize.rangeSizeMax}})
#define SCHEMA_HASH_{{templName}} {{templHash}}
/*
 * Type Constants
 */
{{#templTypes}}
{{#tyInfConsts}}
{{.}}
{{/tyInfConsts}}
{{/templTypes}}

/*
 * Forward Type Declarations
 */
{{#templTypes}}
{{#tyInfFwdDecls}}
{{.}}
{{/tyInfFwdDecls}}
{{/templTypes}}

/*
 * Type Declarations
 */
{{#templTypes}}
{{#tyInfDecls}}
{{.}}
{{/tyInfDecls}}
{{/templTypes}}

#endif /* _CAUTERIZE_CAUT2C11_{{templName}} */
