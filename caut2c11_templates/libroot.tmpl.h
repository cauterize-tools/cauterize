#ifndef _CAUTERIZE_CAUT2C_{{libname}}
#define _CAUTERIZE_CAUT2C_{{libname}}

/* Compile-time constant information. */
{{preprocessor}}

/* Forward delcarations for types. */
{{forwardDecls}}

/* Type delcarations. */
{{#types}}
{{#enumTypes}}
enum {{enumName}}_tag {
  {{#enumFields}}
  {{fieldName}} = {{fieldIndex}},
  {{/enumFields}}
}

struct {{enumName}} {
  enum {{enumName}}_tag tag;
  union {
    {{#enumFields}}
    {{fieldRef}} {{fieldName}};
    {{/enumFields}}
  };
}

{{/enumTypes}}
{{/types}}

/* Prototypes for packing functions. */
{{packPrototypes}}

/* Prototypes for unpacking functions. */
{{unpackPrototypes}}

#endif /* _CAUTERIZE_CAUT2C_{{libname}} */
