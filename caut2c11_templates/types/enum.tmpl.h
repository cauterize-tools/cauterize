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

