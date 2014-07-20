/* Enum: {{eName}} */
struct {{eName}} {
  enum {{eName}}_tag {
{{#eFields}}
    {{eName}}_{{fName}} = {{fIndex}},
{{/eFields}}
  } _tag;

  union {
{{#eFields}}
{{#fRefDecl}}
    {{fRefDecl}} {{fName}};
{{/fRefDecl}}
{{^fRefDecl}}
    /* {{fName}} is void. */
{{/fRefDecl}}
{{/eFields}}
  };
}
