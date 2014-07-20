/* Partial: {{pName}} */
struct {{pName}} {
  enum {{pName}}_tag {
{{#pFields}}
    {{pName}}_{{fName}} = {{fIndex}},
{{/pFields}}
  } _tag;
  {{pLengthRepr}} _len;

  union {
{{#pFields}}
{{#fRefDecl}}
    {{fRefDecl}} {{fName}};
{{/fRefDecl}}
{{^fRefDecl}}
    /* {{fName}} is void. */
{{/fRefDecl}}
{{/pFields}}
  };
}
