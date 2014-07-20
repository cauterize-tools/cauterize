/* Set: {{tName}} */
struct {{tName}} {
  {{tFlagRepr}} flags;

{{#tFields}}
  {{fRefDecl}} {{fName}}; /* Index: {{fIndex}} */
{{/tFields}}
}
