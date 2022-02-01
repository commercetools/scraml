@@toc { depth=1 }

# Migration Guide

## v0.10.x -> v0.11.x

Version `0.11.0` introduces a breaking change when `additionalProperties` support is enabled.  A second constructor argument list is added to these types which did not exist in `0.10.x` and below.

The `Default` policy is to emit code which adheres closely to the [RAML Additional Properties](https://github.com/raml-org/raml-spec/blob/master/versions/raml-10/raml-10.md/#additional-properties) specification.

See @ref:[Setup](setup.md)

@@@ note

Use the `scraml.FieldMatchPolicy.Exact` policy to produce generated code which behaves similarly to `0.10.x`.

@@@

