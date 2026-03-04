## TODO minor fixes
- [ ] Test error message path better
- [ ] Support otp 28
  - [ ] -nominal my_nominal_type() :: Type.

## TODO Performance improvements
- [ ] For each literal atom, convert to binary in spectra_abstract_code, so that we don't have to do binary_to_existing_atom so see that the values match
- [ ] Do work in spectra_abstract_code
  - [ ] user_type_ref -> just switches to another type with args, can be done in  spectra_abstract_code.
  - [ ] do record_apply_args in spectra_abstract_code.
  - [ ] spectra_json:type_replace_vars can also be done in spectra_abstract_code

## TODO PoC FastApi (elli_openapi)
- [ ] How should I get example values into the mix? Some macro ?spec(example = 2, type = integer) that I write to integer and keep the example separate? Some module attribute? Can that be done without IDEs going bananas?
  - [ ] An alternative is to add an example value to the router

## TODO OpenAPI documentation field gaps

Gap analysis of OpenAPI 3.1 fields not yet supported in `spectra_openapi.erl`.

### High priority (commonly needed)
- [x] `request_body` description — add `description => binary()` to `request_body_spec()` and pass through in `generate_request_body/1`
- [x] `info` description — add `description => binary()` to `openapi_metadata()` and include in the generated `info` object
- [ ] Shared path-level parameters — operations currently repeat identical path params (e.g. `{id}`); OpenAPI supports declaring them once on the Path Item object
- [ ] Tag Objects — tags are currently bare `binary()` values; OpenAPI Tag Objects carry `description` and `externalDocs` per tag; needs top-level `tags` list in the spec

### Medium priority (useful for API evolution)
- [x] `deprecated` on parameters — add `deprecated => boolean()` to `parameter_spec()` / `openapi_parameter()`
- [x] `deprecated` on response headers — add `deprecated => boolean()` to `response_header_spec()` / `openapi_header()`
- [x] Top-level `servers` — base URL configuration; needed for useful interactive docs
- [x] `info` extended fields — `summary`, `contact`, `license`, `termsOfService`

### Lower priority (advanced / niche)
- [ ] `example` / `examples` on Parameter and Header objects — note: examples on *type schemas* already work via the `-spectra` attribute and flow into `components/schemas`; this item is about inline examples on the parameter/header objects themselves
- [ ] Path Item `summary` and `description` — rarely used in practice
- [ ] `links` on responses — advanced OpenAPI feature
- [ ] Per-operation `servers` override
- [ ] `security` / security schemes
- [ ] Top-level `externalDocs` on the OpenAPI object
- [ ] `webhooks` (OpenAPI 3.1)

## Not in scope for now
- [ ] to/from Dynamo DB
- [ ] to/from yaml
- [ ] to/from xml (and xml schemas?)

## Maybe never in scope?
- [ ] to/from grpc: Versioned protocols should probably never be generated?
- [ ] to/from sql: There are so many options that the developer should care about (at least when creating tables), that it doesn't make sense to generate it.
