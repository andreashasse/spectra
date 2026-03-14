-module(spectra_codec).

-doc """
Behaviour for custom codecs that extend spectra's encoding, decoding, and schema
generation for specific types and formats.

## Params argument

The `Params` argument passed to every callback is the value of the
`type_parameters` key in the `-spectra(#{type_parameters => Value})` attribute
on the type definition, or `undefined` if that attribute is absent.

**Important**: `Params` is unrelated to Erlang type variables (generic
parameters such as the `T` in `-type wrapper(T) :: {wrapper, T}`). When a
codec is invoked for a parameterised type, the concrete bindings for those type
variables are **not** available in `Params`. Codec types with Erlang type
variables are therefore not useful in practice — the codec cannot act on the
variable bindings. Use `type_parameters` in the `-spectra` attribute to pass
static, per-type configuration to your codec instead.
""".

-callback encode(
    Format :: atom(),
    TypeRef :: spectra:sp_type_reference(),
    Data :: dynamic(),
    Params :: term()
) ->
    spectra:codec_encode_result().
-callback decode(
    Format :: atom(),
    TypeRef :: spectra:sp_type_reference(),
    Input :: dynamic(),
    Params :: term()
) ->
    spectra:codec_decode_result().
-callback schema(
    Format :: atom(), TypeRef :: spectra:sp_type_reference(), Params :: term()
) ->
    dynamic().

-optional_callbacks([schema/3]).
