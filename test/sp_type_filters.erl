-module(sp_type_filters).

-include("../include/spectra_internal.hrl").

-export([
    json_roundtrip_safe/1,
    binary_string_roundtrip_safe/1,
    string_roundtrip_safe/1
]).

-spec json_roundtrip_safe(spectra:sp_type()) -> boolean().
json_roundtrip_safe(#sp_simple_type{type = T}) ->
    json_simple_safe(T);
json_roundtrip_safe(#sp_literal{}) ->
    true;
json_roundtrip_safe(#sp_range{}) ->
    true;
json_roundtrip_safe(#sp_list{type = E}) ->
    json_roundtrip_safe(E);
json_roundtrip_safe(#sp_nonempty_list{type = E}) ->
    json_roundtrip_safe(E);
json_roundtrip_safe(#sp_tuple{}) ->
    false;
json_roundtrip_safe(#sp_map{fields = Fields}) ->
    lists:all(fun json_map_field_safe/1, Fields);
json_roundtrip_safe(#sp_rec{fields = Fields}) ->
    lists:all(
        fun(#sp_rec_field{type = T}) -> json_roundtrip_safe(T) end,
        Fields
    );
json_roundtrip_safe(#sp_rec_ref{}) ->
    false;
json_roundtrip_safe(#sp_union{types = Types}) ->
    lists:all(fun json_roundtrip_safe/1, Types);
json_roundtrip_safe(#sp_user_type_ref{}) ->
    false;
json_roundtrip_safe(#sp_remote_type{}) ->
    false;
json_roundtrip_safe(#sp_function{}) ->
    false;
json_roundtrip_safe(#sp_var{}) ->
    false;
json_roundtrip_safe(#sp_type_with_variables{}) ->
    false;
json_roundtrip_safe(#sp_maybe_improper_list{}) ->
    false;
json_roundtrip_safe(#sp_nonempty_improper_list{}) ->
    false.

json_map_field_safe(#literal_map_field{val_type = V}) ->
    json_roundtrip_safe(V);
json_map_field_safe(#typed_map_field{key_type = K, val_type = V}) ->
    json_map_key_safe(K) andalso json_roundtrip_safe(V).

%% Match spectra_json_schema:can_be_json_key/2.
json_map_key_safe(#sp_simple_type{type = T}) ->
    lists:member(T, [binary, nonempty_binary, string, nonempty_string, atom]);
json_map_key_safe(_) ->
    false.

json_simple_safe(integer) -> true;
json_simple_safe(non_neg_integer) -> true;
json_simple_safe(neg_integer) -> true;
json_simple_safe(pos_integer) -> true;
json_simple_safe(float) -> true;
json_simple_safe(number) -> true;
json_simple_safe(boolean) -> true;
json_simple_safe(atom) -> true;
json_simple_safe(binary) -> true;
json_simple_safe(nonempty_binary) -> true;
json_simple_safe(string) -> true;
json_simple_safe(nonempty_string) -> true;
json_simple_safe(_) -> false.

-spec binary_string_roundtrip_safe(spectra:sp_type()) -> boolean().
binary_string_roundtrip_safe(#sp_simple_type{type = T}) ->
    bs_simple_safe(T);
binary_string_roundtrip_safe(#sp_literal{value = V}) ->
    is_atom(V) orelse is_integer(V);
binary_string_roundtrip_safe(#sp_range{}) ->
    true;
binary_string_roundtrip_safe(#sp_union{types = Types}) ->
    %% Decoding is first-match over an untagged wire format, so only
    %% literal-only unions (enums) roundtrip unambiguously.
    lists:all(fun(T) -> is_record(T, sp_literal) end, Types) andalso
        lists:all(fun binary_string_roundtrip_safe/1, Types);
binary_string_roundtrip_safe(_) ->
    false.

bs_simple_safe(integer) -> true;
bs_simple_safe(non_neg_integer) -> true;
bs_simple_safe(neg_integer) -> true;
bs_simple_safe(pos_integer) -> true;
bs_simple_safe(float) -> true;
bs_simple_safe(number) -> true;
bs_simple_safe(boolean) -> true;
bs_simple_safe(atom) -> true;
bs_simple_safe(binary) -> true;
bs_simple_safe(nonempty_binary) -> true;
bs_simple_safe(string) -> true;
bs_simple_safe(nonempty_string) -> true;
bs_simple_safe(_) -> false.

-spec string_roundtrip_safe(spectra:sp_type()) -> boolean().
string_roundtrip_safe(Type) ->
    binary_string_roundtrip_safe(Type).
