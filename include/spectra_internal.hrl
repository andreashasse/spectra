-record(sp_simple_type, {
    type :: spectra:simple_types(),
    meta = #{} :: spectra:sp_type_meta()
}).
-record(sp_tuple, {
    fields :: any | [spectra:sp_type()],
    meta = #{} :: spectra:sp_type_meta()
}).
-record(sp_map, {
    fields :: [spectra:map_field()],
    struct_name :: undefined | atom(),
    meta = #{} :: spectra:sp_type_meta()
}).
-record(sp_rec_field, {
    name :: atom(),
    binary_name :: binary(),
    type :: spectra:sp_type()
}).
-record(sp_rec, {
    name :: atom(),
    fields :: [#sp_rec_field{}],
    arity :: pos_integer(),
    meta = #{} :: spectra:sp_type_meta()
}).
-record(sp_type_with_variables, {
    type :: spectra:sp_type(),
    vars :: [atom()],
    meta = #{} :: spectra:sp_type_meta()
}).
-record(sp_function, {
    args :: any | [spectra:sp_type()],
    return :: spectra:sp_type(),
    meta = #{} :: spectra:sp_type_meta()
}).
-record(sp_union, {
    types = [spectra:sp_type()],
    meta = #{} :: spectra:sp_type_meta()
}).
-record(sp_literal, {
    value :: spectra:literal_value(),
    binary_value :: binary(),
    meta = #{} :: spectra:sp_type_meta()
}).
-record(sp_rec_ref, {
    record_name :: spectra:user_type_name(),
    field_types :: [spectra:record_field_arg()],
    meta = #{} :: spectra:sp_type_meta()
}).
-record(sp_remote_type, {
    mfargs :: {module(), atom(), [spectra:sp_type()]},
    meta = #{} :: spectra:sp_type_meta()
}).
-record(sp_maybe_improper_list, {
    elements :: spectra:sp_type(),
    tail :: spectra:sp_type(),
    meta = #{} :: spectra:sp_type_meta()
}).
-record(sp_nonempty_improper_list, {
    elements :: spectra:sp_type(),
    tail :: spectra:sp_type(),
    meta = #{} :: spectra:sp_type_meta()
}).
-record(sp_user_type_ref, {
    type_name :: spectra:user_type_name(),
    variables :: [spectra:sp_type()],
    meta = #{} :: spectra:sp_type_meta()
}).
-record(sp_var, {
    name :: atom(),
    meta = #{} :: spectra:sp_type_meta()
}).
-record(sp_range, {
    type :: integer,
    lower_bound :: integer(),
    upper_bound :: integer(),
    meta = #{} :: spectra:sp_type_meta()
}).
-record(sp_list, {
    type :: spectra:sp_type(),
    meta = #{} :: spectra:sp_type_meta()
}).
-record(sp_nonempty_list, {
    type :: spectra:sp_type(),
    meta = #{} :: spectra:sp_type_meta()
}).
-record(sp_function_spec, {args :: [spectra:sp_type()], return :: spectra:sp_type()}).
-record(literal_map_field, {
    kind :: assoc | exact,
    name :: atom() | integer(),
    binary_name :: binary(),
    val_type :: spectra:sp_type()
}).
-record(typed_map_field, {
    kind :: assoc | exact,
    key_type :: spectra:sp_type(),
    val_type :: spectra:sp_type()
}).
%% New structured type information
-record(type_info, {
    types = #{} :: #{spectra_type_info:type_key() => spectra:sp_type()},
    records = #{} :: #{atom() => #sp_rec{}},
    functions = #{} :: #{spectra_type_info:function_key() => [#sp_function_spec{}]}
}).
