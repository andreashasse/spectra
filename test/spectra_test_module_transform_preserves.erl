-module(spectra_test_module_transform_preserves).

-compile({parse_transform, spectra_transform}).
-compile(export_all).
-compile(nowarn_export_all).

-include("../include/spectra_internal.hrl").

-export_type([my_type/0]).

-type my_type() :: integer().

-spec '__spectra_type_info__'() -> spectra:type_info().
'__spectra_type_info__'() ->
    #type_info{
        module = ?MODULE,
        types = #{},
        records = #{},
        functions = #{},
        implements_codec = false
    }.
