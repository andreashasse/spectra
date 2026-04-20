-module(spectra_transform_tests).

-include("../include/spectra_internal.hrl").
-include_lib("eunit/include/eunit.hrl").

generated_function_matches_beam_lib_test() ->
    %% The parse transform builds type_info() from the forms it receives, which
    %% do not yet contain the synthesized __spectra_type_info__/0 function.
    %% beam_lib sees the compiled BEAM afterwards and does see that function.
    %% Strip it from the beam_lib view before comparing.
    Module = spectra_test_module_transform,
    Generated = Module:'__spectra_type_info__'(),
    FromBeam = strip_generated(spectra_abstract_code:types_in_module(Module)),
    ?assertEqual(FromBeam, Generated).

strip_generated(#type_info{functions = Fns} = TI) ->
    TI#type_info{functions = maps:remove({'__spectra_type_info__', 0}, Fns)}.
