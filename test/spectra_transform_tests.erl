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

preserves_hand_written_type_info_test() ->
    %% Module opts into the parse transform but already defines
    %% __spectra_type_info__/0 (via -compile(export_all) rather than a
    %% matching -export attribute). The transform must not inject a
    %% duplicate and must leave the hand-written implementation intact.
    Module = spectra_test_module_transform_preserves,
    TypeInfo = Module:'__spectra_type_info__'(),
    ?assertEqual(Module, TypeInfo#type_info.module),
    ?assertEqual(#{}, TypeInfo#type_info.types),
    ?assertEqual(#{}, TypeInfo#type_info.records),
    ?assertEqual(#{}, TypeInfo#type_info.functions).
