-module(spectra_transform_tests).

-include("../include/spectra_internal.hrl").
-include_lib("eunit/include/eunit.hrl").

generated_function_matches_beam_lib_test() ->
    %% Type extraction must be stable whether or not the transform was
    %% applied: the generated value and the beam_lib-derived value must
    %% compare equal without stripping __spectra_type_info__/0.
    Module = spectra_test_module_transform,
    Generated = Module:'__spectra_type_info__'(),
    FromBeam = spectra_abstract_code:types_in_module(Module),
    ?assertEqual(FromBeam, Generated).

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

injects_function_when_only_export_present_test() ->
    %% Module opts into the transform and adds -export for
    %% __spectra_type_info__/0 but does not define the function body.
    %% The transform must inject the body so the module compiles and the
    %% function is callable.
    Module = spectra_test_module_transform_export_only,
    TypeInfo = Module:'__spectra_type_info__'(),
    ?assertEqual(Module, TypeInfo#type_info.module),
    %% The injected value reflects the module's declared types.
    Types = TypeInfo#type_info.types,
    ?assertMatch(#{{only_export_type, 0} := _}, Types).

injects_function_when_export_and_spec_present_test() ->
    %% Module opts into the transform and provides -export and a hand-written
    %% -spec for __spectra_type_info__/0 but no body. The transform must
    %% inject only the body, not a duplicate spec.
    Module = spectra_test_module_transform_spec_only,
    TypeInfo = Module:'__spectra_type_info__'(),
    ?assertEqual(Module, TypeInfo#type_info.module),
    Types = TypeInfo#type_info.types,
    ?assertMatch(#{{only_spec_type, 0} := _}, Types).

types_in_forms_does_not_orphan_spectra_doc_on_handwritten_spec_test() ->
    %% Module opts into the transform and fully defines
    %% __spectra_type_info__/0 by hand, with a -spectra(...) doc attribute
    %% preceding the -spec. types_in_forms must not raise
    %% {orphaned_spectra, ...} when analysing such a module.
    Module = spectra_test_module_transform_spec_doc,
    TypeInfo = spectra_abstract_code:types_in_module(Module),
    ?assertEqual(Module, TypeInfo#type_info.module).

injects_export_when_only_function_present_test() ->
    %% Module opts into the transform and defines __spectra_type_info__/0
    %% but does not export it (no -export, no export_all). The transform
    %% must inject the export attribute so the hand-written body is
    %% reachable from outside the module.
    Module = spectra_test_module_transform_function_only,
    %% Force the module to be loaded so function_exported/3 sees it.
    TypeInfo = Module:'__spectra_type_info__'(),
    ?assertEqual(hand_written_marker, TypeInfo),
    ?assert(erlang:function_exported(Module, '__spectra_type_info__', 0)).
