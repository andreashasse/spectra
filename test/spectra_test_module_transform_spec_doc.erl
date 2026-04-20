-module(spectra_test_module_transform_spec_doc).

-compile({parse_transform, spectra_transform}).

-export(['__spectra_type_info__'/0]).

-spectra(#{summary => <<"Hand-written type info">>}).
-spec '__spectra_type_info__'() -> spectra:type_info().
'__spectra_type_info__'() ->
    hand_written.
