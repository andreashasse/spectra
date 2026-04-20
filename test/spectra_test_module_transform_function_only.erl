-module(spectra_test_module_transform_function_only).

-compile({parse_transform, spectra_transform}).

'__spectra_type_info__'() ->
    hand_written_marker.
