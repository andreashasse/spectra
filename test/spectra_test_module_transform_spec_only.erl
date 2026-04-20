-module(spectra_test_module_transform_spec_only).

-compile({parse_transform, spectra_transform}).

-export(['__spectra_type_info__'/0]).
-export_type([only_spec_type/0]).

-type only_spec_type() :: integer().

-spec '__spectra_type_info__'() -> spectra:type_info().
