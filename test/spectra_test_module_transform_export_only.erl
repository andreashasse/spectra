-module(spectra_test_module_transform_export_only).

-compile({parse_transform, spectra_transform}).

-export(['__spectra_type_info__'/0]).
-export_type([only_export_type/0]).

-type only_export_type() :: integer().
