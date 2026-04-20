-module(spectra_test_module_transform).

-compile({parse_transform, spectra_transform}).

-export([my_function/1]).
-export_type([my_type/0, my_record_t/0]).

-record(my_record, {field1 :: integer(), field2 :: string()}).

-spectra(#{title => <<"My Type">>, description => <<"A test type">>}).
-type my_type() :: string().

-spectra(#{title => <<"My Record">>, description => <<"A test record">>}).
-type my_record_t() :: #my_record{}.

-spec my_function(integer()) -> string().
my_function(N) ->
    integer_to_list(N).
