-module(inline_doc_remote_helper).

%% Helper for spectra_json_schema_inline_doc_test: a documented type that is
%% inlined into a schema generated from another module.

-spectra(#{
    title => <<"Remote Tag">>,
    description => <<"A tag defined in another module">>,
    deprecated => true,
    examples => [<<"remote">>]
}).
-type tag() :: binary().

-export_type([tag/0]).
