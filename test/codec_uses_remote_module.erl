-module(codec_uses_remote_module).

%% Module that uses codec_remote_impl_module:color().
%% No app env needed — codec_remote_impl_module implements spectra_codec directly.
-type palette() :: #{primary => codec_remote_impl_module:color()}.

-export_type([palette/0]).
