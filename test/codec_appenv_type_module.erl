-module(codec_appenv_type_module).

%% Module with an opaque type and NO -behaviour(spectra_codec).
%% The codec is registered externally via application env.
-opaque token() :: {token, binary()}.
-type maybe_token() :: token() | undefined.

-export_type([token/0, maybe_token/0]).
