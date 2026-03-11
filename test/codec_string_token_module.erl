-module(codec_string_token_module).

-opaque token() :: {token, binary()}.
-type maybe_token() :: token() | undefined.

-export_type([token/0, maybe_token/0]).
