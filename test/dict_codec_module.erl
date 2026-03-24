-module(dict_codec_module).

%% Example module demonstrating a codec for dict:dict/2.
%% The codec uses the concrete type-variable bindings from the sp_type() node
%% to recursively encode and decode keys and values.

-type word_counts() :: dict:dict(binary(), non_neg_integer()).
-type nested() :: dict:dict(binary(), dict:dict(binary(), integer())).

-export_type([word_counts/0, nested/0]).
