-module(codec_top_module).

%% Top-level module that uses codec_middle_module:event(), which uses calendar:datetime().
%% Tests cascading codec resolution.
-type schedule() :: #{events => [codec_middle_module:event()]}.

-export_type([schedule/0]).
