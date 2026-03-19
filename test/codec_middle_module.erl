-module(codec_middle_module).

%% Intermediate module that uses calendar:datetime(), no codec of its own.
-type event() :: #{name => binary(), at => calendar:datetime()}.

-export_type([event/0]).
