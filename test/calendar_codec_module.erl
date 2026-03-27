-module(calendar_codec_module).

%% Types used in spectra_calendar_codec tests.

-type event() :: #{title => binary(), at => calendar:datetime()}.
-type appointment() :: #{title => binary(), on => calendar:date()}.

-export_type([event/0, appointment/0]).
