-module(codec_appenv_rec_module).

%% Module with records and NO -behaviour(spectra_codec).
%% The codec is registered externally via application env.
-record(point2d, {x :: number(), y :: number()}).
-record(shape, {label :: binary(), center :: #point2d{}}).

-type point2d() :: #point2d{}.
-type shape() :: #shape{}.

-export_type([point2d/0, shape/0]).
