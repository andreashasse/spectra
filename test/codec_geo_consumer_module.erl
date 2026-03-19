-module(codec_geo_consumer_module).

%% A module that references codec_geo_module:point_with_status/1 as a remote
%% parameterized type, exercising codec dispatch through a remote type variable.
-type waypoint() :: codec_geo_module:point_with_status(binary()).
-type route() :: #{start := waypoint(), finish := waypoint()}.

-export_type([waypoint/0, route/0]).
