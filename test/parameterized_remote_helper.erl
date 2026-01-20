-module(parameterized_remote_helper).

%% A simple parameterized wrapper type using maps (JSON schema supports maps)
-type wrapper(T) :: #{result := T, error := binary() | nil}.

%% A list-based parameterized type
-type list_wrapper(T) :: [T].

%% A nested parameterized type for more complex testing
-type result(Value) :: #{status := binary(), data := Value}.

-export_type([wrapper/1, list_wrapper/1, result/1]).
