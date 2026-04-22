-module(no_debug_info_test).

-include_lib("eunit/include/eunit.hrl").

no_debug_info_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun({ModuleName, _}) ->
        [test_module_without_debug_info(ModuleName)]
    end}.

setup() ->
    ModuleName = test_no_debug_module,
    Forms = [
        {attribute, 1, module, ModuleName},
        {attribute, 2, export, [{hello, 0}]},
        {attribute, 3, type, {user_id, {type, 3, pos_integer, []}, []}},
        {function, 4, hello, 0, [{clause, 4, [], [], [{atom, 4, ok}]}]}
    ],
    {ok, ModuleName, Bin} = compile:forms(Forms, []),
    %% Write outside the code path so dialyzer does not try to scan a beam
    %% that has no debug_info.  code:load_abs/1 needs the file on disk so
    %% that code:which/1 and beam_lib can resolve it.
    Dir = filename:join(os:getenv("TMPDIR", "/tmp"), "spectra_test"),
    ok = filelib:ensure_path(Dir),
    BeamPath = filename:join(Dir, atom_to_list(ModuleName)),
    ok = file:write_file(BeamPath ++ ".beam", Bin),
    code:purge(ModuleName),
    code:load_abs(BeamPath),
    {ModuleName, BeamPath ++ ".beam"}.

cleanup({ModuleName, BeamFile}) ->
    file:delete(BeamFile),
    code:purge(ModuleName),
    code:delete(ModuleName).

test_module_without_debug_info(ModuleName) ->
    ?_assertError(
        {module_not_compiled_with_debug_info, ModuleName, _},
        spectra_abstract_code:types_in_module(ModuleName)
    ).
