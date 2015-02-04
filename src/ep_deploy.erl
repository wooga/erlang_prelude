-module(ep_deploy).

%% ==================================================================
%% ep_deploy Behaviour Specification
%% ==================================================================

-callback reload([atom()]) -> ok.


%% ==================================================================
%% API Function Exports
%% ==================================================================

-export([ reload_app/1
        , reload_apps/1
        , load_app/1
        , modules_for_app/1
        , reload_module/1
        , reload_module/2
        , add_paths/1
        , module_modified/1
        , find_module_file/1
        , modified_deps/0
        , old_code/1
        , stuck_old_code/1
        ]).


%% ==================================================================
%% API Function Definitions
%% ==================================================================

reload_app(sbs_lib) ->
    reload_apps([sbs_lib]);
reload_app(App) ->
    reload_apps([sbs_lib, App]).


reload_apps(Apps) ->
    reload_apps(Apps, []).
reload_apps([App|T], Acc) ->
    ModifiedModules = lists:filter(fun module_modified/1, modules_for_app(App)),
    [] = [M || M <- ModifiedModules, code:soft_purge(M) =:= false],
    ok = add_paths("deps"),
    [code:load_file(M) || M <- ModifiedModules],
    case application:get_key(App, mod) of
        {ok, {AppModule, _}} -> AppModule:reload(ModifiedModules);
        _Else                -> ok
    end,
    reload_apps(T, Acc ++ ModifiedModules);
reload_apps([], ModifiedModules) ->
    ModifiedModules.

load_app(App) ->
    [code:load_file(M) || M <- modules_for_app(App)].


modules_for_app(App) ->
    case code:where_is_file(atom_to_list(App) ++ ".app") of
        non_existing ->
            {error, no_app_file};
        Path ->
            case file:consult(Path) of
                {ok, [{application, App, Props}]} ->
                    proplists:get_value(modules, Props, []);
                {error, Error} ->
                    {error, Error}
            end
    end.


reload_module(M) ->
    code:soft_purge(M) andalso code:load_file(M).

reload_module(M, force) ->
    code:purge(M),
    code:load_file(M).


add_paths(BaseDir) ->
    case file:list_dir(BaseDir) of
        {ok, Filenames} ->
            lists:foreach(fun (Dep) ->
                              true == code:add_path(
                                          filename:join([BaseDir, Dep, "ebin"]))
                          end, Filenames),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.


%% @doc: List loaded modules from deps/ that have changed on disk,
%% will not notice new beams that have not yet been loaded.
modified_deps() ->
    DepModules = deps_modules(),
    lists:filter(fun (Module) -> not is_latest_version(Module) end, DepModules).


old_code(App) when is_atom(App) ->
    old_code([App]);
old_code(Apps) when is_list(Apps) ->
    lists:filter(fun erlang:check_old_code/1,
                 lists:flatten(
                    [deps_modules() | [modules_for_app(App) || App <- Apps]])).


%% @doc: Returns a list of modules for which at least one process is using old code
stuck_old_code(AppOrApps) ->
    %% NOTE: soft purging a lot of modules can take a while!!
    lists:filter(fun (M) -> not code:soft_purge(M) end, old_code(AppOrApps)).


%% ==================================================================
%% Internal Function Definitions
%% ==================================================================

deps_modules() ->
    {DepModules, _} = lists:unzip(lists:filter(
                        fun ({_, Beam}) ->
                                is_list(Beam) andalso
                                    lists:prefix(filename:absname("deps"), Beam)
                        end,
                        code:all_loaded())),
    DepModules.


%% @doc: Check loaded vsn versus vsn on disk, do this since compile times changes
%% frequently for our deps even if source has not (because of delete-deps).
is_latest_version(Module) ->
    case code:get_object_code(Module) of
        {Module, _ObjectCode, Beam} ->
            {ok, {Module, VsnDisk}} = beam_lib:version(Beam),
            %% Can't use object code since it is read from disk
            VsnLoaded = proplists:get_value(vsn, Module:module_info(attributes)),
            VsnLoaded =:= VsnDisk;
        error ->
            %% Module not loaded
            false
    end.


%%
%% Snipped from Wings3d
%%

module_modified(Module) ->
    case code:is_loaded(Module) of
        {file, preloaded} ->
            false;
        {file, Path} ->
            CompileOpts = proplists:get_value(compile, Module:module_info()),
            CompileTime = proplists:get_value(time, CompileOpts),
            Src = proplists:get_value(source, CompileOpts),
            module_modified(Path, CompileTime, Src);
        _ ->
            true %% consider new modules to be modified so they get loaded
    end.

module_modified(Path, PrevCompileTime, PrevSrc) ->
    case find_module_file(Path) of
        false ->
            false;
        ModPath ->
            {ok, {_, [{_, CB}]}} = beam_lib:chunks(ModPath, ["CInf"]),
            CompileOpts =  binary_to_term(CB),
            CompileTime = proplists:get_value(time, CompileOpts),
            Src = proplists:get_value(source, CompileOpts),
            not ((CompileTime == PrevCompileTime) and (Src == PrevSrc))
    end.

find_module_file(Path) ->
    case file:read_file_info(Path) of
        {ok, _} ->
            Path;
        _ ->
            %% may be the path was changed?
            case code:where_is_file(filename:basename(Path)) of
                non_existing ->
                    false;
                NewPath ->
                    NewPath
            end
    end.
