%% @copyright 2026 Marc Worrell
%% @doc Capture and activate a completed build. Call under the update barrier.
-module(zotonic_filehandler_reload).
-export([all/0, modules/1, load/1]).

-spec all() -> {ok, [module()]} | {error, term()}.
all() ->
    %% Discover newly built applications before resolving modules. Preserve code
    %% path precedence for existing applications, including checkouts/overrides.
    Build = filename:dirname(code:lib_dir(zotonic_filehandler)),
    Existing = [filename:basename(filename:dirname(P)) || P <- code:get_path()],
    New = [P || P <- filelib:wildcard(filename:join([Build, "*", "ebin"])),
        not lists:member(filename:basename(filename:dirname(P)), Existing)],
    ok = code:add_pathsa(New),
    Loaded = [M || {M, P} <- code:all_loaded(), is_list(P),
        filename:extension(P) =:= ".beam"],
    %% Include unloaded project modules too (not only reloader:all_changed/0).
    %% This makes new migration helpers part of the same captured generation.
    Project = [P || P <- code:get_path(), is_project(P)],
    Unloaded = [list_to_atom(filename:basename(F, ".beam")) || P <- Project,
        F <- filelib:wildcard(filename:join(P, "*.beam"))],
    modules(lists:usort(Loaded ++ Unloaded)).

is_project(Ebin) ->
    App = filename:dirname(Ebin),
    lists:prefix("zotonic_", filename:basename(App)) orelse
        filelib:wildcard(filename:join([App, "priv", "zotonic_site.*"])) =/= [].

-spec modules(Modules) -> {ok, [module()]} | {error, term()} when Modules :: [module()].
modules(Modules) ->
    case snapshot(lists:usort(Modules), []) of
        {ok, Beams} -> load(Beams);
        {error, _} = Error -> Error
    end.

snapshot([], Acc) -> {ok, lists:reverse(Acc)};
snapshot([M | Ms], Acc) ->
    case code:where_is_file(atom_to_list(M) ++ ".beam") of
        Path when is_list(Path) ->
            case file:read_file(Path) of
                {ok, Binary} ->
                    case beam_lib:md5(Binary) of
                        {ok, {M, MD5}} ->
                            case is_current(M, MD5) of
                                true -> snapshot(Ms, Acc);
                                false -> snapshot(Ms, [{M, Path, Binary} | Acc])
                            end;
                        Error -> {error, {invalid_beam, M, Path, Error}}
                    end;
                {error, Reason} -> {error, {read_beam, M, Path, Reason}}
            end;
        _ -> {error, {missing_beam, M}}
    end.

is_current(Module, MD5) ->
    case code:is_loaded(Module) of
        false -> false;
        _ -> Module:module_info(md5) =:= MD5
    end.

%% @doc All or none: never notify about a partially loaded batch. An on_load
%% module cannot be loaded atomically; report this and require a node restart.
-spec load(Beams) -> {ok, [module()]} | {error, term()} when
    Beams :: [{module(), file:filename_all(), binary()}].
load([]) -> {ok, []};
load(Beams) ->
    case code:prepare_loading(Beams) of
        {ok, Prepared} ->
            Modules = [M || {M, _, _} <- Beams],
            case [M || M <- Modules, not code:soft_purge(M)] of
                [] ->
                    case code:finish_loading(Prepared) of
                        ok -> {ok, Modules};
                        {error, _} = Error -> Error
                    end;
                Busy -> {error, {old_code_in_use, Busy}}
            end;
        {error, _} = Error -> Error
    end.
