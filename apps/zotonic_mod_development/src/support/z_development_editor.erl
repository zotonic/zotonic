%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Discover and launch a developer's local editor without a command shell.
%% @end

%% Copyright 2026 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(z_development_editor).
-moduledoc(<<"
Editor discovery for mod_development. Container detection always takes precedence:
no discovery commands or editor processes are started inside a container.

The optional zotonic application setting `development_editor` is a trusted
`{Executable, Args}` tuple; args may contain `{file}`, `{line}`, `{column}`.
`ZOTONIC_EDITOR`, VISUAL and EDITOR accept quoted words, not shell expressions.
An explicit preference that cannot run is reported, never silently replaced.
Set `development_editor_browser_only` to true for remote hosts or containers
whose runtime is not automatically recognised. Browser settings are separate.
">>).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    detect/1, open/4, edit/1, edit/2, browser_only/0,
     select/1, split_command/1, arguments/4, save_preference/2,
     source_location/1, source_location/2
]).

%% @doc Check whether configuration or container markers require browser-only editor launches.
-spec browser_only() -> boolean().
browser_only() ->
    z_config:get(development_editor_browser_only, false) =:= true
    orelse filelib:is_file("/.dockerenv")
    orelse filelib:is_file("/run/.containerenv")
    orelse env("container") =/= false
    orelse env("KUBERNETES_SERVICE_HOST") =/= false
    orelse contains_container("/proc/1/cgroup")
    orelse contains_container("/proc/self/mountinfo").

%% @doc Look for known container runtime markers in a Linux process metadata file.
contains_container(Path) ->
    case file:read_file(Path) of
        {ok, Data} ->
            lists:any(
                fun(S) -> binary:match(Data, S) =/= nomatch end,
                [<<"docker">>, <<"kubepods">>, <<"containerd">>, <<"libpod">>]);
        _ ->
            false
    end.

%% @doc Discover and rank local editor candidates for a file, then apply the machine preference.
%% Containers return no local candidates and run no discovery commands.
-spec detect(File) -> map() when File :: file:filename_all().
detect(File) ->
    case browser_only() of
        true ->
            #{
                browser_only => true,
                candidates => [],
                selected => undefined
            };
        false ->
            Installed0 = installed(),
            Sessions = sessions(Installed0),
            %% A responding Emacs client supports positions and takes precedence
            %% over the macOS application launcher, which only opens files.
            Installed = case lists:any(fun(C) -> maps:get(id, C) =:= <<"emacsclient">> end, Sessions) of
                true -> [C || C <- Installed0, maps:get(id, C) =/= <<"emacs">>];
                false -> Installed0
            end,
            Preferred = preference(Installed, Sessions),
            Associated = association(File, Installed),
            Running = running(Installed),
            Candidates = unique(Preferred ++ Sessions ++ Associated ++ Running ++ Installed ++ fallback()),
            #{
                browser_only => false,
                candidates => Candidates,
                selected => machine_selection(Candidates)
            }
    end.

%% @doc Validate and save a machine-wide editor choice from the UI.
%% Store only a candidate identifier, never an executable or arguments.
%% The dedicated config fragment is loaded by Zotonic on subsequent starts.
-spec save_preference(Editor, File) -> ok | {error, term()} when
    Editor :: binary(), File :: file:filename_all().
save_preference(Editor, File) when is_binary(Editor) ->
    case detect(File) of
        #{ browser_only := true } ->
            {error, browser_only};
        #{ candidates := Candidates } ->
            case Editor =:= <<"auto">> orelse lists:any(
                fun(C) -> maps:get(id, C) =:= Editor andalso maps:get(available, C) end,
                Candidates)
            of
                true -> save_machine_preference(Editor);
                false -> {error, choose_editor}
            end
    end.

%% @doc Atomically persist the editor identifier in the node config directory and apply it immediately.
save_machine_preference(Editor) ->
    case z_config_files:config_dir() of
        {ok, Dir} ->
            File = filename:join([Dir, "config.d", "zotonic-editor.config"]),
            Filename = ".zotonic-editor-" ++ integer_to_list(erlang:unique_integer([positive])),
            Temp = filename:join(filename:dirname(File), Filename),
            Data = io_lib:format(
                    "%% Managed by mod_development.\n~tp.~n",
                    [[{zotonic, [{development_editor_id, Editor}]}]]),
            case filelib:ensure_dir(File) of
                ok ->
                    Result = case file:write_file(Temp, Data) of
                        ok -> file:rename(Temp, File);
                        Error -> Error
                    end,
                    _ = file:delete(Temp),
                    case Result of
                        ok -> z_config:set(development_editor_id, Editor);
                        Error1 -> Error1
                    end;
                Error2 -> Error2
            end;
        Error -> Error
    end.

%% @doc Honor the stored editor choice, retaining an unavailable choice instead of silently falling back.
machine_selection(Candidates) ->
    case z_config:get(development_editor_id, <<"auto">>) of
        <<"auto">> ->
            select(Candidates);
        Id ->
            case lists:search(fun(C) -> maps:get(id, C) =:= Id end, Candidates) of
                {value, C} ->
                    C;
                false ->
                    #{
                        id => Id,
                        label => Id,
                        available => false,
                        reason => configuration
                    }
            end
    end.

%% @doc Select the sole candidate with the strongest evidence.
%% Lower rank means stronger evidence. Ambiguous ranks do not select arbitrarily.
-spec select(Candidates) -> map() | undefined when Candidates :: [map()].
select([]) ->
    undefined;
select(Candidates) ->
    Rank = lists:min([maps:get(rank, C) || C <- Candidates]),
    case [C || C <- Candidates, maps:get(rank, C) =:= Rank] of
        [C] -> C;
        _ -> undefined
    end.

%% @doc Keep the first candidate for each editor identifier, preserving discovery precedence.
unique(Cs) ->
    {_, Result} = lists:foldl(
        fun(C, {Seen, Acc}) ->
            Id = maps:get(id, C),
            case lists:member(Id, Seen) of
                true -> {Seen, Acc};
                false -> {[Id | Seen], Acc ++ [C]}
            end
        end, {[], []}, Cs),
    Result.

%% @doc Shell entry points. Resolve source locations before using the same
%% launcher as the template viewer. Never launch an editor inside a container.
-spec edit(Target) -> ok | {error, term()} when
    Target :: module() | list() | binary().
edit(Target) ->
    edit(Target, 1).

%% @doc Open a shell target at a line or function, rejecting local launches in containers.
-spec edit(Target, Position) -> ok | {error, term()} when
    Target :: module() | list() | binary(),
    Position :: atom() | {atom(), non_neg_integer()} | pos_integer().
edit(Target, Position) ->
    case browser_only() of
        true ->
            {error, browser_only};
        false ->
            case edit_location(Target, Position) of
                {ok, File, Line} -> open(File, Line, 1, <<"auto">>);
                {error, _} = Error -> Error
            end
    end.

%% @doc Resolve a shell target to a file and line, using the first frame for stack traces.
edit_location([{Module, Function, Args, Info} | _], _) when
        is_atom(Module), is_atom(Function), is_list(Info) ->
    case proplists:get_value(line, Info) of
        Line when is_integer(Line), Line > 0 ->
            case proplists:get_value(file, Info) of
                undefined ->
                    edit_location(Module, Line);
                File ->
                    case filelib:is_regular(File) of
                        true -> {ok, File, Line};
                        false -> edit_location(Module, Line)
                    end
            end;
        _ ->
            edit_location(Module, {Function, stack_arity(Args)})
    end;
edit_location([{Module, Function, Args} | _], Position) ->
    edit_location([{Module, Function, Args, []}], Position);
edit_location(Module, Line) when is_atom(Module), is_integer(Line), Line > 0 ->
    case module_source(Module) of
        {ok, File} -> {ok, File, Line};
        Error -> Error
    end;
edit_location(Module, Function) when is_atom(Module), is_atom(Function) ->
    function_location(Module, Function, any);
edit_location(Module, {Function, Arity}) when
        is_atom(Module), is_atom(Function), is_integer(Arity), Arity >= 0 ->
    function_location(Module, Function, Arity);
edit_location(File, Line) when (is_binary(File) orelse is_list(File)),
                             is_integer(Line), Line > 0, File =/= [], File =/= <<>> ->
    case filelib:is_regular(File) of
        true -> {ok, File, Line};
        false -> indexed_file(File, Line)
    end;
edit_location(_, _) -> {error, badarg}.

%% @doc Accept either stack-frame arguments or an already recorded arity.
stack_arity(Args) when is_list(Args) -> length(Args);
stack_arity(Arity) -> Arity.

%% @doc Load a module if needed and read its source filename from compiler metadata.
module_source(Module) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            case proplists:get_value(source, Module:module_info(compile)) of
                undefined -> {error, no_source};
                File -> {ok, File}
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Resolve a module's source without launching an editor.
-spec source_location(Module) -> {ok, file:filename_all(), pos_integer()} | {error, term()} when
    Module :: atom().
source_location(Module) ->
    case module_source(Module) of
        {ok, File} ->
            case filelib:is_regular(File) of
                true -> {ok, filename:absname(File), 1};
                false -> {error, enoent}
            end;
        Error ->
            Error
    end.

%% @doc Resolve a source function without launching an editor (also in containers).
-spec source_location(Module, Function) -> {ok, file:filename_all(), pos_integer()} | {error, term()} when
    Module :: atom(), Function :: atom().
source_location(Module, Function) ->
    case function_location(Module, Function, any) of
        {ok, File, Line} ->
            case filelib:is_regular(File) of
                true -> {ok, filename:absname(File), Line};
                false -> {error, enoent}
            end;
        Error ->
            Error
    end.

%% @doc Find a function definition in BEAM abstract code, optionally matching a specific arity.
function_location(Module, Function, Arity) ->
    case module_source(Module) of
        {ok, Source} ->
            case code:get_object_code(Module) of
                {Module, Beam, _} ->
                    case beam_lib:chunks(Beam, [abstract_code]) of
                        {ok, {Module, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
                            function_form(Forms, Function, Arity, Source);
                        _ -> {error, no_debug_info}
                    end;
                error ->
                    {error, no_debug_info}
            end;
        Error ->
            Error
    end.

%% @doc Scan abstract forms for a function, tracking file attributes for included source files.
function_form([{attribute, _, file, {File, _}} | Rest], Function, Arity, _) ->
    function_form(Rest, Function, Arity, File);
function_form([{function, Anno, Function, A, _} | _], Function, Arity, File)
        when Arity =:= any; Arity =:= A ->
    {ok, File, erl_anno:line(Anno)};
function_form([_ | Rest], Function, Arity, File) ->
    function_form(Rest, Function, Arity, File);
function_form([], _, _, _) ->
    {error, function_not_found}.

%% @doc Resolve a library or template name across running sites; reject missing or ambiguous matches.
indexed_file(Name, Line) ->
    Files = lists:usort([
        z_convert:to_binary(File)
        || Context <- z_sites_manager:get_site_contexts(),
           Type <- [lib, template],
           {ok, #module_index{filepath = File}} <- [z_module_indexer:find(Type, Name, Context)]
    ]),
    case Files of
        [File] -> {ok, File, Line};
        [] -> {error, enoent};
        _ -> {error, {ambiguous, Files}}
    end.

%% @doc Open an existing file at a position using a detected editor or the saved automatic choice.
%% Refuse local launches in browser-only environments.
-spec open(File, Line, Column, Editor) -> ok | {error, term()} when
    File :: file:filename_all(), Line :: pos_integer(), Column :: pos_integer(), Editor :: binary().
open(File, Line, Column, Editor) when is_integer(Line), Line > 0,
                                    is_integer(Column), Column > 0 ->
    case browser_only() of
        true ->
            {error, browser_only};
        false ->
            case filelib:is_regular(File) of
                false -> {error, enoent};
                true ->
                    Detection = detect(File),
                    Candidate = case Editor of
                        <<"auto">> ->
                            maps:get(selected, Detection);
                        _ ->
                            lists:search(
                                fun(C) -> maps:get(id, C) =:= Editor end,
                                maps:get(candidates, Detection))
                    end,
                    launch(Candidate, filename:absname(File), Line, Column)
            end
    end.

%% @doc Run the selected editor in a separate process and report launch errors to the caller.
%% Direct Neovim clients receive a second command to position the cursor.
launch({value, C}, File, Line, Column) ->
    launch(C, File, Line, Column);
launch(C = #{available := true, executable := Exe}, File, Line, Column) ->
    %% The short-lived launcher is owned by a separate process, never the request.
    Parent = self(),
    Ref = make_ref(),
    z_proc:spawn_md(
        fun() ->
            Result = run(Exe, arguments(C, File, Line, Column), 5000),
            Final = case {Result, maps:get(id, C)} of
                {{ok, _}, <<"nvim">>} ->
                    [ServerFlag, Server | _] = maps:get(args, C),
                    Expr = lists:flatten(io_lib:format("cursor(~B,~B)", [Line, Column])),
                    run(Exe, [ServerFlag, Server, "--remote-expr", Expr], 800);
                _ -> Result
            end,
            Parent ! {Ref, Final}
        end),
    receive
        {Ref, {ok, _}} -> ok;
        {Ref, Error} -> Error
    after 6500 -> {error, timeout}
    end;
launch(#{available := false}, _, _, _) ->
    {error, editor_unavailable};
launch(_, _, _, _) ->
    {error, choose_editor}.

%% @doc Expand file and position placeholders within separate executable arguments, without shell parsing.
-spec arguments(Candidate, File, Line, Column) -> [string()] when
    Candidate :: map(), File :: file:filename_all(), Line :: pos_integer(), Column :: pos_integer().
arguments(#{args := Args}, File, Line, Column) ->
    Replacements = [
        {"{file}", unicode:characters_to_list(File)},
        {"{line}", integer_to_list(Line)}, {"{column}", integer_to_list(Column)}
    ],
    [lists:append([proplists:get_value(Part, Replacements, Part)
                   || Part <- re:split(Arg, "(\\{file\\}|\\{line\\}|\\{column\\})", [{return, list}, unicode])])
     || Arg <- Args].

%% @doc Find installed GUI editor launchers without starting any subprocesses.
installed() ->
    lists:filtermap(
        fun({Id, Label, Names, Mac, Args}) ->
            case installed_launcher(Names, Mac, Args) of
                false ->
                    false;
                {Exe, LaunchArgs} ->
                    {true, #{
                        id => Id,
                        label => Label,
                        executable => Exe,
                        args => LaunchArgs,
                        available => true,
                        reason => installed,
                        rank => 6
                    }}
            end
        end, definitions()).

%% @doc Resolve a CLI launcher or a macOS application opened through Launch Services.
%% Application launching reuses an existing instance, but cannot select a line or column.
installed_launcher([], {application, Paths}, Args) ->
    case [P || P <- mac_paths(Paths), filelib:is_dir(P)] of
        [App | _] -> {"/usr/bin/open", ["-a", App | Args]};
        [] -> false
    end;
installed_launcher(Names, Mac, Args) ->
    case executable(Names ++ mac_paths(Mac)) of
        false -> false;
        Exe -> {Exe, Args}
    end.

%% @doc Define supported GUI editors, launcher locations and file-position argument formats.
definitions() ->
    [
        {<<"sublime">>, <<"Sublime Text">>, ["subl"],
            ["Sublime Text.app/Contents/SharedSupport/bin/subl"], ["{file}:{line}:{column}"]},
        {<<"vscode">>, <<"Visual Studio Code">>, ["code"],
            ["Visual Studio Code.app/Contents/Resources/app/bin/code"], ["--reuse-window", "--goto", "{file}:{line}:{column}"]},
        {<<"vscodium">>, <<"VSCodium">>, ["codium"],
            ["VSCodium.app/Contents/Resources/app/bin/codium"], ["--reuse-window", "--goto", "{file}:{line}:{column}"]},
        {<<"zed">>, <<"Zed">>, ["zed"],
            ["Zed.app/Contents/MacOS/cli"], ["{file}:{line}:{column}"]},
        {<<"bbedit">>, <<"BBEdit">>, ["bbedit"],
            ["BBEdit.app/Contents/Helpers/bbedit_tool"], ["+{line}", "{file}"]},
        {<<"textmate">>, <<"TextMate">>, ["mate"],
            ["TextMate.app/Contents/Resources/mate"], ["--line", "{line}:{column}", "{file}"]},
        {<<"emacs">>, <<"Emacs">>, [],
            {application, ["Emacs.app"]}, ["{file}"]},
        {<<"gvim">>, <<"GVim / MacVim">>, ["mvim", "gvim"],
            ["MacVim.app/Contents/bin/mvim"], ["--remote", "+{line}", "{file}"]}
    ].

%% @doc Expand application-relative launcher paths in system and user macOS Applications directories.
mac_paths(Paths) ->
    case os:type() of
        {unix, darwin} ->
            Home = case env("HOME") of false -> "/nonexistent"; H -> H end,
            [filename:join(Root, P) || Root <- ["/Applications", filename:join(Home, "Applications")], P <- Paths];
        _ ->
            []
    end.

%% @doc Find the first available launcher from executable names or absolute paths.
executable([]) ->
    false;
executable([Name | Rest]) ->
    case os:find_executable(Name) of
        false -> executable(Rest);
        Path -> Path
    end.

%% @doc Read an environment variable, treating an empty value as unset.
env(Name) -> case os:getenv(Name) of "" -> false; Value -> Value end.

%% @doc Prefer trusted application configuration, otherwise inspect editor environment variables.
preference(Installed, Sessions) ->
    case z_config:get(development_editor, undefined) of
        undefined ->
            preference_env(["ZOTONIC_EDITOR", "VISUAL", "EDITOR"], Installed, Sessions);
        {Exe, Args} when is_list(Args) ->
            [configured([command_text(Exe) | [command_text(A) || A <- Args]], configuration, Installed, Sessions)];
        _ ->
            [unavailable(configuration)]
    end.

%% @doc Decode UTF-8 command binaries while preserving existing scalar configuration conversions.
command_text(Text) when is_binary(Text) ->
    unicode:characters_to_list(Text);
command_text(Text) ->
    z_convert:to_list(Text).

%% @doc Use the first set editor variable; retain invalid preferences as unavailable candidates.
preference_env([], _, _) ->
    [];
preference_env([Name | Rest], Installed, Sessions) ->
    case env(Name) of
        false -> preference_env(Rest, Installed, Sessions);
        Value ->
            case split_command(Value) of
                {ok, [_ | _] = Words} -> [configured(Words, list_to_binary(Name), Installed, Sessions)];
                _ -> [unavailable(list_to_binary(Name))]
            end
    end.

%% @doc Represent an explicit preference that cannot be launched safely or is not installed.
unavailable(Reason) -> #{id => <<"configured">>, label => <<"Configured editor">>,
                         available => false, rank => 1, reason => Reason}.

%% @doc Convert an explicit command to a candidate, reusing known editor sessions when possible.
%% Preserve supplied arguments and reject terminal editors without a usable remote client.
configured([Name | Extra], Reason, Installed, Sessions) ->
    Base = filename:basename(Name, ".exe"),
    Match = [C || C <- Sessions ++ Installed,
                  (filename:basename(maps:get(executable, C), ".exe") =:= Base
                   orelse (Base =:= "nvim" andalso maps:get(id, C) =:= <<"nvr">>))],
    case {executable([Name]), Match, Extra} of
        {_, [C | _], []} ->
            C#{
                rank => 1,
                reason => Reason
            };
        {false, _, _} ->
            unavailable(Reason);
        {_, [], _} when Base =:= "nvr" ->
            unavailable(Reason);
        {Exe, _, _} ->
            Terminal = lists:member(Base, ["vi", "vim", "nvim", "nano", "emacs"]),
            HasFile = lists:any(fun(A) -> string:find(A, "{file}") =/= nomatch end, Extra),
            %% Explicit arguments must not be overridden by an inferred server
            %% address or window option. Use placeholders for custom positions.
            Args0 = case HasFile of true -> Extra; false -> Extra ++ ["{file}"] end,
            Args = case Base of "nvr" -> ["--nostart" | Args0]; _ -> Args0 end,
            #{
                id => <<"configured">>,
                label => unicode:characters_to_binary(Name),
                executable => Exe,
                args => Args,
                available => not Terminal,
                rank => 1,
                reason => Reason
            }
    end.

%% @doc Parse shell-style quoting only. No expansion, substitution, or shell execution.
-spec split_command(Command) -> {ok, [string()]} | {error, invalid_command} when Command :: string().
split_command(Command) ->
    words(Command, none, [], [], false).

%% @doc Tokenize a configured command while tracking quotes, escapes and empty quoted arguments.
words([], none, Word, Acc, Started) -> {ok, lists:reverse(finish_word(Word, Acc, Started))};
words([], _, _, _, _) -> {error, invalid_command};
words([$\\, C | Rest], double, Word, Acc, _) when C =/= $" , C =/= $\\ ->
    words(Rest, double, [C, $\\ | Word], Acc, true);
words([$\\, C | Rest], Quote, Word, Acc, _) when Quote =/= single -> words(Rest, Quote, [C | Word], Acc, true);
words([$' | Rest], none, Word, Acc, _) -> words(Rest, single, Word, Acc, true);
words([$' | Rest], single, Word, Acc, _) -> words(Rest, none, Word, Acc, true);
words([$" | Rest], none, Word, Acc, _) -> words(Rest, double, Word, Acc, true);
words([$" | Rest], double, Word, Acc, _) -> words(Rest, none, Word, Acc, true);
words([C | Rest], none, Word, Acc, Started) when C =:= $ ; C =:= $\t ->
    words(Rest, none, [], finish_word(Word, Acc, Started), false);
words([C | Rest], Quote, Word, Acc, _) -> words(Rest, Quote, [C | Word], Acc, true).

%% @doc Append a completed token, preserving explicitly quoted empty arguments.
finish_word(_, Acc, false) -> Acc;
finish_word(Word, Acc, true) -> [lists:reverse(Word) | Acc].

%% @doc Probe inherited Vim/Neovim servers and available remote clients for existing editor sessions.
sessions(Installed) ->
    Nvr = nvr_sessions(),
    Emacs = emacs_session(),
    Direct = lists:filtermap(
        fun({Var, Id, Names, Probe, Args}) ->
            case {env(Var), executable(Names)} of
                {false, _} ->
                    false;
                {_, false} ->
                    false;
                {Server, Exe} ->
                    Prefix = case Id of <<"nvim">> -> ["--server", Server]; _ -> ["--servername", Server] end,
                    case run(Exe, Prefix ++ Probe, 800) of
                        {ok, _} ->
                            {true, #{
                                id => Id,
                                label => list_to_binary(hd(Names)),
                                executable => Exe,
                                args => Prefix ++ Args,
                                available => true,
                                rank => 3,
                                reason => inherited_session
                            }};
                        _ ->
                            false
                    end
            end
        end, [
            {"NVIM", <<"nvim">>, ["nvim"], ["--remote-expr", "1"], ["--remote", "{file}"]},
            {"VIM_SERVERNAME", <<"vim">>, ["vim"] ++ [maps:get(executable, C) || C <- Installed, maps:get(id, C) =:= <<"gvim">>],
               ["--remote-expr", "1"], ["--remote", "+call cursor({line},{column})", "{file}"]}
        ]),
    case Nvr of
        [#{rank := 3}] -> Emacs ++ Nvr ++ [C || C <- Direct, maps:get(id, C) =/= <<"nvim">>];
        _ -> Emacs ++ Nvr ++ Direct
    end.

%% @doc Find a responding Emacs server without allowing emacsclient to start a fallback editor.
emacs_session() ->
    case executable(["emacsclient"] ++ mac_paths(["Emacs.app/Contents/MacOS/bin/emacsclient"])) of
        false -> [];
        Exe ->
            Args = ["--alternate-editor=false"] ++
                case env("EMACS_SERVER_FILE") of false -> []; Server -> ["--server-file", Server] end,
            case run(Exe, Args ++ ["--eval", "t"], 800) of
                {ok, <<"t">>} ->
                    [
                        #{
                            id => <<"emacsclient">>,
                            label => <<"Emacs (client)">>,
                            executable => Exe,
                            args => Args ++ ["--no-wait", "+{line}:{column}", "{file}"],
                            available => true,
                            rank => 5,
                            reason => running
                        }
                    ];
                _ ->
                    []
            end
    end.

%% @doc Find a single responsive Neovim server through nvr.
%% Never start a new terminal editor during detection or a web request.
nvr_sessions() ->
    case executable(["nvr"]) of
        false -> [];
        Exe ->
            {Addresses, Rank, Reason} = case {env("NVIM"), env("NVIM_LISTEN_ADDRESS")} of
                {Address, _} when Address =/= false -> {[Address], 3, inherited_session};
                {_, Address} when Address =/= false -> {[Address], 3, inherited_session};
                _ ->
                    case run(Exe, ["--serverlist"], 800) of
                        {ok, Data} -> {[unicode:characters_to_list(A) || A <- binary:split(Data, <<"\n">>, [global]), A =/= <<>>], 5, running};
                        _ -> {[], 5, running}
                    end
            end,
            case Addresses of
                [Server|_] ->
                    Prefix = ["--nostart", "--servername", Server],
                    case run(Exe, Prefix ++ ["--remote-expr", "1"], 800) of
                        {ok, <<"1">>} ->
                            [
                                #{
                                    id => <<"nvr">>,
                                    label => <<"Neovim (nvr)">>,
                                    executable => Exe,
                                    args => Prefix ++ ["--remote", "+call cursor({line},{column})", "{file}"],
                                    available => true,
                                    rank => Rank,
                                    reason => Reason
                                }];
                        _ ->
                            []
                    end;
                [] ->
                    []
            end
    end.

%% @doc Offer the operating system file opener when no stronger editor evidence is available.
fallback() ->
    {Name, Args} = case os:type() of
        {unix, darwin} -> {"open", ["{file}"]};
        {unix, _} -> {"xdg-open", ["{file}"]};
        {win32, _} -> {"rundll32.exe", ["url.dll,FileProtocolHandler", "{file}"]}
    end,
    case executable([Name]) of
        false ->
            [];
        Exe ->
            [
                #{
                    id => <<"system">>,
                    label => <<"System default application">>,
                    executable => Exe,
                    args => Args,
                    available => true,
                    rank => 7,
                    reason => system_default
                }
            ]
    end.

%% @doc Match installed editors against associations for the requested file, Erlang source and CSS.
association(File, Installed) ->
    %% Check real Erlang and CSS files as well: .tpl is often associated with Word.
    ErlangFile = proplists:get_value(source, ?MODULE:module_info(compile)),
    CssFile = case code:priv_dir(zotonic_mod_development) of
        {error, _} -> undefined;
        PrivDir -> filename:join([PrivDir, "lib", "css", "development.css"])
    end,
    Files = lists:usort([F || F <- [File, ErlangFile, CssFile], F =/= undefined]),
    Apps = [App || F <- Files, {ok, App} <- [associated_application(F)]],
    [
        C#{
            rank => 4,
            reason => file_association
        } || C <- Installed,
             lists:any(fun(App) -> identifies(App, C) end, Apps)
    ].

%% @doc Query the operating system for the application associated with a file.
associated_application(File) ->
    case os:type() of
        {unix, darwin} ->
            %% The filename is an argument, never interpolated into source code.
            Script = "ObjC.import('AppKit'); function run(a) { var u = $.NSURL.fileURLWithPath($(a[0])); var p = $.NSWorkspace.sharedWorkspace.URLForApplicationToOpenURL(u); return p ? ObjC.unwrap(p.path) : ''; }",
            run_name("osascript", ["-l", "JavaScript", "-e", Script, unicode:characters_to_list(File)], 2000);
        {unix, _} ->
            case run_name("xdg-mime", ["query", "filetype", unicode:characters_to_list(File)], 1000) of
                {ok, Type} when Type =/= <<>> -> run_name("xdg-mime", ["query", "default", unicode:characters_to_list(string:trim(Type))], 1000);
                Error -> Error
            end;
        {win32, _} ->
            %% Only fixed extensions enter the PowerShell source; file paths never do.
            case filename:extension(unicode:characters_to_list(File)) of
                ".tpl" -> windows_association(".tpl");
                ".erl" -> windows_association(".erl");
                ".css" -> windows_association(".css");
                _ -> {error, unsupported}
            end
    end.

%% @doc Read the Windows user or class association for a supported file extension.
windows_association(Extension) ->
    Script = "$e='" ++ Extension ++ "'; "
             "$p=(Get-ItemProperty -LiteralPath ('HKCU:\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\FileExts\\'+$e+'\\UserChoice') -ErrorAction SilentlyContinue).ProgId; "
             "if (!$p) {$p=(Get-Item -LiteralPath ('Registry::HKEY_CLASSES_ROOT\\'+$e) -ErrorAction SilentlyContinue).GetValue('')}; $p",
    run_name("powershell.exe", ["-NoProfile", "-NonInteractive", "-Command", Script], 2000).

%% @doc Match an operating system application identifier to a known editor candidate.
identifies(App, #{id := Id}) ->
    Lower = string:lowercase(App),
    Names = case Id of
        <<"sublime">> -> [<<"sublime">>];
        <<"vscode">> -> [<<"visual studio code">>, <<"code.desktop">>, <<"applications.code.exe">>];
        <<"vscodium">> -> [<<"vscodium">>, <<"codium">>];
        <<"gvim">> -> [<<"macvim">>, <<"gvim">>];
        _ -> [Id]
    end,
    lists:any(fun(Name) -> binary:match(Lower, Name) =/= nomatch end, Names).

%% @doc Find installed editors among processes owned by the Zotonic node’s OS user.
running(Installed) ->
    %% Scope process discovery to the node's OS user, not every desktop session.
    case run_name("id", ["-u"], 500) of
        {ok, Uid} ->
            case run_name("ps", ["-U", z_convert:to_list(string:trim(Uid)), "-o", "comm="], 1000) of
                {ok, Processes} ->
                    [
                        C#{
                            rank => 5,
                            reason => running
                        } || C <- Installed, lists:any(fun(P) -> process_matches(P, C) end, binary:split(Processes, <<"\n">>, [global]))
                    ];
                _ ->
                    []
            end;
        _ ->
            []
    end.

%% @doc Recognize editor process names, including names that differ from their CLI launcher.
process_matches(Process, #{id := <<"emacs">>}) ->
    string:lowercase(filename:basename(string:trim(Process))) =:= <<"emacs">>;
process_matches(Process, #{executable := Exe, label := Label}) ->
    Base = string:lowercase(filename:basename(string:trim(Process))),
    ExeBase = string:lowercase(unicode:characters_to_binary(filename:basename(Exe))),
    Base =:= ExeBase orelse Base =:= string:lowercase(Label)
    orelse (Base =:= <<"sublime_text">> andalso Label =:= <<"Sublime Text">>)
    orelse (Base =:= <<"code">> andalso Label =:= <<"Visual Studio Code">>).

%% @doc Locate a discovery command and execute it with a bounded timeout.
run_name(Name, Args, Timeout) ->
    case executable([Name]) of
        false -> {error, enoent};
        Exe -> run(Exe, Args, Timeout)
    end.

%% @doc Execute a program with bounded time and output. All arguments go directly to exec, never /bin/sh.
run(Exe, Args, Timeout) ->
    try
        open_port({spawn_executable, Exe}, [binary, exit_status, use_stdio, stderr_to_stdout, hide, {args, Args}])
    of
        Port ->
            collect(Port, erlang:monotonic_time(millisecond) + Timeout, <<>>)
    catch
        error:Reason -> {error, Reason}
    end.

%% @doc Collect process output until exit, enforcing the deadline and a one-megabyte output limit.
collect(Port, Deadline, Acc) ->
    Remaining = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {Port, {data, Data}} when byte_size(Acc) + byte_size(Data) =< 1048576 ->
            collect(Port, Deadline, <<Acc/binary, Data/binary>>);
        {Port, {data, _}} ->
            close_port(Port),
            {error, output_limit};
        {Port, {exit_status, 0}} ->
            {ok, string:trim(Acc)};
        {Port, {exit_status, Status}} ->
            {error, {exit_status, Status}}
    after
        Remaining ->
            close_port(Port),
            {error, timeout}
    end.

%% @doc Close a launcher port, tolerating a process that has already exited.
close_port(Port) ->
    try port_close(Port) of
        true -> ok
    catch
        error:badarg -> ok
    end.
