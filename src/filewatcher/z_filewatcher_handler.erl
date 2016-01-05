%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014-2015 Arjan Scherpenisse
%%
%% @doc Handle changed files

%% Copyright 2014-20115 Arjan Scherpenisse
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

-module(z_filewatcher_handler).
-author("Arjan Scherpenisse <arjan@miraclethings.nl>").

-export([
    file_changed/2,
    file_blacklisted/1,
    select_verb/2,
    set_timer/3,
    set_timer/4
    ]).

-type verb() :: create|modify|delete.

-include("zotonic.hrl").

%% Which files do we not consider at all in the file_changed handler
-define(FILENAME_BLACKLIST_RE, 
        "_flymake|\\.#|/sites/[^/]+/files/|/\\.git/|/\\.gitignore|\\.hg/").


%% @doc Called when a file is changed on disk. Decides what to do.
%% @spec file_changed(modify | create, string()) -> ok
file_changed(Verb, F) when is_binary(F) ->
    file_changed(Verb, z_convert:to_list(F));
file_changed(Verb, F) ->
    lager:debug("[filewatcher] ~p of ~p", [Verb, F]),
    case file_blacklisted(F) of
        true ->
            nop;
        false ->
            z_filewatcher_mtime:modified(F),
            Message = handle_file(
                            check_deleted(F, Verb), 
                            filename:basename(F), 
                            filename:extension(F),
                            F),
            send_message(Message)
    end,
    ok.

file_blacklisted(F) ->
    case re:run(F, ?FILENAME_BLACKLIST_RE) of
        {match, _} ->
             true;
        nomatch ->
            false
    end.

%% @doc Select the verb to be passed if there are multiple updates to a file.
-spec select_verb(NewVerb :: verb(), PrevVerb :: verb()|undefined) -> verb().
select_verb(Verb, undefined) -> Verb;
select_verb(delete, _Verb) -> delete;
select_verb(create, _Verb) -> create;
select_verb(modify, Verb) -> Verb.

check_deleted(F, delete) ->
    case filelib:is_file(F) of
        true -> create;
        false -> delete
    end;
check_deleted(F, Verb) ->
    case filelib:is_file(F) of
        true -> Verb;
        false -> delete
    end.

%% @doc Set a timer to prevent duplicate file changed message for the same filename
%% (e.g. if a editor saves a file twice for some reason).
-spec set_timer(string(), verb(), [{string(), timer:tref(), verb()}]) -> [{string(), timer:tref(), verb()}].
set_timer(Filename, Verb, Timers) ->
    set_timer(Filename, Verb, Timers, 0).

-spec set_timer(string(), verb(), [{string(), timer:tref(), verb()}], integer()) -> [{string(), timer:tref(), verb()}].
set_timer(Filename, Verb, Timers, Offset) ->
    {Timers1, OldVerb} = case lists:keytake(Filename, 1, Timers) of
                              {value, {Filename, TRef, PVerb}, Ts1} ->
                                  erlang:cancel_timer(TRef),
                                  {Ts1, PVerb};
                              false ->
                                  {Timers, undefined}
                         end,
    Verb1 = z_filewatcher_handler:select_verb(Verb, OldVerb),
    TRef2 = erlang:send_after(300+Offset, self(), {filechange, Verb1, Filename}),
    [{Filename, TRef2, Verb1} | Timers1].

%% @doc Recompile Erlang files on the fly
-spec handle_file(verb(), string(), string(), string()) -> string() | undefined.
handle_file(_Verb, _Basename, ".bea#", _F) ->
    undefined;

handle_file(_Verb, _Basename, ".beam", F) ->
    Module = z_convert:to_atom(filename:rootname(filename:basename(F))),
    zotonic_compile:ld(Module),
    "Reload module " ++ z_convert:to_list(Module);

handle_file(_Verb, _Basename, ".hrl", F) ->
    spawn(fun() ->
             zotonic_compile:all()
          end),
    "Rebuilding due to change header file: " ++ filename:basename(F);

handle_file(_Verb, "erlydtl_parser.yrl", ".yrl", F) ->
    TargetDir = filename:join(os:getenv("ZOTONIC"), "src"),
    os:cmd("erlc -o "++z_utils:os_escape(TargetDir)++" "++z_utils:os_escape(F)),
    "Rebuilding yecc file: " ++ filename:basename(F);

handle_file(_Verb, _Basename, ".erl", F) ->
    Libdir = z_utils:lib_dir(),
    L = length(Libdir),
    F2 = case string:substr(F, 1, L) of
             Libdir -> string:substr(F, L+2);
             _ -> F
         end,
    try
        make:files([F], zotonic_compile:compile_options()),
        "Recompile " ++ F2
    catch
        error:{badmatch, {error, enoent}} ->
            % File disappeared
            "Not found " ++ F2
    end;

%% @doc SCSS / SASS files from lib/scss -> lib/css
handle_file(_Verb, _Basename, SASS, F) when SASS =:= ".scss"; SASS =:= ".sass" ->
    InPath = filename:dirname(F),
    OutPath = filename:join(filename:dirname(InPath), "css"),
    case filelib:is_dir(OutPath) of
        true ->
            os:cmd("sass -C --sourcemap=none --update " ++ z_utils:os_escape(InPath) ++ ":" ++ z_utils:os_escape(OutPath));
        false ->
            undefined
    end;

%% @doc LESS from lib/less -> lib/css, unless a file 'config' in the file dir is present
handle_file(_Verb, _Basename, ".less", F) ->
    InPath = filename:dirname(F),
    case handle_config_command(InPath, ".less") of
        undefined -> 
            OutPath = filename:join(filename:dirname(InPath), "css"),
            case filelib:is_dir(OutPath) of
                true ->
                    OutFile = filename:join(OutPath, filename:basename(F, ".less")++".css"),
                    os:cmd("lessc " ++ z_utils:os_escape(F) ++ " > " ++ z_utils:os_escape(OutFile)),
                    "Compiled " ++ OutFile;
                false ->
                    undefined
            end;
        Result -> Result
    end;

%% @doc Coffeescript from lib/coffee -> lib/js
handle_file(_Verb, _Basename, ".coffee", F) ->
    InPath = filename:dirname(F),
    OutPath = filename:join(filename:dirname(InPath), "js"),
    case filelib:is_dir(OutPath) of
        true ->
            os:cmd("coffee -o " ++ z_utils:os_escape(OutPath) ++ " -c " ++ z_utils:os_escape(InPath)),
            "Compiled " ++ OutPath;
        false ->
            undefined
    end;

%% @doc Template files
handle_file(Verb, _Basename, ".tpl", F) when Verb =:= create; Verb =:= delete ->
    reindex_templates(F);
handle_file(Verb, "mediaclass.config", ".config", F) when Verb =:= create; Verb =:= delete ->
    reindex_templates(F);
handle_file(modify, "mediaclass.config", ".config", F) ->
    reindex_templates(F);

%% @doc Translations 
handle_file(_Verb, _Basename, ".po", F) ->
    case re:run(F, "/sites/([^/]+).*?/translations/(.*)", [{capture, all_but_first, list}]) of
        nomatch ->
            %% Flush the cache when a new zotonic-wide .po file is changed
            case re:run(F, ".*?/translations/(.*)", [{capture, all_but_first, list}]) of
                nomatch -> 
                    undefined;
                {match, [TranslationFile]} ->
                    z_sites_manager:foreach(
                                        fun(Ctx) ->
                                            z_trans_server:load_translations(Ctx)
                                        end),
                    "Reload translations of all sites due to: " ++ TranslationFile
            end;
        {match, [Site, TranslationFile]} ->
            %% Flush a site cache when a new .po file is changed within a site
            SiteAtom = list_to_atom(Site),
            case lists:member(SiteAtom, z_sites_manager:get_sites()) of
                true ->
                    Ctx = z_context:new(SiteAtom),
                    z_trans_server:load_translations(Ctx),
                    "Reload translations of " ++ Site ++ " due to: " ++ TranslationFile;
                false ->
                    undefined
            end
    end;

%% @doc Check for dispatch and lib files
handle_file(_Verb, _Basename, _Extension, F) ->
    case maybe_handle_lib(F) of
        undefined ->
            maybe_handle_dispatch(F);
        LibMsg ->
            LibMsg
    end.

%% @doc Check for config file on path and read proplist values from, to, params, return as tuple. Path values are local to Path.
config_command(Path) ->
    ConfigFile = filename:join(Path, "config"),
    case filelib:is_regular(ConfigFile) of 
        true -> 
            {ok, C} = file:consult(ConfigFile),
            [Config] = C,
            From = proplists:get_value(from, Config),
            To = proplists:get_value(to, Config),
            Params = proplists:get_value(params, Config, ""),
            case {From, To, Params} of
                {undefined, _, _} -> {};
                {_, undefined, _} -> {};
                _ -> {From, To, Params}
            end;
        false -> {}
    end.

%% @doc Use config variables 'from', 'to', 'params' and run the lessc command from the path directory.
handle_config_command(Path, ".less") ->
    case config_command(Path) of
        {} -> undefined;
        {From, To, Params} ->
            Cmd = "cd " ++ z_utils:os_escape(Path) ++ "; lessc " ++ Params ++ " " ++ z_utils:os_escape(filename:join(Path, From)) ++ " " ++ z_utils:os_escape(filename:join(Path, To)),
            os:cmd(Cmd),
            "Compiled " ++ To
    end.
 
maybe_handle_lib(F) ->
    case re:run(F, "^.*/lib/(.*)") of
        nomatch ->
            undefined;
        {match, _} ->
            z_sites_manager:foreach(
                                fun(Ctx) ->
                                    z_module_indexer:reindex(Ctx)
                                end),
            "Reindexed sites due to changed lib file."
    end.

maybe_handle_dispatch(F) ->
    case re:run(F, "^.*/dispatch/(.*)") of
        nomatch ->
            undefined;
        {match, _} ->
            z_sites_manager:foreach(
                                fun(Ctx) ->
                                    z_dispatcher:reload(Ctx)
                                end),
            z_sites_dispatcher:update_dispatchinfo(),
            "Reindex sites due to file change: " ++ F
    end.


reindex_templates(F) ->
    case re:run(F, "/sites/([^/]+).*?/templates/(.*)", [{capture, all_but_first, list}]) of
        nomatch ->
            %% Flush the cache when a new zotonic-wide .tpl file is used
            case re:run(F, ".*?/templates/(.*)", [{capture, all_but_first, list}]) of
                nomatch -> 
                    undefined;
                {match, [TemplateFile]} ->
                    z_sites_manager:foreach(
                                        fun(Ctx) ->
                                            z_module_indexer:reindex(Ctx)
                                        end),
                    "Reindexed sites due to template: " ++ TemplateFile
            end;
        {match, [Site, TemplateFile]} ->
            SiteAtom = list_to_atom(Site),
            case lists:member(SiteAtom, z_sites_manager:get_sites()) of
                true ->
                    Ctx = z_context:new(SiteAtom),
                    z_module_indexer:reindex(Ctx),
                    "Reindex " ++ Site ++ " due to template: " ++ TemplateFile;
                false ->
                    undefined
            end
    end.

%% @doc Send a message to the user as a system notification
send_message(undefined) ->
    undefined;
send_message("") ->
    undefined;
send_message(Message) ->
    send_message(os:type(), z_string:trim(Message)).

%% @doc send message to the user
send_message(_OS, "") ->
    undefined;
send_message({unix, darwin}, Msg) ->
    os:cmd("which terminal-notifier && terminal-notifier -title Zotonic  -message " ++ z_utils:os_escape(Msg));
send_message({unix, _Arch}, Msg) ->
    os:cmd("which notify-send && notify-send \"Zotonic\" " ++ z_utils:os_escape(Msg));
send_message(_OS, _Msg) ->
    undefined.
