%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017 Marc Worrell
%%
%% @doc Map file changes to actions (compile, load, etc.)

%% Copyright 2017 Marc Worrell
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

-module(zotonic_filehandler_mapper).

-export([
    map_change/2,
    is_application/1
]).

-include_lib("zotonic_notifier/include/zotonic_notifier.hrl").
-include_lib("../include/zotonic_filehandler.hrl").
-include_lib("kernel/include/logger.hrl").

% Filename patterns for which we don't about unhandled events.
-define(FILENAMES_NOWARN, <<"(/test/|/priv/ssl/|/dist/|\\.app$)">>).


-spec map_change(zotonic_filehandler:verb(), Filename::binary()) ->
    {ok, [ mfa() ]} | {error, term()} | false.
map_change(Verb, Filename) ->
    {Application, What} = categorize(Filename),
    Ext = filename:extension(Filename),
    Root = filename:rootname(filename:basename(Filename)),
    Split = filename:split(Filename),
    map_categorized(Verb, Application, What, Ext, Root, Split, Filename).

map_categorized(Verb, Application, What, Ext, Root, Split, Filename) ->
    maybe_flush_indexer(Verb, Application, What),
    case map_notifier(Verb, Application, What, Ext, Root, Split, Filename) of
        {ok, _Mappings} = Ok ->
            Ok;
        undefined ->
            Try = zotonic_filehandler_mappers:mappers(),
            map_categorized(Try, Verb, Application, What, Ext, Root, Split, Filename)
    end.

map_notifier(Verb, Application, What, Ext, Root, Split, Filename) ->
    Msg = #zotonic_filehandler_map{
        verb = Verb,
        application = Application,
        what = What,
        extension = Ext,
        root = Root,
        split = Split,
        filename = Filename
    },
    zotonic_notifier:first(
        ?SYSTEM_NOTIFIER, zotonic_filehandler_map,
        Msg, undefined).

map_categorized([], Verb, _Application, _What, _Ext, _Root, _Split, Filename) ->
    case re:run(Filename, ?FILENAMES_NOWARN) of
        {match, _} -> false;
        nomatch ->
            ?LOG_DEBUG(#{
                text => <<"Unhandled file event">>,
                in => zotonic_filehandler,
                verb => Verb,
                filename => Filename
            })
    end,
    false;
map_categorized([Fun|Other], Verb, Application, What, Ext, Root, Split, Filename) ->
    case Fun(Verb, Application, What, Ext, Root, Split, Filename) of
        {ok, _} = Ok ->
            Ok;
        ok ->
            {ok, []};
        {error, _} = Error ->
            Error;
        false ->
            map_categorized(Other, Verb, Application, What, Ext, Root, Split, Filename)
    end.

%% @doc Always flush the complete app if any file in the app is deleted or created.
maybe_flush_indexer(Verb, App, What) when Verb =:= delete; Verb =:= create ->
    case What of
        {src, _} -> zotonic_fileindexer_cache:flush(App, <<"src">>);
        {priv, Dir, _} -> zotonic_fileindexer_cache:flush(App, <<"priv/", Dir/binary>>);
        _ -> ok
    end;
maybe_flush_indexer(_Verb, _App, _What) ->
    ok.


categorize(Filename) ->
    case categorize_notifier(Filename) of
        {ok, {_,_} = Cat} ->
            Cat;
        undefined ->
            Try = [
                fun config_dir_file/1,
                fun config_file/1,
                fun priv_file/1,
                fun src_file/1,
                fun include_file/1,
                fun app_file/1,
                fun ebin_file/1,
                fun test_file/1
            ],
            categorize_1(Try, Filename)
    end.

categorize_notifier(Filename) ->
    Msg = #zotonic_filehandler_categorize{
        filename = Filename
    },
    zotonic_notifier:first(
        ?SYSTEM_NOTIFIER, zotonic_filehandler_categorize,
        Msg, undefined).

categorize_1([], _Filename) ->
    {undefined, undefined};
categorize_1([Fun|Other], Filename) ->
    case Fun(Filename) of
        {AppVsn, What} ->
            case is_application(AppVsn) of
                {true, AppName} ->
                    {AppName, What};
                false ->
                    categorize_1(Other, Filename)
            end;
        false ->
            categorize_1(Other, Filename)
    end.

config_dir_file(F) ->
    case re:run(F, "/([^/]+)/priv/config.d/", [{capture, all_but_first, binary}]) of
        nomatch ->
            false;
        {match, [AppVsn]} ->
            {AppVsn, config}
    end.

config_file(F) ->
    case re:run(F, "/([^/]+)/priv/zotonic_site.config", [{capture, all_but_first, binary}]) of
        nomatch ->
            false;
        {match, [AppVsn]} ->
            {AppVsn, config}
    end.

priv_file(F) ->
    case re:run(F, "/([^/]+)/priv/([^/]+)/(.*)", [{capture, all_but_first, binary}]) of
        nomatch ->
            false;
        {match, [AppVsn, What, Path]} ->
            {AppVsn, {priv, What, Path}}
    end.

src_file(F) ->
    case re:run(F, "/([^/]+)/src/(.*)", [{capture, all_but_first, binary}]) of
        nomatch ->
            false;
        {match, [AppVsn, Path]} ->
            case src_file(Path) of
                false ->
                    {AppVsn, {src, Path}};
                Match -> Match
            end
    end.

test_file(F) ->
    case re:run(F, "/([^/]+)/test/(.*)", [{capture, all_but_first, binary}]) of
        nomatch ->
            false;
        {match, [AppVsn, Path]} ->
            case test_file(Path) of
                false ->
                    {AppVsn, {src, Path}};
                Match -> Match
            end
    end.

include_file(F) ->
    case re:run(F, "/([^/]+)/include/(.*)", [{capture, all_but_first, binary}]) of
        nomatch ->
            false;
        {match, [AppVsn, Path]} ->
            case include_file(Path) of
                false ->
                    {AppVsn, {src, Path}};
                Match -> Match
            end
    end.

app_file(F) ->
    case re:run(F, "/([^/]+)/ebin/(.*\\.app)", [{capture, all_but_first, binary}]) of
        nomatch ->
            false;
        {match, [AppVsn, AppFile]} ->
            {AppVsn, {app, AppFile}}
    end.

ebin_file(F) ->
    case re:run(F, "/([^/]+)/ebin/(.*)", [{capture, all_but_first, binary}]) of
        nomatch ->
            false;
        {match, [AppVsn, EbinFile]} ->
            {AppVsn, {ebin, EbinFile}}
    end.


%% @doc Check if the directory name is the name of a (compiled) application
is_application(AppName) when is_atom(AppName) ->
    case code:lib_dir(AppName) of
        {error, bad_name} -> is_application__app(AppName, AppName);
        LibDir when is_list(LibDir) -> {true, AppName}
    end;
is_application(AppNameVsn) when is_binary(AppNameVsn) ->
    [AppName|_] = binary:split(AppNameVsn, <<"-">>),
    AppNameS = unicode:characters_to_list(AppName),
    case code:where_is_file(AppNameS ++ ".app") of
        non_existing -> is_application__app(AppNameVsn, AppName);
        _Path -> {true, z_convert:to_atom(AppName)}
    end.

is_application__app(AppNameVsn, AppName) ->
    case filelib:is_regular(
        filename:join([
            filename:dirname(code:lib_dir(zotonic_filehandler)),
            AppNameVsn,
            "ebin",
            z_convert:to_list(AppName)++".app"
        ]))
    of
        true -> {true, z_convert:to_atom(AppName)};
        false -> false
    end.
