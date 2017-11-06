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
    map_change/2
]).

-include_lib("zotonic_notifier/include/zotonic_notifier.hrl").
-include_lib("zotonic_filehandler/include/zotonic_filehandler.hrl").

-spec map_change(zotonic_filehandler:verb(), Filename::binary()) ->
    {ok, [Action]} | {error, term()} | false
    when Action :: zotonic_filehandler:action().
map_change(Verb, Filename) ->
    {Application, What} = categorize(Filename),
    Ext = filename:extension(Filename),
    Root = filename:rootname(filename:basename(Filename)),
    Split = filename:split(Filename),
    map_categorized(Verb, Application, What, Ext, Root, Split, Filename).

map_categorized(Verb, Application, What, Ext, Root, Split, Filename) ->
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
    case binary:match(Filename, <<"/test/">>) of
        {_, _} -> false;
        nomatch ->
            lager:info("Unhandled file event '~p' on '~s'", [Verb, Filename])
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
        {App, What} ->
            case is_application(App) of
                true ->
                    {binary_to_atom(App, utf8), What};
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
        {match, [Application]} ->
            {Application, config}
    end.

config_file(F) ->
    case re:run(F, "/([^/]+)/priv/zotonic_site.config", [{capture, all_but_first, binary}]) of
        nomatch ->
            false;
        {match, [Application]} ->
            {Application, config}
    end.

priv_file(F) ->
    case re:run(F, "/([^/]+)/priv/([^/]+)/(.*)", [{capture, all_but_first, binary}]) of
        nomatch ->
            false;
        {match, [Application, What, Path]} ->
            {Application, {priv, What, Path}}
    end.

src_file(F) ->
    case re:run(F, "/([^/]+)/src/(.*)", [{capture, all_but_first, binary}]) of
        nomatch ->
            false;
        {match, [Application, Path]} ->
            {Application, {src, Path}}
    end.

test_file(F) ->
    case re:run(F, "/([^/]+)/test/(.*)", [{capture, all_but_first, binary}]) of
        nomatch ->
            false;
        {match, [Application, Path]} ->
            {Application, {test, Path}}
    end.

include_file(F) ->
    case re:run(F, "/([^/]+)/include/(.*)", [{capture, all_but_first, binary}]) of
        nomatch ->
            false;
        {match, [Application, Path]} ->
            {Application, {src, Path}}
    end.

ebin_file(F) ->
    case re:run(F, "/([^/]+)/ebin/(.*)", [{capture, all_but_first, binary}]) of
        nomatch ->
            false;
        {match, [Application, EbinFile]} ->
            {Application, {ebin, EbinFile}}
    end.


%% @doc Check if the directory name is the name of a (compiled) application
is_application(AppName) when is_binary(AppName) ->
    AppNameS = unicode:characters_to_list(AppName),
    case code:where_is_file(AppNameS ++ ".app") of
        non_existing ->
            false;
        _Path ->
            true
    end.
