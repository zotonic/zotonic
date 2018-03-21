%% @doc Extend the filehandler with mappers for translations, templates and mediaclass.

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

-module(z_filehandler).

-export([
    start_observers/0,

    filehandler_mappers_observer/3,

    app_file_check/1,
    reindex_modules/0,
    reload_translations/0,
    reload_dispatch/0,
    mark_modified/1

]).

-include_lib("zotonic_notifier/include/zotonic_notifier.hrl").

-spec start_observers() -> ok.
start_observers() ->
    zotonic_notifier:observe(
        ?SYSTEM_NOTIFIER, zotonic_filehandler_mappers,
        {?MODULE, filehandler_mappers_observer, []},
        self(), ?NOTIFIER_DEFAULT_PRIORITY).

filehandler_mappers_observer(zotonic_filehandler_mappers, Mappers, _Ctx) ->
    [
        fun app_file/7
    ] ++ Mappers ++ [
        fun template_file/7,
        fun mediaclass_file/7,
        fun po_file/7,
        fun lib_file/7,
        fun dispatch_file/7
    ].

%% @doc Check for newly created/added Erlang applications
app_file(create, _Application, {app, _AppFile}, <<".app">>, _Root, _Split, Filename) ->
    {ok, [
        {?MODULE, app_file_check, [Filename]}
    ]};
app_file(delete, _Application, {app, _AppFile}, <<".app">>, _Root, _Split, Filename) ->
    {ok, [
        {?MODULE, app_file_check, [Filename]}
    ]};
app_file(_Verb, _Application, _What, _Ext, _Root, _Split, _Filename) ->
    false.


template_file(modify, _Application, {priv, <<"templates">>, _Path}, <<".tpl">>, _Root, _Split, Filename) ->
    {ok, [
        {?MODULE, mark_modified, [Filename]}
    ]};
template_file(_Verb, _Application, {priv, <<"templates">>, _Path}, <<".tpl">>, _Root, _Split, Filename) ->
    {ok, [
        {?MODULE, mark_modified, [Filename]},
        {?MODULE, reindex_modules, []}
    ]};
template_file(_Verb, _Application, _What, _Ext, _Root, _Split, _Filename) ->
    false.


mediaclass_file(modify, _Application, {priv, <<"templates">>, _Path}, <<".config">>, <<"mediaclass">>, _Split, Filename) ->
    {ok, [
        {?MODULE, mark_modified, [Filename]}
    ]};
mediaclass_file(_Verb, _Application, {priv, <<"templates">>, _Path}, <<".config">>, <<"mediaclass">>, _Split, Filename) ->
    {ok, [
        {?MODULE, mark_modified, [Filename]},
        {?MODULE, reindex_modules, []}
    ]};
mediaclass_file(_Verb, _Application, _What, _Ext, _Root, _Split, _Filename) ->
    false.


po_file(_Verb, _Application, {priv, <<"translations">>, _Path}, <<".po">>, _Root, _Split, _Filename) ->
    {ok, [
        {?MODULE, reload_translations, []}
    ]};
po_file(_Verb, _Application, _What, _Ext, _Root, _Split, _Filename) ->
    false.


lib_file(modify, _Application, {priv, <<"lib">>, _Path}, _Ext, _Root, _Split, Filename) ->
    {ok, [
        {?MODULE, mark_modified, [Filename]}
    ]};
lib_file(_Verb, _Application, {priv, <<"lib">>, _Path}, _Ext, _Root, _Split, Filename) ->
    {ok, [
        {?MODULE, mark_modified, [Filename]},
        {?MODULE, reindex_modules, []}
    ]};
lib_file(_Verb, _Application, _What, _Ext, _Root, _Split, _Filename) ->
    false.

dispatch_file(_Verb, _Application, {priv, <<"dispatch">>, _Path}, _Ext, _Root, _Split, Filename) ->
    {ok, [
        {?MODULE, mark_modified, [Filename]},
        {?MODULE, reload_dispatch, []}
    ]};
dispatch_file(_Verb, _Application, _What, _Ext, _Root, _Split, _Filename) ->
    false.


%% @doc Check if an app file is added, if so we need to tell the module managers and sites to upgrade
app_file_check(Filename) ->
    case zotonic_filehandler_compile:code_path_check(Filename) of
        true ->
            z_sites_manager:upgrade(),
            lists:foreach(
                fun(Ctx) ->
                    z_module_manager:upgrade(Ctx)
                end,
                z_sites_manager:get_site_contexts());
        false ->
            ok
    end.


%% @doc A Zotonic template changed
%% @todo This should be handled by a module indexer, which can make a small reindex of
%%       the affected application templates.
reindex_modules() ->
    zotonic_filehandler:terminal_notifier("Index modules."),
    lists:foreach(
        fun(Ctx) ->
            z_module_indexer:reindex(Ctx)
        end,
        z_sites_manager:get_site_contexts()).

%% @doc Reload translations
%% @todo This should be handled incrementally, passing the changed po file.
reload_translations() ->
    zotonic_filehandler:terminal_notifier("Load translations."),
    lists:foreach(
        fun(Ctx) ->
            z_trans_server:load_translations(Ctx)
        end,
        z_sites_manager:get_site_contexts()).

%% @doc Reload all dispatch rules
reload_dispatch() ->
    zotonic_filehandler:terminal_notifier("Load dispatch rules."),
    lists:foreach(
        fun(Ctx) ->
            z_dispatcher:reload(Ctx)
        end,
        z_sites_manager:get_site_contexts()).

%% @doc Mark a file as modified in the mtime index
mark_modified(Filename) ->
    z_file_mtime:modified(Filename),
    ok.
