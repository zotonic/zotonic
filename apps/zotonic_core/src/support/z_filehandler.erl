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
    Mappers ++ [
        fun template_file/7,
        fun mediaclass_file/7,
        fun po_file/7,
        fun lib_file/7,
        fun dispatch_file/7
    ].

template_file(_Verb, _Application, {priv, <<"template">>, _Path}, <<".tpl">>, _Root, _Split, Filename) ->
    {ok, [
        {?MODULE, mark_modified, [Filename]},
        {?MODULE, reindex_modules, []}
    ]};
template_file(_Verb, _Application, _What, _Ext, _Root, _Split, _Filename) ->
    false.


mediaclass_file(_Verb, _Application, {priv, <<"template">>, _Path}, <<".config">>, <<"mediaclass">>, _Split, Filename) ->
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
