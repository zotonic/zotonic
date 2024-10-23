%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2022 Marc Worrell
%% @doc This module is started after the complete site_sup has been booted.
%% This is the moment for system wide initializations.

%% Copyright 2010-2022 Marc Worrell
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

-module(z_site_startup).
-author("Marc Worrell <marc@worrell.nl>").

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

-include("../../include/zotonic_release.hrl").
-include("../../include/zotonic.hrl").

-record(state, { site :: atom() }).

%% @doc Perform all site startup routines.
-spec start_link(Site :: atom()) -> {ok, pid()} | {error, term()}.
start_link(Site) ->
    Name = z_utils:name_for_site(?MODULE, Site),
    gen_server:start_link({local, Name}, ?MODULE, Site, []).


init(Site) ->
    logger:set_process_metadata(#{
        site => Site,
        module => ?MODULE
    }),
    {ok, #state{ site = Site }, 0}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, #state{ site = Site } = State) ->
    do_startup(z_context:new(Site)),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


do_startup(Context) ->
    z_proc:spawn_link_md(
        fun() ->
            z_notifier:await(module_ready, 60000, Context),
            ?zInfo("Site started, modules loaded", Context),
            case z_db:has_connection(Context) of
                true -> m_config:set_value(zotonic, version, ?ZOTONIC_VERSION, Context);
                false -> ok
            end
        end),
    z_language:initialize_config(Context),
    do_install_modules(z_db:has_connection(Context), Context),
    z_depcache:flush(Context),
    z_module_manager:upgrade_await(Context),
    z_sites_manager:set_site_status(z_context:site(Context), running).

do_install_modules(true, Context) ->
    Site = z_context:site(Context),
    {ok, Config} = z_sites_manager:get_site_config(Site),
    Modules = [ Site | proplists:get_value(modules, Config, []) ],
    lists:foreach(
        fun(M) ->
            install_module(M, Context)
        end,
        Modules);
do_install_modules(false, _Context) ->
    ok.

install_module(M, Context) when is_atom(M); is_binary(M); is_list(M) ->
    {ok, 1} = z_db:equery("
        insert into module (name, is_active)
        values ($1, true)
        on conflict (name) do update
        set is_active = true
        ", [M], Context).
