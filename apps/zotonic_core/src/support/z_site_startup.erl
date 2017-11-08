%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2017 Marc Worrell
%% @doc This module is started after the complete site_sup has been booted.
%% This is the moment for system wide initializations.

%% Copyright 2010-2017 Marc Worrell
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

-include_lib("zotonic.hrl").
-include("zotonic_release.hrl").

-record(state, { site :: atom() }).

%% @doc Perform all site startup routines.
-spec start_link(Site :: atom()) -> {ok, pid()} | {error, term()}.
start_link(Site) ->
    Name = z_utils:name_for_site(?MODULE, Site),
    gen_server:start_link({local, Name}, ?MODULE, Site, []).


init(Site) ->
    lager:md([
        {site, Site},
        {module, ?MODULE}
      ]),
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

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


do_startup(Context) ->
    do_startup(z_db:has_connection(Context), Context),
    z_module_manager:upgrade_await(Context),
    z_sites_manager:set_site_status(z_context:site(Context), running).

do_startup(true, Context) ->
    z_install_data:install_modules(Context),
    m_config:set_value(zotonic, version, ?ZOTONIC_VERSION, Context);
do_startup(false, _Context) ->
    ok.
