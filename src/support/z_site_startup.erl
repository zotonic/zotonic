%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% Date: 2010-05-18
%% @doc This module is started after the complete site_sup has been booted. 
%% This is the moment for system wide initializations.

%% Copyright 2010 Marc Worrell
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

-record(state, {context}).

%% @spec start_link(SiteProps::proplist()) -> ignore
%% @doc Perform all site startup routines.
start_link(SiteProps) ->
    {host, Host} = proplists:lookup(host, SiteProps),
    Name = z_utils:name_for_host(?MODULE, Host),
    gen_server:start_link({local, Name}, ?MODULE, SiteProps, []).


init(SiteProps) ->
    {host, Host} = proplists:lookup(host, SiteProps),
    lager:md([
        {site, Host},
        {module, ?MODULE}
      ]),
    Context = z_context:new(Host),
    {ok, #state{context=Context}, 0}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    do_startup(State#state.context),
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

    case z_db:has_connection(Context) of
        true ->
            z_install_data:install_modules(Context),

            %% Make sure all modules are started
            z_module_manager:upgrade(Context),

            m_config:set_value(zotonic, version, ?ZOTONIC_VERSION, Context);

        false ->

            %% Make sure all modules are started
            z_module_manager:upgrade(Context)
    end.
