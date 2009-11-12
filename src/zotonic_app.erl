%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Callbacks for the zotonic application.

%% Copyright 2009 Marc Worrell
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

-module(zotonic_app).
-author('Marc Worrell <marc@worrell.nl>').

-behaviour(application).
-export([start/2,stop/1]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for zotonic.
start(_Type, _StartArgs) ->
    ensure_started(crypto),
    ensure_started(ssl),
    zotonic_deps:ensure(),
    zotonic_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for zotonic.
stop(_State) ->
    ok.
