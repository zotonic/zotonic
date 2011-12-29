%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2011 Marc Worrell
%% @doc zynamo application

%% Copyright 2010-2011 Marc Worrell
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

%% The hash ring of a zotonic system consists of nodes that are responsible for a part of the ring.
%% Keys are hashed to buckets and the buckets have a primary node that is responsible for storing
%% values hashed to the bucket.
%% 
%% When a node is added it will claim a range from the ring.  This range is calculated depending on 
%% the number of nodes in the ring.  All nodes have an equal weight.

-module(zynamo).
-author("Marc Worrell <marc@worrell.nl>").

-behaviour(application).

-export([
    start/0,
    start/2,
    stop/1
]).

start() ->
    check(),
    ensure_started(crypto),
    application:start(zynamo).

start(_StartType, _StartArgs) ->
    case zynamo_sup:start_link() of
        {ok, Pid} -> 
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.


%% @doc Ensure that the give application is started. Crash when it can't be started.
ensure_started(App) ->
    case application:start(App) of
    ok ->
        ok;
    {error, {already_started, App}} ->
        ok
    end.


-spec check() -> ok.
check() ->
    % Check if the node name is correct for Zynamo
    zynamo_hash:node_id(),
    ok.


