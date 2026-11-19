%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Shared IP-rule lookups and independent expiry cleanup.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(z_traffic_guard_rules).
-behaviour(gen_server).
-export([start_link/1, replace/2, lookup/2, prune/1, invalidate/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link(Args) ->
    C=proplists:get_value(context,Args),
    gen_server:start_link({local,name(C)},?MODULE,C,[]).
name(C) -> z_utils:name_for_site(traffic_guard_rules,z_context:site(C)).

%% A missing table falls back to the admission worker's authoritative copy.
-spec replace(map(), z:context()) -> ok.
replace(Lists,C) ->
    try gen_server:call(name(C),{replace,Lists},infinity)
    catch exit:{noproc,_} -> ok end.
-spec lookup(term(), z:context()) -> ip_allow | ip_block | none | unavailable.
lookup(IP,C) ->
    try
        Table=name(C),
        case ets:lookup(Table,ready) of
            [] -> unavailable;
            [_] ->
                case ets:lookup(Table,IP) of
                    [{IP,Allow,Block}] ->
                        Now=erlang:system_time(second),
                        case active(Allow,Now) of
                            true -> ip_allow;
                            false -> case active(Block,Now) of true -> ip_block; false -> none end
                        end;
                    [] -> none
                end
        end
    catch error:badarg -> unavailable end.
active(undefined,_) -> false;
active(0,_) -> true;
active(Until,Now) -> Until > Now.

-spec invalidate(z:context()) -> ok.
invalidate(C) ->
    try gen_server:call(name(C),invalidate,infinity)
    catch exit:{noproc,_} -> ok end.

-spec prune(z:context()) -> ok.
prune(C) -> gen_server:call(name(C),prune).
init(C) ->
    ets:new(name(C),[named_table,protected,set,{read_concurrency,true}]),
    erlang:send_after(1000,self(),prune),
    {ok,C}.
handle_call({replace,#{allow:=Allow,block:=Block}},_,C) ->
    Table=name(C),
    IPs=maps:merge(Allow,Block),
    Rows=[{IP,maps:get(IP,Allow,undefined),maps:get(IP,Block,undefined)} || IP<-maps:keys(IPs)],
    %% Each address is replaced atomically, including allow/block precedence.
    ets:insert(Table,Rows),
    [ets:delete(Table,IP) || {IP,_,_}<-ets:tab2list(Table),not maps:is_key(IP,IPs)],
    ets:insert(Table,{ready,true}),
    {reply,ok,C};
handle_call(invalidate,_,C) -> ets:delete(name(C),ready),{reply,ok,C};
handle_call(prune,_,C) -> expire(C),{reply,ok,C}.
handle_cast(_,C) -> {noreply,C}.
handle_info(prune,C) ->
    expire(C),
    erlang:send_after(1000,self(),prune),
    {noreply,C};
handle_info(_,C) -> {noreply,C}.
expire(C) ->
    Now=erlang:system_time(second),
    [ets:delete_object(name(C),Row) || {_,A,B}=Row<-ets:tab2list(name(C)),
        not active(A,Now),not active(B,Now)],
    ok.
