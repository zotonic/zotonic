%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019-2023 Driebit BV
%% @doc Rate limiting of authentication tries and other types of requests

%% Copyright 2019-2023 Driebit BV
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

-module(m_ratelimit).

-behaviour(zotonic_model).

-export([
    m_get/3,

    init/1,
    insert_event/4,
    insert_event/5,
    is_event_limited/4,
    list_event/3,
    delete_event/3,
    delete_event/4,
    reset/1,
    prune/1
]).

-include_lib("stdlib/include/qlc.hrl").

-type device_id() :: binary() | undefined.

% Failure event for the given key/device.
-record(ratelimit_event, {
    key :: {atom(), binary()},
    device :: device_id(),
    timestamp :: pos_integer(),
    props :: proplists:proplist()
}).

% Default period for counting events
-define(RATELIMIT_T, 3600).

% Default number of event before restriction
-define(RATELIMIT_N, 5).


m_get([ <<"timeout">> | Rest ], _Msg, Context) ->
    {ok, {ratelimit_t(Context), Rest}}.

%% @doc Insert an event, use the context for extra properties.
-spec insert_event( atom(), binary(), device_id(), z:context() ) -> ok | {error, term()}.
insert_event(Type, Key, Device, Context) ->
    Props = [
        {peer, m_req:get(peer, Context)},
        {user_agent, m_req:get(user_agent, Context)}
    ],
    insert_event(Type, Key, Device, Props, Context).


%% @doc Insert an event, with extra properties.
-spec insert_event( atom(), binary(), device_id(), proplists:proplist(), z:context() ) -> ok.
insert_event(Type, Key, Device, Props, Context) ->
    Event = #ratelimit_event{
        key = {Type, Key},
        device = Device,
        timestamp = z_datetime:timestamp(),
        props = Props
    },
    case mnesia:transaction( fun() -> mnesia:write(event_table(Context), Event, write) end ) of
        {atomic, _} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.


%% @doc Check if the key/device is rate limited due to previous events.
-spec is_event_limited( atom(), binary(), device_id(), z:context() ) -> boolean().
is_event_limited( Type, Key, Device, Context ) ->
    T = z_datetime:timestamp() - ratelimit_t(Context),
    List = mnesia:dirty_read( event_table(Context), {Type, Key} ),
    Filtered = lists:filter(
        fun(R) ->
            R#ratelimit_event.device =:= Device
            andalso R#ratelimit_event.timestamp >= T
        end,
        List),
    length(Filtered) >= ratelimit_n(Context).

%% @doc Return all entries for an event
-spec list_event( atom(), binary(), z:context() ) -> list().
list_event(Type, Key, Context) ->
    mnesia:dirty_read( event_table(Context), {Type, Key} ).

%% @doc Delete all entries for an event
-spec delete_event( atom(), binary(), z:context() ) -> ok.
delete_event(Type, Key, Context) ->
    {atomic, _} = mnesia:transaction(
        fun() ->
            mnesia:delete({ event_table(Context), {Type, Key}})
        end),
    ok.

%% @doc Delete all entries for an event with a matching device id.
-spec delete_event( Type, Key, Device, Context ) -> ok when
    Type :: atom(),
    Key :: binary(),
    Device :: device_id(),
    Context :: z:context().
delete_event(Type, Key, Device, Context) ->
    List = mnesia:dirty_read( event_table(Context), {Type, Key} ),
    Filtered = lists:filter(
        fun(R) ->
            R#ratelimit_event.device =:= Device
        end,
        List),
    {atomic, _} = mnesia:transaction(
        fun() ->
            lists:foreach(
                fun(Obj) ->
                    mnesia:delete_object(event_table(Context), Obj, write)
                end,
                Filtered)
        end),
    ok.

-spec init( z:context() ) -> ok.
init(Context) ->
    TabName = event_table(Context),
    TabDef = [
        {type, bag},
        {record_name, ratelimit_event},
        {attributes, record_info(fields, ratelimit_event)}
        | case application:get_env(mnesia, dir) of
             {ok, _} -> [ {disc_copies, [node()]} ];
             undefined -> []
          end
    ],
    case mnesia:create_table(TabName, TabDef) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, TabName}} -> ok
    end.

-spec ratelimit_t( z:context() ) -> integer().
ratelimit_t(Context) ->
    case m_config:get_value(mod_ratelimit, event_period, Context) of
        <<>> -> ?RATELIMIT_T;
        undefined -> ?RATELIMIT_T;
        T -> z_convert:to_integer(T)
    end.

-spec ratelimit_n( z:context() ) -> integer().
ratelimit_n(Context) ->
    case m_config:get_value(mod_ratelimit, event_count, Context) of
        <<>> -> ?RATELIMIT_N;
        undefined -> ?RATELIMIT_N;
        N -> z_convert:to_integer(N)
    end.

-spec reset(z:context()) -> ok.
reset(Context) ->
    Table = event_table(Context),
    PruneFun = fun() ->
        Query = qlc:q([ Ev || Ev <- mnesia:table(Table) ]),
        lists:foreach(
            fun(Ev) -> mnesia:delete_object(Table, Ev, write) end,
            qlc:e(Query))
    end,
    {atomic, _} = mnesia:transaction(PruneFun),
    ok.

-spec prune(z:context()) -> ok.
prune(Context) ->
    Table = event_table(Context),
    Oldest = z_datetime:timestamp() - ratelimit_t(Context) - 1,
    PruneFun = fun() ->
        Query = qlc:q([ Ev || Ev <- mnesia:table(Table), Ev#ratelimit_event.timestamp < Oldest ]),
        lists:foreach(
            fun(Ev) -> mnesia:delete_object(Table, Ev, write) end,
            qlc:e(Query))
    end,
    {atomic, _} = mnesia:transaction(PruneFun),
    ok.

event_table(Context) ->
    list_to_atom("ratelimit_event-" ++ z_convert:to_list(z_context:site(Context))).
