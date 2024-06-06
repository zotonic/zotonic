%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2024 Marc Worrell
%% @doc Track onetime anti replay tokens, prevent re-use within a
%% period after being invalidated.
%% @end

%% Copyright 2024 Marc Worrell
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

-module(z_replay_token).

-behaviour(gen_server).

-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
    ]).

-export([
    new_token/0,
    invalidate_token/3,
    is_spent_token/2
    ]).

% Cleanup interval for tokens
-define(CLEANUP_INTERVAL, 60000).

-record(state, {
    site :: atom(),
    table :: atom()
    }).

-record(spent, {
    token :: binary(),
    timeout :: non_neg_integer()
}).


%%====================================================================
%% API
%%====================================================================

%% @doc Return a new token, assume random is good enough to generate
%% an unique token.
-spec new_token() -> binary().
new_token() ->
    z_ids:id().

%% @doc Invalidate a token, garbage collected at the given timestamp.
-spec invalidate_token(Token, Timestamp, Context) -> ok when
    Token :: binary(),
    Timestamp :: non_neg_integer(),
    Context :: z:context().
invalidate_token(Token, Seconds, Context) ->
    Table = table(Context),
    Timestamp = z_datetime:timestamp() + Seconds,
    ets:insert(Table, #spent{ token = Token, timeout = Timestamp}),
    ok.

%% @doc Check if a token is registered as being invalidated.
-spec is_spent_token(Token, Context) -> boolean() when
    Token :: binary(),
    Context :: z:context().
is_spent_token(Token, Context) ->
    Table = table(Context),
    [] =/= ets:lookup(Table, Token).


%% @doc Starts the MQTT ticket server server
start_link(Site) ->
    Name = z_utils:name_for_site(?MODULE, Site),
    gen_server:start_link({local, Name}, ?MODULE, [ Site ], []).


table(SiteOrContext) ->
    z_utils:name_for_site(?MODULE, SiteOrContext).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([ Site ]) ->
    State = #state{
        site = Site,
        table = table(Site)
    },
    ets:new(State#state.table, [
        named_table,
        public,
        {keypos, 2}
    ]),
    timer:send_after(?CLEANUP_INTERVAL, cleanup),
    {ok, State}.


handle_call(_Msg, _From, #state{} = State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup, #state{ table = Table } = State) ->
    Now = z_datetime:timestamp(),
    Keys = ets:foldl(
        fun
            (#spent{ token = Token, timeout = T }, Acc) when T < Now ->
                [ Token | Acc ];
            (#spent{}, Acc) ->
                Acc
        end,
        [],
        Table),
    lists:foreach(fun(Token) -> ets:delete(Table, Token) end, Keys),
    timer:send_after(?CLEANUP_INTERVAL, cleanup),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

