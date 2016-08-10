%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2012 Marc Worrell
%% @doc Server supplying random strings and unique ids

%% Copyright 2009-2012 Marc Worrell
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

-module(z_ids).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

%% Length of session keys, used for the cookies, must be unique
-define(ID_LENGTH,20).
-define(OPTID_LENGTH,6).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% id server exports
-export([
    start_tests/0,
    start_link/0,
    unique/0,
    id/0,
    id/1,
    identifier/0,
    identifier/1,
    optid/1,
    sign_key/1,
    sign_key_simple/1,
    number/0,
    number/1,
    rand_bytes/1
]).

-record(state, {unique_counter = 0}).
-include("zotonic.hrl").

start_tests() ->
    start_link().
    % gen_server:start({local, ?MODULE}, ?MODULE, [[{fixed_seed,true}]], []).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Return an unique id to be used in javascript or html.  No randomness, just unique in the cluster.
unique() ->
    gen_server:call(?MODULE, unique).

%% @doc Return a long random id, can be used for session ids.
id() ->
    gen_server:call(?MODULE, {id, ?ID_LENGTH}).

id(Len) ->
    gen_server:call(?MODULE, {id, Len}).

%% @spec identifier() -> binary()
%% @doc Get a random indentifier of a certain length, case insensitive
identifier() ->
    gen_server:call(?MODULE, {identifier, ?OPTID_LENGTH}).

identifier(Len) ->
    gen_server:call(?MODULE, {identifier, Len}).

optid(undefined) ->
    identifier(?OPTID_LENGTH);
optid(false) ->
    identifier(?OPTID_LENGTH);
optid(Id) ->
    Id.

%% @spec sign_key(Context) -> binary()
%% @doc Get the key for signing requests stored in the user agent.
sign_key(Context) ->
    case m_config:get_value(site, sign_key, Context) of
        undefined ->
            Key = list_to_binary(generate_id(true, 50)),
            m_config:set_value(site, sign_key, Key, Context),
            Key;
        <<>> ->
            application_key(sign_key);
        SignKey ->
            SignKey
    end.

%% @spec sign_key_simple(Context) -> binary()
%% @doc Get the key for less secure signing of data (without nonce).
sign_key_simple(Context) ->
    case m_config:get_value(site, sign_key_simple, Context) of
        undefined ->
            Key = list_to_binary(generate_id(true, 10)),
            m_config:set_value(site, sign_key_simple, Key, Context),
            Key;
        <<>> ->
            application_key(sign_key_simple);
        SignKey ->
            SignKey
    end.

%% @doc Set/get a default sign key for the zotonic.
application_key(Name) when is_atom(Name) ->
    case application:get_env(zotonic, Name) of
        undefined ->
            Key = list_to_binary(generate_id(true, 50)),
            application:set_env(zotonic, Name, Key),
            Key;
        {ok, Key} ->
            Key
    end.

%% @doc Return a big random integer, but smaller than maxint32
number() ->
    number(1000000000).

number(Max) ->
    gen_server:call(?MODULE, {number, Max}).

init(_Props) ->
    {ok, #state{unique_counter=0}}.

%%% Really random generators below. These are used for production Zotonic.

handle_call(unique, _From, #state{} = State) ->
    Id = make_unique(),
    {reply, Id, State};

handle_call({number, Max}, _From, #state{} = State) ->
    Number = crypto:rand_uniform(1, Max+1),
    {reply, Number, State};

handle_call({identifier, Len}, _From, #state{} = State) ->
    Id = generate_identifier(true, Len),
    {reply, Id, State};

handle_call({id, Len}, _From, #state{} = State) ->
    Id = generate_id(true, Len),
    {reply, Id, State};


handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.


%% @doc Create an unique temporary id, safe to use in html and javascript
make_unique() ->
    Ref = lists:flatten(io_lib:format("~p",[make_ref()])),
    "t" ++ unique1(Ref, []).

unique1([], Acc) -> Acc;
unique1([$.|T], Acc) ->
    unique1(T, [$_|Acc]);
unique1([H|T], Acc) when H >= $0 andalso H =< $9 ->
    unique1(T, [H|Acc]);
unique1([_|T], Acc) ->
    unique1(T, Acc).


%% @spec generate_id(boolean(), int()) -> string()
%% @doc Generate a random key
generate_id(IsUnique, Len) ->
    [ case N of
          C when C < 26 -> C  + $a;
          C when C < 52 -> C - 26 + $A;
          C -> C - 52 + $0
      end
      || N <- random_list(IsUnique, 62, Len)
    ].

%% @spec generate_identifier(boolean(), int()) -> string()
%% @doc Generate a random identifier, case insensitive, only letters
generate_identifier(IsUnique, Len) ->
    [ N + $a || N <- random_list(IsUnique, 26, Len) ].

random_list(false, Radix, Length) ->
    not_so_random_list(Radix, Length, []);
random_list(true, Radix, Length) ->
    N = (radix_bits(Radix) * Length + 7) div 8,
    Val = bin2int(rand_bytes(N)),
    int2list(Val, Radix, Length, []).

not_so_random_list(_Radix, 0, Acc) ->
    Acc;
not_so_random_list(Radix, N, Acc) ->
    not_so_random_list(Radix, N-1, [ crypto:rand_uniform(0, Radix) | Acc ]).

int2list(_, _, 0, Acc) ->
    Acc;
int2list(Val, Radix, Length, Acc) ->
    int2list(Val div Radix, Radix, Length-1, [ Val rem Radix | Acc]).

bin2int(Bin) ->
    lists:foldl(fun(N, Acc) -> Acc * 256 + N end, 0, binary_to_list(Bin)).

radix_bits(N) when N =< 16 -> 4;
radix_bits(N) when N =< 26 -> 5;
radix_bits(N) when N =< 64 -> 6.


%% @doc Return N random bytes. This falls back to the pseudo random version of rand_uniform
%% if strong_rand_bytes fails.
-spec rand_bytes(integer()) -> binary().
rand_bytes(N) when N > 0 ->
    try
        crypto:strong_rand_bytes(N)
    catch
        error:low_entropy ->
            lager:info("Crypto is low on entropy"),
            list_to_binary([ crypto:rand_uniform(0,256) || _X <- lists:seq(1, N) ])
    end.
