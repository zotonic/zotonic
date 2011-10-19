%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc Simple configuration server.  Holds and updates the global config in priv/config

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


-module(z_config).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

% gen_server exports
-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

% API export
-export([
    get/1,
    set/2,
    get_dirty/1,
    set_dirty/2
]).

-record(state, {config=[]}).

-include_lib("zotonic.hrl").

%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Get value from config file (cached)
get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

%% @doc Set value in config file, update cache.
set(Key, Value) ->
    gen_server:cast(?MODULE, {set, Key, Value}).


%%====================================================================
%% THE FOLLOWING TWO FUNCTIONS ARE USED ON STARTUP.
%% DO NOT USE ON ANY OTHER MOMENT.
%%====================================================================

%% @doc Dirty read of complete config file
get_dirty(Key) ->
    Config = case ensure_config() of
                 {ok, Cfg} -> Cfg;
                 _ -> []
             end,
    case proplists:get_value(Key, Config) of
        undefined -> 
            case default(Key) of
                {ok, Value} -> 
                    case Key of
                        password -> set_dirty(Key, Value);
                        _ -> nop
                    end,
                    Value;
                undefined ->
                    undefined
            end;
        Value ->
            Value
    end.


%% @doc Dirty write of config key, updates the config file on disk
%% when the value is changed
set_dirty(Key, Value) ->
    case ensure_config() of
        {ok, Config} ->
            case proplists:get_value(Key, Config) of
                Value -> ok;
                _ -> 
                    write_config(Key, Value),
                    ok
            end;
        Error ->
            Error
    end.


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(_Args) ->
    case ensure_config() of
        {ok, Config} ->
            {ok, #state{config=Config}};
        {error, Reason} ->
            ?ERROR("~s~n", [Reason]),
            {stop, config_file_error}
    end.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Get a value
handle_call({get, Key}, _From, State) ->
    handle_get(Key, State);

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

%% @doc Set a value
handle_cast({set, Key, Value}, State) ->
    case proplists:get_value(Key, State#state.config) of
        Value ->
            {noreply, State};
        _ -> 
            Config = write_config(Key, Value),
            {no_reply, State#state{config=Config}}
    end;

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.


%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================


ensure_config() ->
    File = config_file(),
    case filelib:is_regular(File) of
        true ->
            case file:consult(File) of
                {ok, Consult} ->
                    {ok, hd(Consult)};
                {error, Reason} ->
                    {error, io_lib:format("failed to parse: ~s: ~s", [File, file:format_error(Reason)])}
            end;
        false -> 
            {ok, Password} = default(password),
            write_config([{password, Password}]),
            ensure_config()
    end.


write_config(Key, Value) ->
    {ok, Config} = ensure_config(),
    Config1 = lists:keystore(Key, 1, Config, {Key, Value}),
    Config2 = lists:keystore(modify_date, 1, Config1, {modify_date, erlang:localtime()}),
    write_config(Config2).


write_config(Config) ->
    {ok, Dev} = file:open(config_file(), [write]),
    try
        ok = file:write(Dev, <<
            "% THIS FILE IS AUTOMATICALLY GENERATED, DO NOT CHANGE WHILE ZOTONIC IS RUNNING.",10,
            "%", 10,
            "% You can change these options when Zotonic is not running.",10,
            "% Delete this file to reset Zotonic to its defaults (when Zotonic is not running).",10,
            "% Consult the config.in file for available options and their defaults.",10,
            10>>),
        Data = io_lib:format("~p.~n", [Config]),
        ok = file:write(Dev, iolist_to_binary(Data))
    after
        ok = file:close(Dev)
    end,
    Config.
    

config_file() ->
    filename:join([z_utils:lib_dir(priv), "config"]).


handle_get(Prop, State) ->
    case proplists:get_value(Prop, State#state.config) of
        undefined ->
            case default(Prop) of
                {ok, Value} ->
                    case Prop of
                        password ->
                            Config = write_config([{Prop, Value} | State#state.config]),
                            {reply, Value, State#state{config=Config}};
                        _ ->
                            {reply, Value, State}
                    end;
                undefined ->
                    {reply, undefined, State}
            end;
        Value ->
            {reply, Value, State}
    end.


default(ssl) -> {ok, false};
default(ssl_cert) -> {ok, filename:join([z_utils:lib_dir(priv), "ssl"])};
default(ssl_key) -> {ok, filename:join([z_utils:lib_dir(priv), "ssl"])};
default(password) -> {ok, generate_id(8)};
default(listen_port) -> {ok, 8000};
default(listen_port_ssl) -> {ok, 8443};
default(listen_ip) -> {ok, any};
default(smtp_verp_as_from) -> {ok, false};
default(smtp_no_mx_lookups) -> {ok, false};
default(smtp_relay) -> {ok, false};
default(smtp_host) -> {ok, "localhost"};
default(smtp_port) -> {ok, 2525};
default(smtp_ssl) -> {ok, false};
default(smtp_bounce_ip) -> {ok, "127.0.0.1"};
default(smtp_bounce_port) -> {ok, 2525};
default(smtp_spamd_port) -> {ok, 783};
default(log_dir) -> {ok, filename:join([z_utils:lib_dir(priv), "log"])};
default(inet_backlog) -> {ok, 500};
default(webmachine_error_handler) -> {ok, z_webmachine_error_handler};
default(dbhost) -> {ok, "localhost"};
default(dbport) -> {ok, 5432};
default(dbuser) -> {ok, "zotonic"};
default(dbpassword) -> {ok, ""};
default(dbschema) -> {ok, "public"};
default(_) -> undefined.



%% @spec generate_id(int()) -> string()
%% @doc Generate a random key
generate_id(Len) ->
    generate_id(Len, []).

generate_id(0, Key) ->
    Key;
generate_id(Len, Key) ->
    Char = case random:uniform(62) of
                C when C =< 26 -> C - 1  + $a;
                C when C =< 52 -> C - 27 + $A;
                C -> C - 53 + $0
            end,
    generate_id(Len-1, [Char|Key]).
