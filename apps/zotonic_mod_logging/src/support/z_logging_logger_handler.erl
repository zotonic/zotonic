%% @author Viacheslav Katsuba, Marc Worrell
%% @copyright 2022-2024 Viacheslav Katsuba, Marc Worrell
%% @doc Loggger handler, mapping log message to JSON for display on the
%% browser console. Formatting routines are similar to those in the logstasher
%% application. As that application is optional, we have a duplicate format_msg
%% function here in this module.
%% @end

%% Copyright 2022-2024 Viacheslav Katsuba, Marc Worrell
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

-module(z_logging_logger_handler).

-export([
    install/2,
    uninstall/1,
    add_filter/1,
    handler_filter/2
]).

%% logger callbacks
-export([log/2]).

%% Xref ignores
-ignore_xref([log/2]).

-define(LOG_BINARY_SIZE, 2000).

%% @doc Install the logger handler for the given sitename. The handler is installed
%% for the site and the logging module. The logging module will install the handler
%% when the module starts with copying the logs to a browser (or topic).
-spec install(Sitename, LogPid) -> ok | {error, term()} when
    LogPid :: pid(),
    Sitename :: atom().
install(Sitename, LogPid) ->
    HandlerId = z_utils:name_for_site(?MODULE, Sitename),
    case logger:add_handler(HandlerId, ?MODULE, #{ config => #{ site => Sitename, pid => LogPid } }) of
        ok ->
            add_filter(Sitename);
        {error, {already_exist, HandlerId}} ->
            uninstall(Sitename),
            install(Sitename, LogPid);
        {error, _} = Error ->
            Error
    end.

%% @doc Remove the logger handler for the given sitename.
-spec uninstall(Sitename) -> ok | {error, term()} when
    Sitename :: atom().
uninstall(Sitename) ->
    HandlerId = z_utils:name_for_site(?MODULE, Sitename),
    logger:remove_handler(HandlerId).


%% @doc Set the logging filter level to only log events for this site or system
%% level events without a specific site.
-spec add_filter(Sitename) -> ok | {error, term()} when
    Sitename :: atom().
add_filter(Sitename) ->
    HandlerId = z_utils:name_for_site(?MODULE, Sitename),
    FilterId = z_utils:name_for_site(logger_filter, Sitename),
    case logger:add_handler_filter(HandlerId, FilterId, {fun z_logging_logger_handler:handler_filter/2, Sitename}) of
        ok ->
            ok;
        {error, {already_exist, FilterId}} ->
            logger:remove_handler_filter(HandlerId, FilterId),
            add_filter(Sitename);
        {error, _} = Error ->
            Error
    end.

handler_filter(#{ level := info, meta := #{error_logger := #{type := progress}} }, _Sitename) ->
    stop;
handler_filter(#{ msg := {report, #{ is_secure_log := true }} }, _Sitename) ->
    stop;
handler_filter(#{ meta := #{ is_secure_log := true } }, _Sitename) ->
    stop;
handler_filter(#{ meta := Meta } = Event, Sitename) ->
    case maps:get(site, Meta, Sitename) of
        Sitename -> Event;
        _Other -> stop
    end.

%%==============================================================================
%% API
%%==============================================================================

-spec log(logger:log_event(), logger:handler_config()) -> ok.
log(#{ level := info, meta := #{error_logger := #{type := progress}} }, _Config) ->
    % Ignore supervisor progress reports
    ok;
log(#{ level := Level, msg := EventData, meta := Meta }, #{ config := #{ pid := Pid } }) ->
    try
        {Msg, MsgMap} = format_msg(EventData),
        SafeMeta = safe_meta(maps:without([ gl ], Meta)),
        {Msg1, MsgMap1} = case Msg of
            null -> {maps:get(text, MsgMap, null), maps:remove(text, MsgMap)};
            _ -> {Msg, MsgMap}
        end,
        Data = #{
            severity => Level,
            timestamp => format_timestamp(Meta),
            message => Msg1,
            fields => MsgMap1,
            meta => SafeMeta
        },
        Pid ! {logger, Data},
        ok
    catch
        _:_ -> ok
    end.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec format_msg(Data) -> {Message, #{ Key => Value } } when
    Data :: {io:format(), [ term() ]}
          | {report, logger:report()}
          | {string, unicode:chardata()},
    Message :: binary() | null,
    Key :: binary() | atom(),
    Value :: jsx:json_term().
format_msg({string, Message}) ->
    {unicode:characters_to_binary(Message), #{}};
format_msg({report, Report}) when is_map(Report) ->
    {maps:get(msg, Report, null), safe_fields(Report)};
format_msg({report, Report}) when is_list(Report) ->
    format_msg({report, maps:from_list(Report)});
format_msg({"Error in process ~p on node ~p with exit value:~n~p~n", [_, _, {undef, Undef}]}) ->
    format_undef(Undef);
format_msg({Format, Params}) when is_list(Format), is_list(Params) ->
    {unicode:characters_to_binary(io_lib:format(Format, Params)), #{}};
format_msg(Other) ->
    {unicode:characters_to_binary(io_lib:format("~p", [ Other ])), #{}}.

format_undef([ {Module, Function, Args, _} | _ ] = Stack) when is_list(Args) ->
    Arity = length(Args),
    Message = io_lib:format("Undefined function ~p:~p/~p", [Module, Function, Arity]),
    Report = #{
        result => error,
        reason => undef,
        module => Module,
        function => Function,
        args => Args,
        stack => Stack
    },
    {unicode:characters_to_binary(Message), safe_fields(Report)}.

-spec format_timestamp(logger:metadata()) -> binary().
format_timestamp(#{time := Ts}) ->
    list_to_binary(calendar:system_time_to_rfc3339(Ts, [{unit, microsecond}, {offset, "Z"}])).

-spec safe_meta(logger:metadata()) -> #{ Key => Term } when
    Key :: binary() | atom(),
    Term :: jsx:json_term().
safe_meta(Meta) ->
    safe_fields(Meta).

-spec safe_fields(map()) -> map().
safe_fields(Terms) ->
    maps:fold(
        fun(K, V, Acc) ->
            {K1, V1} = safe_field(K, V),
            Acc#{ K1 => V1 }
        end,
        #{},
        Terms).

-spec safe_field(atom() | binary() | string(), term()) -> {atom() | binary(), jsx:json_term()}.
safe_field(stack, Stack) when is_list(Stack) ->
    {stack, safe_stack(Stack)};
safe_field(file, Filename) when is_list(Filename) ->
    {file, unicode:characters_to_binary(Filename)};
safe_field(Key, Value) when is_atom(Key); is_binary(Key) ->
    {Key, safe_value(Value)};
safe_field(Key, Value) when is_list(Key) ->
    safe_field(unicode:characters_to_binary(Key), Value).

safe_stack(Stack) ->
    lists:map(fun safe_stack_entry/1, Stack).

safe_stack_entry({Mod, Fun, Args, _}) when is_atom(Mod), is_atom(Fun), is_list(Args) ->
    Arity = length(Args),
    Function = io_lib:format("~p:~p/~p", [Mod, Fun, Arity]),
    #{
        function => unicode:characters_to_binary(Function)
    };
safe_stack_entry({Mod, Fun, Arity, Loc}) when is_atom(Mod), is_atom(Fun), is_integer(Arity) ->
    Function = io_lib:format("~p:~p/~p", [ Mod, Fun, Arity ]),
    #{
        function => unicode:characters_to_binary(Function),
        at => unicode:characters_to_binary([stack_file(Loc), $:, integer_to_binary(stack_line(Loc))])
    };
safe_stack_entry(Entry) ->
    safe_value(Entry).

stack_file(Loc) when is_list(Loc) -> proplists:get_value(file, Loc, "");
stack_file({File, _}) -> File;
stack_file({File, _, _}) -> File;
stack_file(_) -> "".

stack_line([ {_, _} | _ ] = Loc) -> proplists:get_value(line, Loc, "");
stack_line({_, Line}) -> Line;
stack_line({_, Line, _}) -> Line;
stack_line(_) -> 0.

-spec safe_value(term()) -> jsx:json_term().
safe_value(Pid) when is_pid(Pid) ->
    list_to_binary(pid_to_list(Pid));
safe_value([]) ->
    [];
safe_value(List) when is_list(List) ->
    case is_proplist(List) of
        true -> safe_value(map_from_proplist(List));
        false ->
            case is_ascii_list(List) of
                true -> unicode:characters_to_binary(List);
                false -> lists:map(fun safe_value/1, List)
            end
    end;
safe_value(Map) when is_map(Map) ->
    safe_fields(Map);
safe_value(undefined) ->
    null;
safe_value(Val) when is_atom(Val); is_number(Val) ->
    Val;
safe_value(Val) when is_binary(Val) ->
    maybe_truncate(Val);
safe_value(Val) ->
    maybe_truncate(unicode:characters_to_binary(io_lib:format("~p", [Val]))).

% Map a proplists to a map
map_from_proplist(L) ->
    lists:foldl(
        fun
            ({K,V}, Acc) -> Acc#{ K => V };
            (K, Acc) -> Acc#{ K => true }
        end,
        #{},
        L).

% If something is a proplist, then we will display it as a map.
is_proplist([]) -> true;
is_proplist([ {K, _} | T ]) when is_atom(K); is_binary(K) -> is_proplist(T);
is_proplist([ K | T ]) when is_atom(K) -> is_proplist(T);
is_proplist(_) -> false.

% Simple ASCII character string, typically SQL statements, filenames or literal texts.
is_ascii_list([]) -> true;
is_ascii_list([ C | T ]) when C >= 32, C =< 127 -> is_ascii_list(T);
is_ascii_list([ C | T ]) when C =:= $\n; C =:= $\t -> is_ascii_list(T);
is_ascii_list(_) -> false.

maybe_truncate(Bin) when size(Bin) >= ?LOG_BINARY_SIZE ->
    <<Truncated:?LOG_BINARY_SIZE/binary, _/binary>> = Bin,
    <<Truncated/binary, "...">>;
maybe_truncate(Bin) ->
    Bin.

