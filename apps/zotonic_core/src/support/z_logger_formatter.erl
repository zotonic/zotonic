%%% @doc
%%% This is the main module that exposes custom formatting to the OTP
%%% logger library (part of the `kernel' application since OTP-21).
%%%
%%% Adapted from logjam, added pretty print of reports and stack traces.
%%% @end

%% Copyright 2019-2021 LFE Exchange https://github.com/lfex/logjam/
%% Copyright 2022-2024 Marc Worrell
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

-module(z_logger_formatter).

%% API exports
-export([
    format/2,
    apply_defaults/1, format_log/4, format_to_binary/2, string_to_binary/1
    ]).

-export([format_msg/2, to_string/2, pretty_stack/2, truncate_value/1]).

-ifdef(TEST).
-endif.

-define(BLACK, "\e[0;30m").
-define(BLACKB, "\e[1;30m").
-define(BLACK_ON_GOLD, "\e[30;43m").
-define(BLUE, "\e[0;34m").
-define(BLUEB, "\e[1;34m").
-define(CYAN, "\e[0;36m").
-define(CYANB, "\e[1;36m").
-define(GOLD, "\e[0;33m").
-define(GOLDB, "\e[1;33m").
-define(GOLDB_ON_RED, "\e[1;33;41m").
-define(GREEN, "\e[0;32m").
-define(GREENB, "\e[1;32m").
-define(GREY, "\e[0;37m").
-define(GREYB, "\e[1;37m").
-define(MAGENTA, "\e[0;35m").
-define(MAGENTAB, "\e[1;35m").
-define(RED, "\e[0;31m").
-define(REDB, "\e[1;31m").
-define(COLOR_END, "\e[0m").


-type template() :: [metakey() | {metakey(), template(), template()} | {atom()} | string()].
-type metakey() :: atom() | [atom()].

-define(TRUNCATE_DEPTH, 5).
-define(TRUNCATE_LIST, 6).
-define(TRUNCATE_STRING, 1000).

-include_lib("zotonic_core/include/zotonic.hrl").


%%====================================================================
%% API functions
%%====================================================================
-spec format(LogEvent, Config) -> unicode:chardata() when
      LogEvent :: logger:log_event(),
      Config :: logger:formatter_config().
format(Map = #{msg := {report, #{label := {error_logger, _}, format := Format, args := Terms}}}, UsrConfig) ->
    Map1 = Map#{
        msg := {report, #{ text => format_to_binary(Format, Terms) }}
    },
    format(Map1, UsrConfig);
format(Map = #{msg := {report, #{label := {proc_lib, crash}, report := [Info, Linked] }}}, UsrConfig) ->
    Report = case maps:from_list(Info) of
        #{
            error_info := {Class, {badmatch, Value}, Stack}
        } = InfoMap ->
            #{
                text => <<"Badmatch">>,
                linked => Linked,
                result => Class,
                reason => badmatch,
                match => truncate_value(Value),
                stack => Stack,
                proc => maps:without([ error_info, dictionary ], InfoMap)
            };
        #{
            error_info := {Class, Reason, Stack}
        } = InfoMap ->
            #{
                text => case Reason of
                    undef -> <<"Undefined function">>;
                    _ -> <<"Error in process">>
                end,
                linked => Linked,
                result => Class,
                reason => truncate_value(Reason),
                stack => Stack,
                proc => maps:without([ error_info, dictionary ], InfoMap)
            };
        InfoMap ->
            #{
                text => <<"Error in process">>,
                linked => Linked,
                proc => truncate_value(InfoMap)
            }
    end,
    format(Map#{ msg => {report, Report} }, UsrConfig);
format(#{level:=Level, msg:={report, Msg}, meta:=Meta}, UsrConfig) when is_map(Msg) ->
    Config = apply_defaults(UsrConfig),
    NewMeta = maps:merge(Meta, #{level => Level
                                ,colored_start => Level
                                ,colored_end => ?COLOR_END
                                }),
    format_log(maps:get(template, Config), Config, Msg, NewMeta);
format(Map = #{msg := {report, KeyVal}}, UsrConfig) when is_list(KeyVal) ->
    format(Map#{msg := {report, maps:from_list(KeyVal)}}, UsrConfig);
format(Map = #{msg := {string, String}}, UsrConfig) ->
    Map1 = Map#{
        msg => {report, #{ text => string_to_binary(String) }}
    },
    format(Map1, UsrConfig);
format(Map = #{msg := {"Error in process ~p on node ~p with exit value:~n~p~n", Terms}}, UsrConfig) ->
    [ Pid, Node, ExitValue ] = Terms,
    Map1 = case ExitValue of
        {undef, [ {M, F, Args, _Pos} | _ ] = Stack} when is_list(Args) ->
            Map#{
                msg => {report, #{
                    text => <<"Undefined function">>,
                    pid => Pid,
                    node => Node,
                    result => error,
                    reason => undef,
                    module => M,
                    function => F,
                    arity => length(Args),
                    stack => Stack
                }}
            };
        {{Reason, Value}, [ {M, F, _Arity, Pos} | _ ] = Stack}  when is_atom(M), is_atom(F), is_list(Pos) ->
            Map#{
                msg => {report, #{
                    text => <<"Error in process, exit">>,
                    pid => Pid,
                    node => Node,
                    result => error,
                    reason => Reason,
                    value => truncate_value(Value),
                    stack => Stack
                }}
            };
        {Term, [ {M, F, _Arity, Pos} | _ ] = Stack} when is_atom(M), is_atom(F), is_list(Pos) ->
            Map#{
                msg => {report, #{
                    text => <<"Error in process, exit">>,
                    pid => Pid,
                    node => Node,
                    result => error,
                    reason => undef,
                    exit_value => truncate_value(Term),
                    stack => Stack
                }}
            };
        _ ->
            Map#{
                msg => {report, #{
                    text => <<"Error in process, exit">>,
                    pid => Pid,
                    node => Node,
                    result => error,
                    reason => undef,
                    exit_value => truncate_value(ExitValue)
                }}
            }
    end,
    format(Map1, UsrConfig);
format(Map = #{msg := {Format, Terms}}, UsrConfig) when is_list(Format), is_list(Terms) ->
    Map1 = Map#{
        msg := {report, #{ text => format_to_binary(Format, Terms) }}
    },
    format(Map1, UsrConfig);
format(Map = #{msg := Msg}, UsrConfig) ->
    Map1 = Map#{
        msg := {report, #{ msg => Msg }}
    },
    format(Map1, UsrConfig).

%%====================================================================
%% Internal functions
%%====================================================================

apply_defaults(UserConfig) ->
    DefaultConfig = #{
        prettify => true,
        colored => false,
        colored_date => ?GREEN,
        colored_debug => ?BLUEB,
        colored_info => ?CYAN,
        colored_notice => ?GREENB,
        colored_warning => ?GOLDB,
        colored_error => ?REDB,
        colored_critical => ?RED,
        colored_alert => ?BLACK_ON_GOLD,
        colored_emergency => ?GOLDB_ON_RED,
        colored_pid => ?GOLD,
        colored_pid_brackets => ?GREEN,
        colored_mfa => ?GOLD,
        colored_arrow => ?CYANB,
        colored_msg => ?GREENB,
        colored_text => ?GREEN,
        colored_key => ?MAGENTA},
    Map = maps:merge(DefaultConfig, UserConfig),
    #{prettify := Prettify} = Map,
    #{colored := IsColored} = Map,
    #{colored_mfa := ColoredMfa} = Map,
    #{colored_arrow := ColoredArrow} = Map,
    Template = case IsColored of
        true -> [time, " ", colored_start, level, colored_end, " ",
                 {id}, {parent_id}, {correlation_id},
                 {site}, {user_id}, {dispatch},
                 pid,
                 " [", ColoredMfa, mfa, ":", line, ?COLOR_END, "] ",
                 ColoredArrow, "▸ ", ?COLOR_END, msg ];
        _ -> [time, " ", level, " ",
              {id}, {parent_id}, {correlation_id},
              {site}, {user_id}, {dispatch},
              pid,
              " [", mfa, ":", line, "] ",
              "▸ ", msg ]
    end,
    Template1 = Template ++ case Prettify of
        true -> [ stack, "\n" ];
        false -> [ "\n" ]
    end,
    maps:merge(
      #{term_depth => undefined,
        map_depth => -1,
        time_offset => 0,
        time_unit => second,
        time_designator => $T,
        strip_tz => false,
        level_capitalize => false,
        level_length => -1,
        template => Template1
       },
      Map
    ).

-spec format_log(template(), Config, Msg, Meta) -> unicode:chardata() when
      Config :: logger:formatter_config(),
      Msg :: Data,
      Meta :: Data,
      Data :: #{string() | binary() | atom() => term()}.
format_log(Tpl, Config, Msg, Meta) -> format_log(Tpl, Config, Msg, Meta, []).

format_log([], _Config, _Msg, _Meta, Acc) ->
    lists:reverse(Acc);
format_log([msg | Rest], Config, Msg, Meta, Acc) ->
    Msg1 = case lists:member(stack, Rest) of
        true -> remove_stack(Msg);
        false -> Msg
    end,
    format_log(Rest, Config, Msg, Meta, [format_msg(Msg1, Config) | Acc]);
format_log([stack | Rest], Config, Msg, Meta, Acc) ->
    format_log(Rest, Config, Msg, Meta, [format_stack(Msg, Meta, Config) | Acc]);
format_log([Key | Rest], Config, Msg, Meta, Acc) when is_atom(Key)
                                                 orelse is_atom(hd(Key)) -> % from OTP
    case maps:find(Key, Meta) of
        error ->
            format_log(Rest, Config, Msg, Meta, Acc);
        {ok, Val} ->
            format_log(Rest, Config, Msg, Meta, [format_val(Key, Val, Config) | Acc])
    end;
format_log([{Key, IfExists, Else} | Rest], Config, Msg, Meta, Acc) ->
    case maps:find(Key, Meta) of
        error ->
            format_log(Rest, Config, Msg, Meta, [Else | Acc]);
        {ok, undefined} ->
            format_log(Rest, Config, Msg, Meta, [Else | Acc]);
        {ok, Val} ->
            Text = format_log(IfExists, Config, Msg, #{Key => Val}, []),
            format_log(Rest, Config, Msg, Meta, [ Text | Acc])
    end;
format_log([{Key} | Rest], Config, Msg, Meta, Acc) when is_atom(Key) ->
    case maps:find(Key, Meta) of
        error ->
            format_log(Rest, Config, Msg, Meta, Acc);
        {ok, undefined} ->
            format_log(Rest, Config, Msg, Meta, Acc);
        {ok, Val} ->
            Text = format_msg(#{ Key => Val }, Config),
            format_log(Rest, Config, Msg, Meta, [ [" ", Text] | Acc])
    end;
format_log([Term | Rest], Config, Msg, Meta, Acc) when is_list(Term) ->
    format_log(Rest, Config, Msg, Meta, [Term | Acc]).


format_stack(
        #{ label := {proc_lib,crash}, report := [ Props, _ ] },
        _Meta,
        Config) when is_list(Props) ->
    case proplists:get_value(error_info, Props) of
        {error, E, S} when is_list(S) ->
            pretty_reason({E, S}, Config);
        {exit, E, S} when is_list(S) ->
            pretty_reason({E, S}, Config);
        X ->
            format_msg(#{ report => X }, Config)
    end;
format_stack(
        #{ label := {gen_server,terminate}, reason := Reason },
        _Meta,
        Config) when is_tuple(Reason) ->
    pretty_reason(Reason, Config);
format_stack(
        #{ label := {supervisor,child_terminated}, report := Report},
        _Meta,
        Config) when is_list(Report) ->
    case proplists:get_value(reason, Report) of
        {E, S} when is_list(S) ->
            pretty_reason({E, S}, Config);
        X ->
            format_msg(#{ report => X }, Config)
    end;
format_stack(#{ stack := [] }, _Meta, _Config) ->
    [];
format_stack(#{ stack := Stack }, _Meta, Config) ->
    [
        "\n",
        "  Stack:",
        pretty_stack(Stack, Config)
    ];
format_stack(_Data, #{ stack := Stack }, Config) ->
    [
        "\n",
        "  Stack:",
        pretty_stack(Stack, Config)
    ];
format_stack(_Data, _Meta, _Config) ->
    [].

remove_stack(#{ label := {proc_lib,crash}, report := [ Props, X ]} = Msg) when is_list(Props) ->
    Props1 = proplists:delete(error_info, Props),
    Msg#{ report => [ Props1, X ]};
remove_stack(#{ label := {gen_server,terminate}, reason := Reason } = Msg) when is_tuple(Reason) ->
    maps:remove(reason, Msg);
remove_stack(#{ label := {supervisor,child_terminated}, report := Props} = Msg) when is_list(Props) ->
    Props1 = proplists:delete(reason, Props),
    Msg#{ report => Props1};
remove_stack(Msg) ->
    Msg.


format_msg(Data, Config) ->
    format_msg("", Data, Config).

format_msg(Parents, Data, Config=#{map_depth := 0}) when is_map(Data) ->
    [
        maybe_color(colored_text, Config),
        to_string(truncate_key(Parents), Config),
        maybe_color(colored_end, Config),
        "=... "
    ];
format_msg(Parents, Data, Config = #{map_depth := Depth}) when is_map(Data) ->
    maps:fold(
      fun
        (K, V, Acc) when is_map(V) ->
            [
                format_msg(Parents ++ to_string(K, Config) ++ "_",
                           V,
                           Config#{map_depth := Depth-1})
                | Acc
            ];
        (stack, _, Acc) ->
            Acc;
        (K, V, Acc) ->
            [
                maybe_color(colored_key, Config),
                Parents ++ to_string(K, Config),
                maybe_color(colored_end, Config),
                $=,
                maybe_color(colored_text, Config),
                to_string(V, Config),
                maybe_color(colored_end, Config),
                $\s
                | Acc
            ]
      end,
      [],
      Data
    ).

format_val(time, Time, Config) ->
    format_time(Time, Config);
format_val(mfa, MFA, Config) ->
    escape(format_mfa(MFA, Config));
format_val(level, Level, Config) ->
    format_level(Level, Config);
format_val(pid, Pid, Config) ->
    format_pid(Pid, Config);
format_val(colored_end, _EOC, #{colored := false}) -> "";
format_val(colored_end, EOC,  #{colored := true}) -> EOC;
format_val(colored_start, _Level,    #{colored := false}) -> "";
format_val(colored_start, debug,     #{colored := true, colored_debug     := BOC}) -> BOC;
format_val(colored_start, info,      #{colored := true, colored_info      := BOC}) -> BOC;
format_val(colored_start, notice,    #{colored := true, colored_notice    := BOC}) -> BOC;
format_val(colored_start, warning,   #{colored := true, colored_warning   := BOC}) -> BOC;
format_val(colored_start, error,     #{colored := true, colored_error     := BOC}) -> BOC;
format_val(colored_start, critical,  #{colored := true, colored_critical  := BOC}) -> BOC;
format_val(colored_start, alert,     #{colored := true, colored_alert     := BOC}) -> BOC;
format_val(colored_start, emergency, #{colored := true, colored_emergency := BOC}) -> BOC;
format_val(_Key, Val, Config) ->
    to_string(Val, Config).

format_time(N, #{time_offset := O,
                 time_unit := U,
                 time_designator := D,
                 strip_tz := Strip}) when is_integer(N) ->
    N2 = case U of
             second -> round(N / 1000000);
             millisecond -> round(N / 1000);
             _ -> N
    end,
    Time = calendar:system_time_to_rfc3339(N2, [{unit, U},
                                                {offset, O},
                                                {time_designator, D}]),
    case Strip of
        true -> lists:sublist(Time, 1, length(Time) - 6);
        _ -> Time
    end.

format_level(Level, Config) when is_atom(Level) ->
    format_level(atom_to_list(Level), Config);
format_level(Level, #{level_capitalize := Is_cap, level_length := Lvl_len}) ->
    L2 = case Is_cap of
        true -> string:to_upper(Level);
        _ -> Level
    end,
    case Lvl_len > 0 of
        true -> lists:sublist(L2, Lvl_len);
        _ -> L2
    end.

format_pid(Pid, Config) when is_pid(Pid) ->
    format_pid(pid_to_list(Pid), Config);
format_pid(Pid, #{colored := false}) when is_list(Pid) -> Pid;
format_pid(Pid, #{colored := true, colored_pid := CP, colored_pid_brackets := CPB}) when is_list(Pid) ->
    CPB ++ "<" ?COLOR_END ++
    CP ++ re:replace(Pid, "[<>]", "", [global, {return, list}]) ++ ?COLOR_END ++
    CPB ++ ">" ++ ?COLOR_END.

format_mfa({M, F, A}, _) when is_atom(M), is_atom(F), is_integer(A) ->
   [atom_to_list(M), $:, atom_to_list(F), $/, integer_to_list(A)];
format_mfa({M, F, A}, Config) when is_atom(M), is_atom(F), is_list(A) ->
    %% arguments are passed as a literal list ({mod, fun, [a, b, c]})
    format_mfa({M, F, length(A)}, Config);
format_mfa(MFAStr, Config) -> % passing in a pre-formatted string value
    re:replace(
        re:replace(escape(to_string(MFAStr,Config)), "^{'", ""),
        "'}$", "").

to_string(X, _) when is_atom(X) ->
    escape(atom_to_list(X));
to_string(X, _) when is_integer(X) ->
    integer_to_list(X);
to_string(X, _) when is_pid(X) ->
    pid_to_list(X);
to_string(X, _) when is_reference(X) ->
    ref_to_list(X);
to_string(X, C = #{colored := IsColored, colored_text := CT}) when is_binary(X) ->
    BeginColor = case IsColored of
        true -> CT;
        _ -> ""
    end,
    EndColor = case IsColored of
        true -> ?COLOR_END;
        _ -> ""
    end,
    String = case unicode:characters_to_list(X) of
        {_, _, _} -> % error or incomplete
            escape(format_str(C, X));
        List ->
            case io_lib:printable_unicode_list(List) of
                true -> escape(List);
                _ -> escape(format_str(C, X))
            end
    end,
    BeginColor ++ String ++ EndColor;
to_string(X, C) when is_list(X) ->
    case io_lib:printable_list(X) of
        true -> escape(X);
        _ -> escape(format_str(C, X))
    end;
to_string(X, C) ->
    escape(format_str(C, X)).

format_str(#{term_depth := undefined}, T) ->
    io_lib:format("~0tp", [T]);
format_str(#{term_depth := D}, T) ->
    io_lib:format("~0tP", [T, D]).

escape(Str) ->
    % io:format("Checking to escape ~p~n", [Str]),
    case needs_escape(Str) of
        false ->
            case needs_quoting(Str) of
                true -> [$", maybe_truncate(Str), $"];
                false -> Str
            end;
        true ->
            [$", maybe_truncate(do_escape(Str)), $"]
    end.

needs_quoting(Str) ->
    string:find(Str, " ") =/= nomatch orelse
    string:find(Str, "=") =/= nomatch.

needs_escape(Str) ->
    string:find(Str, "\"") =/= nomatch orelse
    string:find(Str, "\\") =/= nomatch orelse
    string:find(Str, "\n") =/= nomatch.

do_escape([]) ->
    [];
do_escape(Str) ->
    % io:format("Escaping string: ~p~n", [Str]),
    case string:next_grapheme(Str) of
        [$\n | Rest] -> [$\\, $\n | do_escape(Rest)];
        ["\r\n" | Rest] -> [$\\, $\r, $\\, $\n | do_escape(Rest)];
        [$" | Rest] -> [$\\, $" | do_escape(Rest)];
        [$\\ | Rest] -> [$\\, $\\ | do_escape(Rest)];
        [Grapheme | Rest] -> [Grapheme | do_escape(Rest)]
    end.

truncate_key([]) -> [];
truncate_key("_") -> "";
truncate_key([H|T]) -> [H | truncate_key(T)].

string_to_binary(String) ->
    %% Remove any ANSI colors; this is intended for inputs that have ANSI
    %% colors added to them, e.g., by another logging library/framework.
    T1 = re:replace(String, "\e\[[0-9;]*m", ""),
    maybe_truncate(unicode:characters_to_binary(T1)).

format_to_binary(Format, Terms) ->
    try
        String = io_lib:format(Format, Terms),
        string_to_binary(String)
    catch
        _:_ ->
            String1 = io_lib:format("~tp", [ {Format, Terms} ]),
            string_to_binary(String1)
    end.

maybe_truncate(L) when is_list(L), length(L) > ?TRUNCATE_STRING ->
    z_string:truncatechars(unicode:characters_to_binary(L), ?TRUNCATE_STRING, <<"...">>);
maybe_truncate(B) when size(B) > ?TRUNCATE_STRING ->
    z_string:truncatechars(B, ?TRUNCATE_STRING, <<"...">>);
maybe_truncate(B) ->
    B.


%% ================ pretty print gen_server reason ==================

pretty_reason({Error, Stack}, Config) when is_list(Stack) ->
    [
        "\n",
        pretty_error(Error, Config),
        pretty_stack(Stack, Config)
    ];
pretty_reason(Reason, Config) ->
    format_msg(#{ reason => Reason }, Config).

pretty_error({Error, Reason}, Config) ->
    [
        "  Reason: ",
        maybe_color(?MAGENTA, Config),
        io_lib:format("~tp: ", [ Error ]),
        maybe_color(?COLOR_END, Config),
        to_string(Reason, Config)
    ];
pretty_error(Error, Config) ->
    [
        "  Reason: ",
        maybe_color(?MAGENTA, Config),
        io_lib:format("~tp: ", [ Error ]),
        maybe_color(?COLOR_END, Config)
    ].


pretty_stack(Stack, Config) ->
    lists:map(
        fun
            ({M, F, Args, Pos}) when is_list(Args) ->
                [
                    "\n     ",
                    maybe_color(colored_mfa, Config),
                    io_lib:format("~ts:~ts(", [ bin(M), bin(F) ]),
                    case truncate_value(Args) of
                        [ A | As ] ->
                            [
                                io_lib:format("~tp", [ A ]),
                                lists:map(
                                    fun(V) ->
                                        io_lib:format(", ~tp", [ V ])
                                    end,
                                    As)
                            ];
                        [] ->
                            ""
                    end,
                    ")",
                    maybe_color(?COLOR_END, Config),
                    " @ ",
                    pretty_pos(Pos, Config)
                ];
            ({M, F, Arity, Pos}) ->
                [
                    "\n     ",
                    maybe_color(colored_mfa, Config),
                    io_lib:format("~ts:~ts/~p", [ bin(M), bin(F), Arity ]),
                    maybe_color(?COLOR_END, Config),
                    " @ ",
                    pretty_pos(Pos, Config)
                ]
        end,
        Stack).

%% todo: check to see if io_lib:limit_term/2 can be used.
truncate_value(V) ->
    truncate_value(V, 0).

truncate_value(Args, 0) when is_list(Args) ->
    lists:map(fun(A) -> truncate_value(A, 1) end, Args);
truncate_value(Args, Level) when is_list(Args) ->
    L1 = truncate_list(Args, Level),
    lists:map(fun(A) -> truncate_value(A, Level+1) end, L1);
truncate_value(Args, Level) when is_map(Args), Level > ?TRUNCATE_DEPTH ->
    #{ '...' => '...' };
truncate_value(Args, Level) when is_map(Args) ->
    maps:fold(
        fun(K, V, Acc) ->
            Acc#{ K => truncate_value(V, Level+1) }
        end,
        #{},
        Args);
truncate_value(#context{} = Context, Level) ->
    truncate_context(Context, Level);
truncate_value(T, Level) when is_tuple(T) ->
    L = tuple_to_list(T),
    L1 = lists:map(fun(V) -> truncate_value(V, Level+1) end, L),
    list_to_tuple(L1);
truncate_value(L, Level) when is_list(L) ->
    L1 = truncate_list(L, Level),
    lists:map(fun(V) -> truncate_value(V, Level+1) end, L1);
truncate_value(V, _Level) ->
    V.

truncate_list(_L, Level) when Level > ?TRUNCATE_DEPTH ->
    [ '...' ];
truncate_list(L, _Level) ->
    case length(L) > ?TRUNCATE_LIST of
        true ->
            {L1, _} = lists:split(?TRUNCATE_LIST, L),
            L1 ++ [ '...' ];
        false ->
            L
    end.

truncate_context(#context{ site = Site }, Level) when Level > 2 ->
    {context, #{ site => Site }};
truncate_context(#context{} = Context, Level) ->
    #context{
        site = Site,
        client_id = ClientId,
        routing_id = RoutingId,
        user_id = UserId,
        acl = Acl,
        acl_is_read_only = AclIsReadOnly,
        language = Language,
        tz = Tz,
        controller_module = ControllerModule,
        props = Props,
        cowreq = Req
    } = Context,
    SessionId = case z_context:session_id(Context) of
        {ok, SId} -> SId;
        {error, _} -> undefined
    end,
    Map = #{
        site => Site,
        client_id => ClientId,
        routing_id => RoutingId,
        session_id => SessionId,
        user_id => UserId,
        acl => Acl,
        acl_is_read_only => AclIsReadOnly,
        language => Language,
        tz => Tz,
        controller_module => ControllerModule,
        props => Props,
        cowreq => truncate_cowreq(Req)
    },
    {context, truncate_value(Map, Level+1)}.

truncate_cowreq(Req) when is_map(Req) ->
    maps:with([
            pid, port, scheme, version, path, host, peer, bindings, headers,
            ref, method, qs, body_length, has_body, sock, resp_headers
        ], Req);
truncate_cowreq(Req) ->
    Req.

pretty_pos(Pos, Config) ->
    [
        maybe_color(colored_text, Config),
        io_lib:format("\"~ts\"", [ proplists:get_value(file, Pos, "undefined") ]),
        maybe_color(colored_end, Config),
        case proplists:get_value(line, Pos) of
            undefined -> "";
            Line when is_integer(Line) -> [ $:, integer_to_list(Line) ];
            {Line, Col} -> [ $:, integer_to_list(Line), $:, integer_to_list(Col) ]
        end
    ].

maybe_color(_, #{ colored := false }) ->
    "";
maybe_color(colored_end, _Config) ->
    ?COLOR_END;
maybe_color(Color, Config) when is_atom(Color) ->
    maps:get(Color, Config, "");
maybe_color(Color, _Config) ->
    Color.

bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
bin(B) when is_binary(B) -> B.
