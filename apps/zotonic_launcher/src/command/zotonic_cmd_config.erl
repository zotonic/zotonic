%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell
%% @doc Test if the config files are syntactically ok

%% Copyright 2019 Marc Worrell
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

-module(zotonic_cmd_config).
-author("Marc Worrell").

%% API
-export([run/1]).
-export([pretty_print_value/2]).

run([ "all" ]) ->
    show(all);
run([ "zotonic" ]) ->
    show(zotonic);
run([ "erlang" ]) ->
    show(erlang);
run([]) ->
    show(zotonic);
run(_) ->
    io:format("USAGE: config [all | zotonic | erlang]~n"),
    halt(1).

show(What) ->
    case zotonic_command:get_target_node() of
        {ok, Target} ->
            if
                What =:= all; What =:= zotonic ->
                    io:format("~nZotonic config for ~p:~n=================================~n", [Target]),
                    ZotonicConfigFiles = zotonic_launcher_config:zotonic_config_files(Target),
                    case zotonic_launcher_config:read_configs(ZotonicConfigFiles) of
                        {ok, ZCfg} ->
                            pretty_print(ZCfg);
                        {error, _} = ZError ->
                            zotonic_command:format_error(ZError)
                    end;
                true ->
                    ok
            end,
            io:format("~n"),
            if
                What =:= all; What =:= erlang ->
                    io:format("~nErlang init for ~p:~n=================================~n", [Target]),
                    ErlangConfigFiles = zotonic_launcher_config:erlang_config_files(Target),
                    case zotonic_launcher_config:read_configs(ErlangConfigFiles) of
                        {ok, ECfg} ->
                            pretty_print(ECfg);
                        {error, _} = EError ->
                            zotonic_command:format_error(EError)
                    end;
                true ->
                    ok
            end,
            io:format("~n");
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end.

pretty_print(Map) when is_map(Map) ->
    AppList = lists:sort( maps:to_list(Map) ),
    case proplists:get_value(zotonic, AppList) of
        undefined ->
            ok;
        ZCfg ->
            zotonic_launcher_config:load_configs(#{ zotonic => ZCfg }),
            ZCfgList = z_config:all(),
            io:format("~nzotonic:"),
            lists:foreach(
                fun({Key, Value}) ->
                    io:format("~n    ~p: ", [ Key ]),
                    pretty_print_value(Key, Value)
                end,
                ZCfgList)
    end,
    lists:foreach(
        fun({App, AppCfg}) ->
            pretty_print_app(App, AppCfg)
        end,
        proplists:delete(zotonic, AppList)).

pretty_print_app(App, Cfg) ->
    CfgList = lists:sort( maps:to_list(Cfg) ),
    io:format("~n~p:", [App]),
    lists:foreach(
        fun({Key, Value}) ->
            io:format("~n    ~p: ", [ Key ]),
            pretty_print_value(Key, Value)
        end,
        CfgList).

pretty_print_value(IP, Value) when is_tuple(Value) andalso
    (       IP =:= listen_ip      orelse IP =:= listen_ip6
     orelse IP =:= mqtt_listen_ip orelse IP =:= mqtt_listen_ip6
     orelse IP =:= smtp_listen_ip orelse IP =:= smtp_listen_ip6
     orelse IP =:= smtp_spamd_ip)
      ->
    io:format("~s", [ inet:ntoa(Value) ]);
pretty_print_value(_Key, Value) when is_binary(Value) ->
    case is_utf8(Value) of
        true ->
            io:format("~s", [ Value ]);
        false ->
            io:format("~p", [ Value ])
    end;
pretty_print_value(_Key, Value) when is_list(Value) ->
    case z_string:is_string(Value) of
        true ->
            io:format("~s", [ Value ]);
        false ->
            lists:foreach(
                fun
                    (V) when is_binary(V) ->
                        case is_utf8(V) of
                            true -> io:format("~n      - ~s", [ V ]);
                            false -> io:format("~n      - ~p", [ V ])
                        end;
                    (V) when is_list(V) ->
                        case z_string:is_string(V) of
                            true -> io:format("~n      - ~s", [ V ]);
                            false -> io:format("~n      - ~p", [ V ])
                        end;
                    (V) ->
                        io:format("~n      - ~p", [ V ])
                end,
                Value)
    end;
pretty_print_value(_Key, undefined) ->
    ok;
pretty_print_value(_Key, Value) when is_map(Value) ->
    List = lists:sort( maps:to_list(Value) ),
    lists:foreach(
        fun
            ({K, V}) when is_binary(V) ->
                case is_utf8(V) of
                    true -> io:format("~n      ~p: ~s", [ K, V ]);
                    false -> io:format("~n      ~p: ~p", [ K, V ])
                end;
            ({K, V}) when is_list(V) ->
                case z_string:is_string(V) of
                    true -> io:format("~n      ~p: ~s", [ K, V ]);
                    false -> io:format("~n      ~p: ~p", [ K, V ])
                end;
            ({K, V}) ->
                io:format("~n      ~p: ~p", [ K, V ])
        end,
        List);
pretty_print_value(_Key, Value) ->
    io:format("~p", [ Value ]).

is_utf8(<<>>) -> true;
is_utf8(<<_/utf8, R/binary>>) -> is_utf8(R);
is_utf8(_) -> false.
