%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%% @doc Show dispatch information for a site

%% Copyright 2021 Marc Worrell
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

-module(zotonic_cmd_dispatch).
-author("Marc Worrell").

%% API
-export([info/0, run/1]).

info() ->
    "Show dispatch information for a site or url.".

run([ "http:" ++ _ = Url ]) ->
    dispatch_url(Url);
run([ "https:" ++ _ = Url ]) ->
    dispatch_url(Url);
run([ Site ]) ->
    dispatch_site_list(Site);
run([ Site, "detail" ]) ->
    dispatch_site_detail(Site);
run([ Site, Path ]) ->
    dispatch_site_path(Site, Path);
run(_) ->
    io:format("USAGE: dispatch <site_name> [path|detail]~n"),
    io:format("       dispatch <URL>~n"),
    halt().

dispatch_url(Url) ->
    case zotonic_command:net_start() of
        ok ->
            UrlBin = unicode:characters_to_binary(Url, utf8),
            case zotonic_command:rpc(z, dispatch_url, [ UrlBin ]) of
                {ok, Trace} ->
                    io:format("# Dispatch trace for \"~s\"~n", [ UrlBin ]),
                    pretty_print_trace(Trace);
                {error, _} = Error ->
                    zotonic_command:format_error(Error)
            end;
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end.

dispatch_site_list(Site) ->
    case zotonic_command:net_start() of
        ok ->
            SiteName = list_to_atom(Site),
            case zotonic_command:rpc(z, dispatch_list, [ SiteName ]) of
                {ok, #{
                    hostname := Hostname,
                    smtphost := _SmtpHost,
                    hostalias := _Hostalias,
                    is_redirect := _IsRedirect,
                    dispatch_list := DispatchList
                }} ->
                    io:format("# Dispatch rules for ~p: ~s~n", [ SiteName, Hostname ]),
                    io:format("# First matching rule is used.~n"),
                    DL = lists:map(
                        fun(#{
                            dispatch := Dispatch,
                            controller := Controller,
                            controller_options := ControllerOptions,
                            path := Path
                        }) ->
                            {
                                pretty_controller_path(Path),
                                z_convert:to_binary(Dispatch),
                                z_convert:to_binary(proplists:get_value(zotonic_dispatch_module, ControllerOptions)),
                                z_convert:to_binary(Controller)
                            }
                        end,
                        DispatchList),
                    print_table(
                        [ "Path", "Dispatch", "Module", "Controller" ],
                        DL),
                    ok;
                {error, _} = Error ->
                    zotonic_command:format_error(Error)
            end;
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end.

dispatch_site_detail(Site) ->
    case zotonic_command:net_start() of
        ok ->
            SiteName = list_to_atom(Site),
            case zotonic_command:rpc(z, dispatch_list, [ SiteName ]) of
                {ok, #{
                    hostname := Hostname,
                    smtphost := SmtpHost,
                    hostalias := Hostalias,
                    is_redirect := IsRedirect,
                    dispatch_list := DispatchList
                }} ->
                    io:format("# Dispatch rules for ~p~n", [ SiteName ]),
                    io:format("hostname: \"~s\"~n", [ Hostname ]),
                    case Hostalias of
                        [] -> ok;
                        undefined -> ok;
                        _ ->
                            io:format("hostalias:~n"),
                            lists:foreach(
                                fun(A) ->
                                    io:format("  - \"~s\"~n", [ A ])
                                end,
                                Hostalias)
                    end,
                    case SmtpHost of
                        undefined -> ok;
                        _ -> io:format("smtphost: \"~s\"~n", [ SmtpHost ])
                    end,
                    io:format("redirect: ~p~n", [ z_convert:to_bool(IsRedirect) ]),
                    io:format("dispatch_rules:~n"),
                    lists:map(
                        fun(#{
                            dispatch := Dispatch,
                            controller := Controller,
                            controller_options := ControllerOptions,
                            path := Path
                        }) ->
                            io:format("  - ~p~n", [ Dispatch ]),
                            io:format("    path: \"~s\"~n", [ pretty_controller_path(Path) ]),
                            io:format("    controller: ~p~n", [ Controller ]),
                            io:format("    options:"),
                            pretty_print_proplist(2, ControllerOptions)
                        end,
                        DispatchList),
                    ok;
                {error, _} = Error ->
                    zotonic_command:format_error(Error)
            end;
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end.

pretty_controller_path([]) ->
    <<"/">>;
pretty_controller_path(Path) ->
    Mapped = lists:map(
        fun
            (S) when is_list(S) -> unicode:characters_to_binary(S, utf8);
            (S) when is_binary(S) -> S;
            ('*') -> "*";
            (A) when is_atom(A) -> [ $:, atom_to_binary(A, utf8) ];
            ({A, Pattern}) when is_atom(A), is_list(Pattern) ->
                [ $:, atom_to_binary(A, utf8), $:, Pattern ];
            (Other) -> io_lib:format("~p", [ Other ])
        end,
        Path),
    Mapped1 = [ [ $/, P ] || P <- Mapped ],
    iolist_to_binary(Mapped1).

dispatch_site_path(Site, Path) ->
    case zotonic_command:net_start() of
        ok ->
            SiteName = list_to_atom(Site),
            PathBin = unicode:characters_to_binary(Path, utf8),
            case zotonic_command:rpc(z, dispatch_path, [ PathBin, SiteName ]) of
                {ok, Trace} ->
                    io:format("# Dispatch trace for ~p: \"~s\"~n", [ SiteName, PathBin ]),
                    pretty_print_trace(Trace);
                {error, _} = Error ->
                    zotonic_command:format_error(Error)
            end;
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end.

pretty_print_trace([]) ->
    io:format("~n");
pretty_print_trace([ #{ args := Args, path := Path, step := Step } | Trace ]) ->
    io:format("- ~p:", [ Step ]),
    case Path of
        undefined -> ok;
        _ when is_list(Path) -> io:format("~n    dispatch_path: \"/~s\"", [ lists:join($/, Path) ]);
        _ when is_binary(Path) -> io:format("~n    dispatch_path: \"~s\"", [ Path ])
    end,
    pretty_print_proplist(1, Args),
    pretty_print_trace(Trace).

pretty_print_value(_Indent, V) when is_atom(V); is_integer(V) ->
    io:format("~p~n", [ V ]);
pretty_print_value(_indent, V) when is_binary(V) ->
    io:format("\"~s\"~n", [ V ]);
pretty_print_value(Indent, [ X | _ ] = L) when not is_integer(X) ->
    pretty_print_proplist(Indent, L);
pretty_print_value(_Indent, V) when is_list(V) ->
    io:format("~s~n", [ V ]);
pretty_print_value(_Indent, V) ->
    io:format("~p~n", [ V ]).


pretty_print_proplist(Indent, L) ->
    io:format("~n"),
    lists:map(
        fun
            ({path, [ X | _] = P}) when is_integer(X) ->
                io:format("~spath: \"~s\"~n", [ indent(Indent), P ]);
            ({P, DP}) when P =:= zotonic_dispatch_path; P =:= path, is_list(DP) ->
                DP1 = lists:map(
                    fun(V) ->
                        z_convert:to_list(V)
                    end,
                    DP),
                io:format("~spath: ~p~n", [ indent(Indent), DP1 ]);
            ({K,V}) ->
                io:format("~s~p: ", [ indent(Indent), K ]),
                pretty_print_value(Indent+1, V);
            (K) when is_atom(K) ->
                io:format("~s~p: true~n", [ indent(Indent), K ]);
            (K) when is_binary(K) ->
                io:format("~s - ~s~n", [ indent(Indent), K ])
        end,
        L).

indent(0) ->
    "";
indent(N) ->
    "    " ++ indent(N-1).


print_table(_Hs, []) ->
    io:format("~n");
print_table(Hs, DL) ->
    W1 = colsize(1, DL, 1),
    W2 = colsize(2, DL, 1),
    W3 = colsize(3, DL, 1),
    W4 = colsize(4, DL, 1),
    io:format("~*.s ~*.s ~*.s ~s~n", [
        -W1, lists:nth(1, Hs),
        -W2, lists:nth(2, Hs),
        -W3, lists:nth(3, Hs),
        lists:nth(4, Hs)
        ]),
    io:format("~*.s ~*.s ~*.s ~s~n", [
        -W1, sep(W1, ""),
        -W2, sep(W2, ""),
        -W3, sep(W3, ""),
        sep(W4, "")
        ]),
    lists:map(
        fun({C1, C2, C3, C4}) ->
            io:format("~*.s ~*.s ~*.s ~s~n", [ -W1, C1, -W2, C2, -W3, C3, C4 ] )
        end,
        DL).

sep(0, Acc) -> Acc;
sep(N, Acc) -> sep(N-1, [ $= | Acc ]).

colsize(_N, [], W) ->
    W;
colsize(N, [ T | Ts ], W) ->
    W1 = erlang:max(W, size( erlang:element(N, T))),
    colsize(N, Ts, W1).