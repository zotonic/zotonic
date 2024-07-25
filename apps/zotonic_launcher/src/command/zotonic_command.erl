%%%-------------------------------------------------------------------
%%% @author Blaise
%%% @doc
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%%
%%% @end
%%% Created : 18. Dec 2017 4:53 PM
%%%-------------------------------------------------------------------
-module(zotonic_command).
-author("Blaise").

%% API
-export([
    get_zotonic_dir/0,
    get_target_node/0,
    net_start/0,
    net_start/1,
    rpc/3,
    format_error/1,
    base_cmd/0,
    base_cmd_test/0
]).

-include("../../include/zotonic_command.hrl").
-include_lib("zotonic_core/include/zotonic_release.hrl").

get_zotonic_dir() ->
    {ok, Dir} = case os:getenv("ZOTONIC") of
        false -> file:get_cwd();
        "" -> file:get_cwd();
        ZotonicDir -> {ok, ZotonicDir}
    end,
    Dir.

get_target_node() ->
    case zotonic_command_nodename:nodename_target( list_to_atom(?DEFAULT_NODENAME) ) of
        {ok, {_LongOrShortnames, Nodename}} ->
            {ok, Nodename};
        {error, _} = Error ->
            Error
    end.

net_start() ->
    net_start("command" ++ integer_to_list(rand:uniform(1000))).

net_start(Name) ->
    case zotonic_command_nodename:nodename_command( list_to_atom(Name) ) of
        {error, _} = Error ->
            Error;
        {ok, {LongOrShortnames, Nodename}} ->
            os:cmd("epmd -daemon"),
            case net_kernel:start([Nodename, LongOrShortnames]) of
                {ok, _Pid} ->
                    case erlang:get_cookie() of
                        nocookie ->
                            {error, nocookie};
                        Cookie ->
                            erlang:set_cookie(node(), Cookie),
                            ok
                    end;
                {error, _} = Error ->
                    Error
            end
    end.

rpc(Module, Function, Args) ->
    case get_target_node() of
        {ok, Target} ->
            case net_adm:ping(Target) of
                pong ->
                    case rpc:call(Target, Module, Function, Args) of
                        {badrpc, _} = RpcError ->
                            format_error(RpcError);
                        Other ->
                            Other
                    end;
                pang ->
                    format_error({error, pang})
            end;
        {error, _} = Error ->
            Error
    end.


format_error({error, long}) ->
    io:format(standard_error,
              "Zotonic is configured to run as distributed node, but your hostname is "
              "not configured to be a fully qualified domain name. Please configure "
              "your system so the output of 'hostname -f' returns a FQDN.~n",
              []),
    halt(1);
format_error({error, short}) ->
    io:format(standard_error,
              "Zotonic is configured to run as local (short name) node, but your SNAME "
              "contains a \".\" character, please try again with another SNAME argument.~n",
              []),
    halt(1);
format_error({error, pang}) ->
    {ok, Target} = get_target_node(),
    io:format(standard_error, "Zotonic node at ~p is not running~n", [Target]),
    halt(1);
format_error({badrpc, Reason}) ->
    {ok, Target} = get_target_node(),
    io:format(standard_error, "RPC error to ~p:~n~p~n", [ Target, Reason ]),
    halt(1);
format_error({error, {config_file, Reason, File, undefined}}) ->
    io:format(standard_error, "Error reading config file '~s': ~p~n", [ File, Reason ]),
    halt(1);
format_error({error, {config_file, consult_error, File, {Line, erl_parse, Msg}}}) ->
    io:format(standard_error, "Error reading config file '~s':~p ~s~n", [ File, Line, Msg ]),
    halt(1);
format_error({error, {config_file, Reason, File, Extra}}) ->
    io:format(standard_error, "Error reading config file '~s': ~p~n~p~n", [ File, Reason, Extra ]),
    halt(1);
format_error({error, not_running_as_root}) ->
    io:format(standard_error,
              "Running Zotonic as root is extremely dangerous and not supported.~n"
              "Try again using a regular user account.~n",
              []),
    halt(1);
format_error({error, Reason}) ->
    io:format(standard_error, "Error: ~p~n", [ Reason ]),
    halt(1).

base_cmd() ->
    base_cmd(?DEFAULT_NODENAME, code_paths()).

base_cmd_test() ->
    base_cmd(?DEFAULT_NODENAME_TEST, code_paths_test()).

base_cmd(DefaultName, CodePaths) ->
    case zotonic_command_nodename:nodename_target( list_to_atom(DefaultName) ) of
        {error, long} ->
            {error,
                "Zotonic is configured to run as distributed node, but your hostname is "
                "not configured to be a fully qualified domain name. Please configure "
                "your system so the output of 'hostname -f' returns a FQDN."};
        {error, short} ->
            {error,
                "Zotonic is configured to run as local (short name) node, but your SNAME "
                "contains a \".\" character, please try again with another SNAME argument."};
        {ok, {LongOrShortnames, Nodename}} ->
            CfgFiles = zotonic_launcher_config:config_files(Nodename),
            case zotonic_launcher_config:read_configs(CfgFiles) of
                {ok, _Cfgs} ->
                        SOpt = case erlang:system_info(schedulers) of
                            1 -> " +S 2:2";
                            _ -> ""
                        end,
                        KOpt = case binary:split(atom_to_binary(Nodename), <<"@">>) of
                            [ _, <<"localhost">> ] ->
                                " -kernel inet_dist_listen_options '[{ip, {127,0,0,1}}]' ";
                            _ ->
                                ""
                        end,
                        {ok, lists:flatten([
                            "erl",
                            " -smp enable",
                            SOpt,
                            KOpt,
                            " -env ERL_MAX_PORTS ", max_ports(),
                            " +P ", max_processes(),
                            " +t ", max_atoms(),
                            " +K ", kernel_poll(),
                            " -pa ", lists:map( fun(D) -> [ " ", D ] end, CodePaths ),
                            " ", name_arg(LongOrShortnames, Nodename),
                            " -boot start_sasl ",
                            erlang_configs(Nodename)
                        ])};
                {error, _} = Error ->
                    Error
            end
    end.

erlang_configs(Nodename) ->
    lists:map(
        fun(F) ->
            [ " -config ", z_filelib:os_escape(F) ]
        end,
        zotonic_launcher_config:erlang_config_files(Nodename)).

name_arg(longnames, Nodename) ->
    [ "-name ", atom_to_list(Nodename) ];
name_arg(shortnames, Nodename) ->
    [ "-sname ", atom_to_list(Nodename) ].

max_ports() ->
    case os:getenv("ERL_MAX_PORTS") of
        false -> ulimit_n();
        "" -> ulimit_n();
        EnvMP -> EnvMP
    end.

ulimit_n() ->
    case strip_nl(os:cmd("ulimit -n")) of
        "unlimited" -> "100000";
        N -> N
    end.

max_processes() ->
    case os:getenv("ERL_MAX_PROCESSES") of
        false -> "5000000";
        "" -> "5000000";
        MaxP -> MaxP
    end.

max_atoms() ->
    case os:getenv("ERL_MAX_ATOMS") of
        false -> "1048576";
        "" -> "1048576";
        MaxA -> MaxA
    end.

kernel_poll() ->
    case os:getenv("ERL_KERNEL_POLL") of
        false -> "true";
        "" -> "true";
        KP -> KP
    end.

code_paths_test() ->
    code_paths()
    ++ [
        filename:join( [ get_zotonic_dir(), "_build", "default", "lib", "zotonic_*", "test" ])
    ].

code_paths() ->
    [
        filename:join( [ get_zotonic_dir(), "_checkouts", "*", "ebin" ]),
        filename:join( [ get_zotonic_dir(), "_build", "default", "checkouts", "*", "ebin" ]),
        filename:join( [ get_zotonic_dir(), "_build", "default", "lib", "*", "ebin" ])
    ].

strip_nl(S) ->
    lists:filter(fun(C) -> C >= 32 end, S).
