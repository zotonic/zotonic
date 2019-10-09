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
-export([get_zotonic_dir/0, get_node_host/0, base_cmd/0, base_cmd_test/0]).

-include("../../include/zotonic_command.hrl").
-include_lib("zotonic_core/include/zotonic_release.hrl").

get_zotonic_dir() ->
    {ok, CurrentDir} = file:get_cwd(),
    _Dir = CurrentDir.

get_node_host() ->
    {ok, HostName} = inet:gethostname(),
    _NodeHost = HostName.

base_cmd() ->
    zotonic_launcher_config:load_configs(),
    IsDistributed = is_distributed(),
    case is_fqdn() of
        false when IsDistributed ->
            {error,
                "echo Zotonic is configured to run as distributed node, but your hostname is "
                "not configured to be a fully qualified domain name. Please configure "
                "echo your system so the output of \"hostname -f\" returns a FQDN."};
        IsFQDN ->
            SOpt = case erlang:system_info(schedulers) of
                1 -> "+S 4:4";
                _ -> ""
            end,
            {ok, lists:flatten([
                "erl -smp enable ", SOpt,
                " -env ERL_MAX_PORTS ", max_ports(),
                " +P ", max_processes(),
                " +K ", kernel_poll(),
                " -pa ", lists:map( fun(D) -> [ " ", D ] end, code_paths() ),
                " ", name_arg(IsDistributed andalso IsFQDN),
                " -boot start_sasl ",
                    maybe_config( zotonic_launcher_config:erlang_config_file() ), " ",
                    maybe_config( zotonic_launcher_config:zotonic_config_file() )
            ])}
    end.

base_cmd_test() ->
    zotonic_launcher_config:load_configs(),
    SOpt = case erlang:system_info(schedulers) of
        1 -> "+S 4:4";
        _ -> ""
    end,
    {ok, lists:flatten([
        "erl -smp enable ", SOpt,
        " -env ERL_MAX_PORTS ", max_ports(),
        " +P ", max_processes(),
        " +K ", kernel_poll(),
        " -pa ", lists:map( fun(D) -> [ " ", D ] end, code_paths_test() ),
        " -sname zotonic001_testsandbox@", ?NODEHOST,
        " -boot start_sasl ",
            maybe_config( zotonic_launcher_config:erlang_config_file() ), " ",
            maybe_config( zotonic_launcher_config:zotonic_config_file() )
    ])}.

maybe_config({ok, F}) -> "-config " ++ F;
maybe_config(false) -> "".

name_arg(true) ->
    NodeName = ?NODENAME ++ "@" ++ ?NODEHOST,
    [ "-name ", NodeName ];
name_arg(false) ->
    NodeName = ?NODENAME ++ "@" ++ ?NODEHOST,
    [ "-sname ", NodeName ].

is_fqdn() ->
    lists:member($., hostname()).

hostname() ->
    strip_nl( os:cmd("hostname -f") ).

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
    case os:getenv("MAX_PROCESSES") of
        false -> "10000000";
        "" -> "10000000";
        MaxP -> MaxP
    end.

kernel_poll() ->
    case os:getenv("KERNEL_POLL") of
        false -> "true";
        "" -> "true";
        KP -> KP
    end.

is_distributed() ->
    case os:getenv("ZOTONIC_DISTRIBUTED") of
        false -> false;
        "true" -> true;
        _ -> false
    end.

code_paths_test() ->
    code_paths()
    ++ [
        filename:join( [ ?ZOTONIC, "_build", "default", "lib", "zotonic_*", "test" ])
    ].

code_paths() ->
    [
        filename:join( [ ?ZOTONIC, "_build", "default", "lib", "*", "ebin" ]),
        filename:join( [ ?ZOTONIC, "_checkouts", "*", "ebin" ])
    ]
    ++ lists:usort(
        case application:get_env(user_sites_dir) of
            undefined -> [];
            {ok, "_checkouts"} -> [];
            {ok, ""} -> [];
            {ok, Dir} -> filename:join([ Dir, "*", "ebin" ])
        end
        ++ case application:get_env(user_modules_dir) of
            undefined -> [];
            {ok, "_checkouts"} -> [];
            {ok, ""} -> [];
            {ok, Dir} -> filename:join([ Dir, "*", "ebin" ])
        end
    ).

strip_nl(S) ->
    lists:filter(fun(C) -> C >= 32 end, S).
