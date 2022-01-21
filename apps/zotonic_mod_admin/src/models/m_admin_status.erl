%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2019-2021 Maas-Maarten Zeeman 
%% @doc Zotonic: admin status model

%% Copyright 2019-2021 Maas-Maarten Zeeman
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

-module(m_admin_status).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-behaviour(zotonic_model).

-include_lib("zotonic_core/include/zotonic_release.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").


%% interface functions
-export([
    m_get/3,

    otp_version/0,
    database_version/1,
    tcp_connection_count/0,
    group_sockets/0,
    close_sockets/2,
    disks/0,
    disks_alert/0
]).

-spec m_get( list(), zotonic_model:opt_msg(), z:context()) -> zotonic_model:return().
m_get([ <<"zotonic_version">> | Rest ], _Msg, _Context) ->
    {ok, {z_convert:to_binary(?ZOTONIC_VERSION), Rest}};
m_get(Path, Msg, Context) ->
    case z_acl:is_admin(Context) of
        true -> m_get_1(Path, Msg, Context);
        false -> {error, eacces}
    end.
m_get_1([ <<"database_version">> | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true -> {ok, {database_version(Context), Rest}};
        false -> {error, eacces}
    end;
m_get_1([ <<"otp_version">> | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true -> {ok, {otp_version(), Rest}};
        false -> {error, eacces}
    end;
m_get_1([ <<"config_dir">> | Rest ], _Msg, _Context) ->
    case z_config_files:config_dir() of
        {ok, Dir} ->
            {ok, {Dir, Rest}};
        {error, _} = Error ->
            Error
    end;
m_get_1([ <<"security_dir">> | Rest ], _Msg, _Context) ->
    {ok, {z_config:get(security_dir), Rest}};
m_get_1([ <<"log_dir">> | Rest ], _Msg, _Context) ->
    {ok, {z_config:get(log_dir), Rest}};
m_get_1([ <<"data_dir">> | Rest ], _Msg, _Context) ->
    {ok, {z_config:get(data_dir), Rest}};
m_get_1([ <<"cache_dir">> | Rest ], _Msg, _Context) ->
    {ok, {z_config:get(cache_dir), Rest}};
m_get_1([ <<"work_dir">> | Rest ], _Msg, _Context) ->
    case file:get_cwd() of
        {ok, Dir} ->
            {ok, {unicode:characters_to_binary(Dir), Rest}};
        {error, _} = Error ->
            Error
    end;
m_get_1([ <<"files_dir">> | Rest ], _Msg, Context) ->
    Dir = z_path:files_subdir(".", Context),
    {ok, {filename:dirname(unicode:characters_to_binary(Dir)), Rest}};
m_get_1([ <<"tcp_connection_count">> | Rest ], _Msg, _Context) ->
    {ok, {tcp_connection_count(), Rest}};

m_get_1([ <<"group_sockets">> | Rest ], _Msg, _Context) ->
    {ok, {group_sockets(), Rest}};

m_get_1([ <<"memory">>, <<"used">> | Rest ], _Msg, _Context) ->
    {ok, {recon_alloc:memory(used), Rest}};
m_get_1([ <<"memory">>, <<"allocated">> | Rest ], _Msg, _Context) ->
    {ok, {recon_alloc:memory(allocated), Rest}};
m_get_1([ <<"memory">>, <<"unused">> | Rest ], _Msg, _Context) ->
    {ok, {recon_alloc:memory(unused), Rest}};
m_get_1([ <<"memory">>, <<"usage">> | Rest ], _Msg, _Context) ->
    {ok, {recon_alloc:memory(usage), Rest}};

m_get_1([ <<"disks">>, <<"alert">> | Rest ], _Msg, _Context) ->
    {ok, {disks_alert(), Rest}};
m_get_1([ <<"disks">> | Rest ], _Msg, _Context) ->
    {ok, {disks(), Rest}};

m_get_1([ <<"task_queue">> | Rest ], _Msg, Context) ->
    case z_pivot_rsc:count_tasks(Context) of
        {ok, Ts} ->
            {ok, {Ts, Rest}};
        {error, _} = Error ->
            Error
    end;

m_get_1([ <<"is_ssl_application_configured">> | Rest ], _Msg, _Context) ->
    IsConf = case application:get_env(ssl, session_lifetime) of
        undefined -> false;
        {ok, _} -> true
    end,
    {ok, {IsConf, Rest}};

m_get_1([ <<"init_arguments">>, <<"config">> | Rest ], _Msg, _Context) ->
    {ok, {proplists:get_all_values(config, init:get_arguments()), Rest}};

m_get_1([ <<"init_arguments">> | Rest ], _Msg, _Context) ->
    {ok, {init:get_arguments(), Rest}};

m_get_1(_Path, _Msg, _Context) ->
    {error, unknown_path}.


%%
%% Helpers
%%

% Return the total number of open tcp connections in the system.
% This includes local sockets.
tcp_connection_count() ->
    length(recon:tcp()).

%% @doc Return the exact OTP version.
-spec otp_version() -> binary().
otp_version() ->
    OtpVersionFile = filename:join([
        code:root_dir(),
        "releases",
        erlang:system_info(otp_release),
        "OTP_VERSION"
    ]),
    {ok, Version} = file:read_file(OtpVersionFile),
    z_string:trim(Version).

%% @doc Return the version string of the used database.
-spec database_version( z:context() ) -> binary().
database_version(Context) ->
    z_db:database_version_string(Context).

% Group open sockets per ip-address, returns a list of proplists.
group_sockets() ->
    Dict = group_sockets(recon:tcp(), dict:new()),
    [[{count, length(Ports)}, {ip, inet:ntoa(IP)}, {ports, Ports}] || {IP, Ports} <- dict:to_list(Dict)].

% Return a dict with as key the ip-address. 
group_sockets([], Dict) ->
    Dict;
group_sockets([Port|Rest], Dict) ->
    case inet:peername(Port) of
        {ok, {Addr, _}} ->
            group_sockets(Rest, dict:append_list(Addr, [Port], Dict));
        _ ->
            group_sockets(Rest, Dict)
    end.

% Close sockets
close_sockets(Max, _Context) ->
    socket_reaper(Max).

% Close sockets of ip-addresses which have 
socket_reaper(Max) ->
    Dict = group_sockets(recon:tcp(), dict:new()),
    socket_reaper(dict:to_list(Dict), Max, 0).

socket_reaper([], _Max, Acc) ->
    Acc;
socket_reaper([{_Ip, Ports}|Rest], Max, Acc) when length(Ports) >= Max ->
    [inet:close(Port) || Port <- Ports],
    socket_reaper(Rest, Max, length(Ports) + Acc);
socket_reaper([_|Rest], Max, Acc) ->
    socket_reaper(Rest, Max, Acc).


%% @doc Return disk space information
-spec disks() -> list( map() ).
disks() ->
    DiskData = disksup:get_disk_data(),
    Threshold = disks_threshold(),
    lists:map(
        fun({Disk, Size, Capacity}) ->
            #{
                disk => unicode:characters_to_binary(Disk),
                size => Size,
                percent_used => Capacity,
                alert => Capacity > Threshold
            }
        end,
        DiskData).

%% @doc Return disk space information
-spec disks_alert() -> boolean().
disks_alert() ->
    DiskData = disksup:get_disk_data(),
    Threshold = disks_threshold(),
    lists:any(
        fun({_Disk, _Size, Capacity}) ->
            Capacity > Threshold
        end,
        DiskData).

%% @doc Return the percentage to be used as threshold.
disks_threshold() ->
    disksup:get_almost_full_threshold().
