%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2019 Maas-Maarten Zeeman 
%% @doc Zotonic: admin status model

%% Copyright 2019 Maas-Maarten Zeeman
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

-include_lib("include/zotonic.hrl").


%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,

    session_count/1,
    page_count/1,
    tcp_connection_count/0,
    group_sockets/0,
    close_sockets/2
]).

%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(session_count, #m{value=undefined}, Context) ->
    session_count(Context);
m_find_value(page_count, #m{value=undefined}, Context) ->
    page_count(Context);
m_find_value(tcp_connection_count, #m{value=undefined}, _Context) ->
    tcp_connection_count();

m_find_value(group_sockets, #m{value=undefined}, _Context) ->
    group_sockets();

m_find_value(memory, #m{value=undefined} = M, _Context) ->
    M#m{value=memory};
m_find_value(used, #m{value=memory}, _Context) ->
    recon_alloc:memory(used);
m_find_value(allocated, #m{value=memory}, _Context) ->
    recon_alloc:memory(used);
m_find_value(unused, #m{value=memory}, _Context) ->
    recon_alloc:memory(unused);
m_find_value(usage, #m{value=memory}, _Context) ->
    recon_alloc:memory(usage).


%% @spec m_to_list(Source, Context) -> List
m_to_list(_, _Context) ->
    undefined.

%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
    undefined.

%%
%% Helpers
%%

% Return the total number of open tcp connections in the system.
% This includes local sockets.
tcp_connection_count() ->
    length(recon:tcp()).

% Return the number of sessions of this site. 
session_count(Context) ->
    z_session_manager:count(Context).

% Return the number of page processes which are open.
page_count(Context) ->
    z_session_manager:fold(
      fun(S, Acc) ->
              case z_session:get_pages(S) of
                  Pages when is_list(Pages) ->
                      length(Pages) + Acc;
                  _ -> Acc
              end
      end,
      0,
      Context).

% Group open sockets per ip-address, returns a list of proplists.
group_sockets() ->
    Dict = group_sockets(recon:tcp(), dict:new()),
    [[{ip, inet:ntoa(IP)}, {ports, Ports}, {count, length(Ports)}] || {IP, Ports} <- dict:to_list(Dict)].

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

