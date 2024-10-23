%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2014-2024 Maas-Maarten Zeeman
%% @doc Process registry interface for a site and proc_lib routines
%% to spawn new processes whilst keeping the logger metadata.
%% @end

%% Copyright 2014-2024 Maas-Maarten Zeeman
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

-module(z_proc).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl").

-export([
    spawn_link_md/1,
    spawn_md/1
]).

-export([
    whereis/2,
    register/3,
    unregister/2
]).

%% Behaviour exports

-export([
    register_name/2,
    unregister_name/1,
    whereis_name/1,
    send/2
    ]).

-include("zotonic.hrl").

%%
%% Api
%%


%% @doc Spawn a new process using proc_lib and copy the logger metadata
%% from the calling process to the new process.
-spec spawn_link_md( Fun ) -> Pid when
    Fun :: function(),
    Pid :: pid().
spawn_link_md(Fun) ->
    MD = logger:get_process_metadata(),
    proc_lib:spawn_link(
        fun() ->
            set_process_metadata(MD),
            Fun()
        end).

%% @doc Spawn a new process using proc_lib and copy the logger metadata
%% from the calling process to the new process.
-spec spawn_md( Fun ) -> Pid when
    Fun :: function(),
    Pid :: pid().
spawn_md(Fun) ->
    MD = logger:get_process_metadata(),
    proc_lib:spawn(
        fun() ->
            set_process_metadata(MD),
            Fun()
        end).

set_process_metadata(undefined) -> ok;
set_process_metadata(MD) -> logger:set_process_metadata(MD).


% @doc Lookup the pid of the process.
%
-spec whereis( term(), atom() | z:context() ) -> undefined | pid().
whereis(Name, Site) when is_atom(Site) ->
    whereis_name(name(Name, Site));
whereis(Name, Context) ->
    whereis_name({Name, Context}).

% @doc Register a Pid under Name. Name can be any erlang term.
%
register(Name, Pid, Site) when is_atom(Site) ->
    register_name(name(Name, Site), Pid);
register(Name, Pid, Context) ->
    register_name({Name, Context}, Pid).


% @doc Unregister a process
%
unregister(Name, Site) when is_atom(Site) ->
    unregister_name(name(Name, Site));
unregister(Name, Context) ->
    unregister_name({Name, Context}).

%%
%% Behaviour callbacks needed to act as a process registry which works
%% with {via, Module, Name}.
%%

register_name({Name, #context{}=Context}, Pid) ->
    register_name(name(Name, Context), Pid);
register_name(Name, Pid) ->
    gproc:register_name({n, l, Name}, Pid).

unregister_name({Name, #context{}=Context}) ->
    unregister_name(name(Name, Context));
unregister_name(Name) ->
    gproc:unregister_name({n, l, Name}).

whereis_name({Name, #context{}=Context}) ->
    whereis_name(name(Name, Context));
whereis_name(Name) ->
    gproc:whereis_name({n, l, Name}).

send({Name, #context{}=Context}, Msg) ->
    send(name(Name, Context), Msg);
send(Name, Msg) ->
    gproc:send({n, l, Name}, Msg).

%%
%% Helpers
%%

name(Name, #context{}=Context) ->
    name(Name, z_context:site(Context));
name(Name, Site) when is_atom(Site) ->
    {Name, Site}.

