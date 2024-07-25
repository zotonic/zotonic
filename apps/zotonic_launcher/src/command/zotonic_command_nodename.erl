%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019-2024 Marc Worrell
%% @doc Nodename and netadmin management for the the zotonic command modules.
%% @end

%% Copyright 2019-2024 Marc Worrell
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

-module(zotonic_command_nodename).

%% API
-export([
    nodename_target/1,
    nodename_command/1,

    is_distributed/0
]).

-include("../../include/zotonic_command.hrl").

-spec nodename_target( atom() ) -> {ok, {longnames | shortnames, node()}} | {error, long | short}.
nodename_target(DefaultName) when is_atom(DefaultName) ->
    LongOrShortNames = case is_distributed() of
        true -> longnames;
        false -> shortnames
    end,
    Name = name(DefaultName, LongOrShortNames),
    nodename(Name, LongOrShortNames).

name(DefaultName, longnames) ->
    case os:getenv("LNAME") of
        false -> DefaultName;
        "" -> DefaultName;
        Name -> list_to_atom(Name)
    end;
name(DefaultName, shortnames) ->
    case os:getenv("SNAME") of
        false -> DefaultName;
        "" -> DefaultName;
        Name -> list_to_atom(Name)
    end.

-spec nodename_command( atom() ) -> {ok, {longnames | shortnames, node()}} | {error, long | short}.
nodename_command(Name) when is_atom(Name) ->
    LongOrShortNames = case is_distributed() of
        true -> longnames;
        false -> shortnames
    end,
    nodename(Name, LongOrShortNames).

nodename(Name, LongOrShortNames) when is_atom(Name) ->
    case create_name(Name, LongOrShortNames, 1) of
        {ok, Nodename} ->
            {ok, {LongOrShortNames, Nodename}};
        {error, _} = Error ->
            Error
    end.

is_distributed() ->
    case os:getenv("ZOTONIC_DISTRIBUTED") of
        false -> has_lname();
        "" -> has_lname();
        "true" -> true;
        "1" -> true;
        _ -> false
    end.

has_lname() ->
    case os:getenv("LNAME") of
        false -> false;
        "" -> false;
        _ -> true
    end.

%% Code below is from net_kernel.erl, which is:
%%
%% Copyright Ericsson AB 1996-2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

%% Create the node name
create_name(Name, LongOrShortNames, Try) ->
    {Head, Host1} = create_hostpart(Name, LongOrShortNames),
    case Host1 of
        {ok, HostPart} ->
            {ok, list_to_atom(Head ++ HostPart)};
        {error, long} when Try =:= 1 ->
            %% It could be we haven't read domain name from resolv file yet
            inet_config:do_load_resolv(os:type(), longnames),
            create_name(Name, LongOrShortNames, 0);
        {error, Type} ->
            {error, Type}
    end.

create_hostpart(Name, LongOrShortNames) ->
    {Head, Host} = lists:splitwith(
                    fun
                        ($@) -> false;
                        (_) -> true
                    end,
                    atom_to_list(Name)),
    Host1 = case {Host, LongOrShortNames} of
        {[ $@, _ | _ ], longnames} ->
            {ok, Host};
        {[ $@, _ | _ ], shortnames} ->
            case lists:member($., Host) of
                true ->
                    {error, short};
                _ ->
                    {ok, Host}
            end;
        {_, shortnames} ->
            {ok, "@localhost"};
        {_, longnames} ->
            case {inet_db:gethostname(),inet_db:res_option(domain)} of
                {H, D} when is_list(D), is_list(H), length(D) > 0, length(H) > 0 ->
                    {ok, "@" ++ H ++ "." ++ D};
                _ ->
                    {error, long}
            end
        end,
    {Head, Host1}.

