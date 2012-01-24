%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011-2012 Marc Worrell
%% @doc Generic handoff administration for zynamo distribution.

%% Copyright 2011-2012 Marc Worrell
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

-module(m_handoff).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,

    handle_handoff/7,
    handoff_check/5,
    
    next_handoff/4,
    delete_handoff/2,
    
    is_local/5,
    insert_local/5,
    delete_local/5,
    
    install/1
]).

-include_lib("zotonic.hrl").
-include_lib("deps/zynamo/include/zynamo.hrl").

m_find_value(_, #m{}, _Context) ->
    undefined.

m_to_list(_, _Context) ->
    [].

m_value(_, _Context) ->
    undefined.

-spec handle_handoff(list( node() ), atom(), atom(), atom(), any(), zynamo_data_version(), #zynamo_command{}) -> ok.
handle_handoff(Nodes, Site, Service, Model, Key, Version, Cmd) ->
    Context = z_context:new(Site),
    OtherNodes = Nodes -- [node()],
    ShouldUpdate =  length(OtherNodes) > 0
                orelse (
                    lists:member(node(), Nodes) 
                    andalso not is_local(Site, Service, Model, Key, Context)
                ),
    case ShouldUpdate of
        true ->
            z_db:transaction(
                fun(Ctx) ->
                    % Register all handoffs to other nodes, iff this handoff is newer than an existing one.
                    [
                        begin
                            {Id, CurrVersion} = handoff_version(Node, Site, Service, Model, Key, Ctx),
                            case zynamo_version:is_newer(Version, CurrVersion) of
                                true ->
                                    case Id of
                                        undefined ->
                                            nop;
                                        _Other ->
                                            z_db:q("delete from handoff_command where id = $1", [Id], Ctx)
                                    end,
                                    handoff_insert(Nodes, Site, Service, Model, Key, Version, Cmd, Ctx);
                                false ->
                                    skip
                            end
                        end
                        || Node <- Nodes, Node /= node()
                    ],
                    % Check if the local node was a destination, if so insert local
                    case lists:member(node(), Nodes) of
                        true ->
                            insert_local(Site, Service, Model, Key, Ctx);
                        false ->
                            nop
                    end,
                    ok
                end,
                Context);
        false ->
            ok
    end.


    handoff_version(Node, Site, Service, Model, Key, Context) when is_integer(Key) ->
        case z_db:q_row("select id, version 
                         from handoff_command
                         where node = $1
                           and site = $2
                           and service = $3
                           and model = $4
                           and key_int = $5",
                         [Node, Site, Service, Model, Key],
                         Context)
        of
            undefined -> {undefined, undefined};
            R -> R
        end;
     handoff_version(Node, Site, Service, Model, Key, Context) ->
         case z_db:q_row("select id, version 
                          from handoff_command
                          where node = $1
                            and site = $2
                            and service = $3
                            and model = $4
                            and key_bin = $5",
                          [Node, Site, Service, Model, z_convert:to_binary(Key)],
                          Context)
        of
            undefined -> {undefined, undefined};
            R -> R
        end.

    handoff_insert(Node, Site, Service, Model, Key, Version, Cmd, Context) ->
        1 = z_db:q("insert into handoff_command
                        (node, site, service, model, key_int, key_bin, version, cmd)
                    values ($1, $2, $3, $4, $5, $6, $7, $8)",
                   [ Node, Site, Service, Model,
                     case is_integer(Key) of true -> Key; false -> undefined end,
                     case is_integer(Key) of true -> undefined; false -> to_binary(Key) end,
                     Version, Cmd
                   ], Context).


-spec handoff_check(node(), atom(), atom(), atom(), function()) -> {ok, done | #zynamo_command{}}.
handoff_check(Node, Host, Service, Model, GetFun) ->
   case m_handoff:next_handoff(Node, Host, config, m_config) of
       {ok, done} ->
            {ok, done};
       {ok, HandoffId, Key, Version, delete} ->
           Command = #zynamo_command{
               command=delete,
               key=Key,
               version=Version,
               handoff_ref=HandoffId
           },
           {ok, Command};
       {ok, HandoffId, Key, Version, put} ->
           case GetFun(Host, Key) of
               {ok, {gone, _Version}, _} ->
                   m_handoff:delete_handoff(Host, HandoffId),
                   handoff_check(Node, Host, Service, Model, GetFun);

               {ok, not_found, _} ->
                   m_handoff:delete_handoff(Host, HandoffId),
                   handoff_check(Node, Host, Service, Model, GetFun);

               {ok, StoredVersion, Data} ->
                   case zynamo_version:is_equal(StoredVersion, Version) of
                       true ->
                           Command = #zynamo_command{
                               command=put,
                               key=Key,
                               version=Version,
                               value=Data,
                               handoff_ref=HandoffId
                           },
                           {ok, Command};
                       false ->
                           m_handoff:delete_handoff(Host, HandoffId),
                           handoff_check(Node, Host, Service, Model, GetFun)
                   end;

               {error, _} ->
                   m_handoff:delete_handoff(Host, HandoffId),
                   handoff_check(Node, Host, Service, Model, GetFun)
           end
   end.



next_handoff(Node, Site, Service, Model) ->
    Context = z_context:new(Site),
    case z_db:q_row("select id, key_int, key_bin, version, cmd
                     from handoff_command
                     where node = $1
                       and site = $2
                       and service = $3
                      and model = $4
                     limit 1",
                     [Node, Site, Service, Model],
                     Context)
    of
        undefined ->
            {ok, done};
        {Id, Key, undefined, Version, Command} ->
            {ok, Id, Key, Version, Command};
        {Id, undefined, Key, Version, Command} ->
            {ok, Id, from_binary(Key), Version, Command}
    end.


delete_handoff(Site, HandoffRef) ->
    Context = z_context:new(Site),
    z_db:q("delete from handoff_command where id = $1", [HandoffRef], Context).


%% @doc Register that a certain value is local
insert_local(Site, Service, Model, Key, Context) ->
    ok = z_db:transaction(
                fun(Ctx) ->
                    case is_local(Site, Service, Model, Key, Ctx) of
                        true -> 
                            ok;
                        false ->
                            1 = z_db:q("insert into handoff_local (site,service,model,key_int,key_bin)
                                        values ($1,$2,$3,$4,$5)", [
                                            Site,
                                            Service,
                                            Model,
                                            case is_integer(Key) of true -> Key; false -> undefined end,
                                            case is_integer(Key) of true -> undefined; false -> to_binary(Key) end
                                       ], Ctx),
                            ok
                    end
                end,
                Context).

%% @doc Register that a stored value is not local to this node. 
%%      A finished handoff will delete the value.
delete_local(Site, Service, Model, Key, Context) when is_integer(Key) ->
    z_db:transaction(
             fun(Ctx) ->
                 case z_db:q("delete from handoff_local
                              where site = $1
                                and service = $2
                                and model = $3
                                and key_int = $4", 
                             [Site, Service, Model, Key], 
                             Ctx) 
                 of
                     0 -> ok;
                     1 -> ok
                 end
             end,
             Context);
delete_local(Site, Service, Model, Key, Context) ->
   z_db:transaction(
            fun(Ctx) ->
                case z_db:q("delete from handoff_local
                             where site = $1
                               and service = $2
                               and model = $3
                               and key_bin = $4", 
                            [Site, Service, Model, to_binary(Key)], 
                            Ctx) 
                of
                    0 -> ok;
                    1 -> ok
                end
            end,
            Context).


%% @doc Test if a stored value is local to this node.
%%      A finished handoff will not delete the value.
is_local(Site, Service, Model, Key, Context) when is_integer(Key) ->
    case z_db:q1("select count(*) 
                  from handoff_local 
                  where site = $1
                    and service = $2
                    and model = $3
                    and key_int = $4", 
                 [Site, Service, Model, Key], Context) 
    of
        0 -> false;
        1 -> true
    end;
is_local(Site, Service, Model, Key, Context) ->
    case z_db:q1("select count(*) 
                  from handoff_local 
                  where site = $1
                    and service = $2
                    and model = $3
                    and key_bin = $4", 
                 [Site, Service, Model, to_binary(Key)], Context) 
    of
        0 -> false;
        1 -> true
    end.



to_binary(T) -> term_to_binary(T).
from_binary(B) -> binary_to_term(B).


%% @doc Create the handoff tables for the nodes
install(Context) ->
    case z_db:table_exists(handoff_command, Context) of
        false ->
            z_db:create_table(handoff_command, [
                                #column_def{name=id,       type="bigserial", is_nullable=false, primary_key=true},
                                #column_def{name=node,     type="varchar",   length=80, is_nullable=false},
                                #column_def{name=site,     type="varchar",   length=64, is_nullable=false},
                                #column_def{name=service,  type="varchar",   length=64, is_nullable=false},
                                #column_def{name=model,    type="varchar",   length=64, is_nullable=false},
                                #column_def{name=key_int,  type="bigint",    is_nullable=true},
                                #column_def{name=key_bin,  type="bytea",     is_nullable=true},
                                #column_def{name=version,  type="bytea",     is_nullable=false},
                                #column_def{name=cmd,      type="varchar",   length=64, is_nullable=false}
                            ], Context),
            z_db:q("alter table handoff_command add constraint handoff_site_int 
                    unique (node, site, service, model, key_int)", Context),
            z_db:q("alter table handoff_command add constraint handoff_site_bin
                    unique (node, site, service, model, key_bin)", Context),
            ok;
        true ->
            ok
    end,
    case z_db:table_exists(handoff_local, Context) of
        false ->
            z_db:create_table(handoff_local, [
                                #column_def{name=id,       type="bigserial", is_nullable=false, primary_key=true},
                                #column_def{name=site,     type="varchar",   length=64, is_nullable=false},
                                #column_def{name=service,  type="varchar",   length=64, is_nullable=false},
                                #column_def{name=model,    type="varchar",   length=64, is_nullable=false},
                                #column_def{name=key_int,  type="bigint",    is_nullable=true},
                                #column_def{name=key_bin,  type="bytea",     is_nullable=true}
                            ], Context),
            z_db:q("alter table handoff_local add constraint handoff_local_key_int 
                    unique (site, service, model, key_int)", Context),
            z_db:q("alter table handoff_local add constraint handoff_local_key_bin 
                    unique (site, service, model, key_bin)", Context),
            ok;
        true ->
            ok
    end.

