%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% Date: 2010-10-07
%%
%% @doc Simple store for key/value pairs

%% Copyright 2010 Marc Worrell
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

-module(m_tkvstore).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    get/3,
    put/4,
    delete/3,
    init/1
]).


-include_lib("zotonic.hrl").


%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(Type, #m{value=undefined} = M, _Context) ->
    M#m{value=Type};
m_find_value(Key, #m{value=Type}, Context) ->
    get(Type, Key, Context).

%% @doc Transform a value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> list()
m_to_list(_, _Context) ->
    [].
    
%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{}, _Context) ->
    undefined.


%% @doc Fetch a value from the store
get(Type, Key, Context) ->
    z_db:q1("select props from tkvstore where type = $1 and key = $2", [Type, Key], Context).

%% @doc Put a value into the store
put(Type, Key, Data, Context) ->
    F = fun(Ctx) ->
        case z_db:q1("select count(*) from tkvstore where type = $1 and key = $2", [Type, Key], Ctx) of
            0 ->
                z_db:q("insert into tkvstore (type, key, props) values ($1, $2, $3)", [Type, Key, Data], Ctx);
            1 ->
                z_db:q("update tkvstore set props = $3 where type = $1 and key = $2", [Type, Key, Data], Ctx)
        end
    end,
    z_db:transaction(F, Context).

%% @doc Delete a value from the store
delete(Type, Key, Context) ->
    z_db:q("delete from tkvstore where type = $1 and key = $2", [Type, Key], Context),
    ok.



%% @doc Ensure that the persistent table is present
init(Context) ->
    case z_db:table_exists(tkvstore, Context) of
        true ->
            ok;
        false ->
            z_db:q("
                create table tkvstore (
                    type character varying(32) not null,
                    key character varying(64) not null,
                    props bytea,
                    
                    constraint tkvstore_pkey primary key (type, key) 
                )
                ", Context),
            z_db:flush(Context)
    end.
