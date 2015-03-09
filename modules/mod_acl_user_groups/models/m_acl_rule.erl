%% @author Arjan Scherpenisse <marc@worrell.nl>
%% @copyright 2015 Arjan Scherpenisse
%% Date: 2015-03-09
%%
%% @doc Access to the ACL rules

%% Copyright 2009 Arjan Scherpenisse
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

-module(m_acl_rule).
-author("Arjan Scherpenisse <marc@worrell.nl").

-behaviour(gen_model).

%% interface functions
-export(
   [
    m_find_value/3,
    m_to_list/2,
    m_value/2,

    init/1,
    all_rules/3
   ]).

-include_lib("zotonic.hrl").

-define(valid_acl_kind(T), ((T) =:= rsc orelse (T) =:= module)).
-define(valid_acl_state(T), ((T) =:= edit orelse (T) =:= publish)).


%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(T, M=#m{value=undefined}, _Context) when ?valid_acl_kind(T) ->
    M#m{value=T};
m_find_value(S, #m{value=T}, Context) when ?valid_acl_kind(T), ?valid_acl_state(S) ->
    all_rules(T, S, Context).


%% @spec m_to_list(Source, Context) -> List
m_to_list(_, _Context) ->
    [].

%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
    undefined.


-type acl_rule() :: list().
-spec all_rules(rsc | module, edit | publish, #context{}) -> [acl_rule()].
all_rules(Kind, State, Context) ->
    lager:warning("Kind: ~p", [Kind]),
    lager:warning("State: ~p", [State]),
    [].



init(Context) ->

    case z_db:table_exists(acl_rule_rsc, Context) of
        false ->
            z_db:create_table(acl_rule_rsc,
                              shared_table_columns() ++
                                  [
                                   #column_def{name=category_id, type="integer", is_nullable=true},
                                   #column_def{name=content_group_id, type="integer", is_nullable=true}
                                  ],
                              Context),
            fk("acl_rule_rsc", "creator_id", Context),
            fk("acl_rule_rsc", "modifier_id", Context),
            fk("acl_rule_rsc", "acl_user_group_id", Context),
            fk("acl_rule_rsc", "content_group_id", Context),
            fk("acl_rule_rsc", "category_id", Context),
            ok;
            
        true ->
            ok
    end,

    case z_db:table_exists(acl_rule_module, Context) of
        false ->
            z_db:create_table(acl_rule_module,
                              shared_table_columns() ++
                                  [
                                   #column_def{name=module, type="character varying", length=300, is_nullable=false}
                                  ],
                              Context),
            fk("acl_rule_module", "creator_id", Context),
            fk("acl_rule_module", "modifier_id", Context),
            fk("acl_rule_module", "acl_user_group_id", Context),
            ok;
        true ->
            ok
    end,
    ok.


shared_table_columns() ->
    [
     #column_def{name=id, type="serial", is_nullable=false},
     #column_def{name=is_edit, type="boolean", is_nullable=false, default="false"},
     #column_def{name=creator_id, type="integer", is_nullable=false},
     #column_def{name=modifier_id, type="integer", is_nullable=false},
     #column_def{name=created, type="timestamp", is_nullable=true},
     #column_def{name=modified, type="timestamp", is_nullable=true},
     #column_def{name=acl_user_group_id, type="integer", is_nullable=false},
     #column_def{name=actions, type="character varying", length=300, is_nullable=true}
    ].    

fk(Table, Field, Context) ->
    z_db:equery("alter table " ++ Table ++ " add constraint fk_" ++ Table ++ "_" ++ Field ++ "_id foreign key (" ++ Field ++ ") references rsc(id) on update cascade on delete cascade", Context).
