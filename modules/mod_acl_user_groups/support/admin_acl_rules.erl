%% @copyright 2015 Arjan Scherpenisse
%% @doc Admin callbacks for the user groups

%% Copyright 2015 Arjan Scherpenisse
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

-module(admin_acl_rules).

-include_lib("include/zotonic.hrl").

-export([event/2]).

event(Msg, Context) ->
    case      z_acl:is_allowed(use, mod_acl_user_groups, Context)
      andalso z_acl:is_allowed(insert, acl_user_group, Context)
    of
        true ->
            event1(Msg, Context);
        false ->
            z_render:growl_error(?__("You are not allowed to perform this action", Context), Context)
    end.

event1(#submit{message={add_rule, [{kind, Kind}]}}, Context) ->
    Row = z_context:get_q_all_noz(Context),
    Row1 = normalize_values(Row),
    {ok, _NewRuleId} = m_acl_rule:insert(Kind, Row1, Context),
    Context;

event1(#submit{message={update_rule, [{id, RuleId}, {kind, Kind}]}}, Context) ->
    Row = z_context:get_q_all_noz(Context),
    Row1 = normalize_values(Row),
    m_acl_rule:update(Kind, RuleId, Row1, Context),
    Context;

event1(#postback{message={remove_rule, [{id, RuleId}, {kind, Kind}]}}, Context) ->
    ok = m_acl_rule:delete(Kind, RuleId, Context),
    Context;

event1(#postback{message={revert, _Args}}, Context) ->
    ok = m_acl_rule:revert(rsc, Context),
    ok = m_acl_rule:revert(module, Context),
    z_render:growl(?__("Revert OK", Context), Context);

event1(#postback{message={publish, _Args}}, Context) ->
    ok = m_acl_rule:publish(rsc, Context),
    ok = m_acl_rule:publish(module, Context),
    z_render:growl(?__("Publish successful", Context), Context);

event1(#postback{message={set_upload_size, [{id,Id}]}}, Context) ->
    NewSize = z_convert:to_integer(z_context:get_q("triggervalue", Context)),
    case m_rsc:p(Id, acl_upload_size, Context) of
        NewSize ->
            Context;
        _OldSize ->
            {ok, _} = m_rsc:update(Id, [{acl_upload_size, NewSize}], Context),
            Context
    end;

event1(#submit{message={acl_rule_import, []}}, Context) ->
    #upload{tmpfile=TmpFile} = z_context:get_q_validated("upload_file", Context),
    {ok, Binary} = file:read_file(TmpFile),
    ContextAsync = z_context:prune_for_async(Context),
    erlang:spawn(fun() ->
                    Data = binary_to_term(Binary),
                    acl_user_groups_export:import(Data, ContextAsync)
                 end),
    z_render:dialog_close(z_render:growl(?__("Importing, the list of rules will refresh after importing.", Context), Context)). 

normalize_values(Row) ->
    {Actions, Rest} =
        lists:foldl(
            fun({"action$" ++ A, "on"}, {Actions, Rest}) ->
                  {[A|Actions], Rest};
               ({"action$"++ _, _}, Acc) ->
                  Acc;
               ({"module", M}, {A,Rest}) ->
                  {A, [{module, M} | Rest]};
               ({"is_owner", V}, {A, Rest}) ->
                  {A, [{is_owner,z_convert:to_bool(V)}|Rest]};
               ({"is_block", V}, {A, Rest}) ->
                  {A, [{is_block,z_convert:to_bool(V)}|Rest]};
               ({K, V}, {A, Rest}) ->
                  {A, [{z_convert:to_atom(K),val(V)}|Rest]}
            end,
            {[], []},
            Row),
    [{actions, string:join(Actions, ",")} | Rest].

val([]) -> undefined;
val(I) -> z_convert:to_integer(I).
