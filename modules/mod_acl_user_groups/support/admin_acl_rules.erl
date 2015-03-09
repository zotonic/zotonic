-module(admin_acl_rules).

-include_lib("include/zotonic.hrl").

-export([event/2]).

event(Msg, Context) ->
    case z_acl:is_allowed(use, mod_acl_user_groups, Context) of
        true ->
            event1(Msg, Context);
        false ->
            z_render:growl_error(?__("You are not allowed to perform this action", Context), Context)
    end.

event1(#submit{message={add_rule, [{kind, Kind}]}}, Context) ->
    Row = z_context:get_q_all(Context),
    Row1 = normalize_values(Row),
    {ok, NewRuleId} = m_acl_rule:insert(Kind, Row1, Context),
    Vars = [{kind, Kind}],
    Context1 = z_render:update("acl-rules", #render{template="_acl_rules_list.tpl", vars=Vars}, Context),
    highlight_row(NewRuleId, Context1);

event1(#submit{message={update_rule, [{id, RuleId}, {kind, Kind}]}}, Context) ->
    Row = z_context:get_q_all(Context),
    Row1 = normalize_values(Row),
    m_acl_rule:update(Kind, RuleId, Row1, Context),
    highlight_row(RuleId, Context);

event1(#postback{message={remove_rule, [{id, RuleId}, {kind, Kind}]}}, Context) ->
    ok = m_acl_rule:delete(Kind, RuleId, Context),
    Script = "$('.acl-rule-" ++ z_convert:to_list(RuleId) ++ "').slideUp(function() { $(this).remove(); });",
    z_context:add_script_page(Script, Context),
    Context;

event1(#postback{message={revert, [{kind, Kind}]=V}}, Context) ->
    lager:warning("revert: ~p", [revert]),
    ok = m_acl_rule:revert(Kind, Context),
    Context1 = z_render:update("acl-rules", #render{template="_acl_rules_list.tpl", vars=V}, Context),
    z_render:growl(?__("Revert OK", Context), Context1);

event1(#postback{message={publish, [{kind, Kind}]=V}}, Context) ->
    ok = m_acl_rule:publish(Kind, Context),
    Context1 = z_render:update("acl-rules", #render{template="_acl_rules_list.tpl", vars=V}, Context),
    z_render:growl(?__("Publish successful", Context), Context1).

normalize_values(Row) ->
    {Actions, Rest} =
        lists:foldl(
          fun({"action$" ++ A, "on"}, {Actions, Rest}) ->
                  {[A|Actions], Rest};
             ({"action$"++ _, _}, Acc) ->
                  Acc;
             ({"triggervalue", _}, Acc) ->
                  Acc;
             ({"module", M}, {A,Rest}) ->
                  {A, [{module, M} | Rest]};
             ({K, V}, {A, Rest}) ->
                  {A, [{z_convert:to_atom(K),val(V)}|Rest]}
          end,
          {[], []},
          Row),
    [{actions, string:join(Actions, ",")} | Rest].

val([]) -> undefined;
val(I) -> z_convert:to_integer(I).


highlight_row(RuleId, Context) ->
    Script = "setTimeout(function() { $('.acl-rule-" ++ z_convert:to_list(RuleId) ++ "').animate({backgroundColor: '#ffff99'}, 200).animate({backgroundColor: '#ffffff'}, 1000); }, 10);",
    z_context:add_script_page(Script, Context),
    Context.

