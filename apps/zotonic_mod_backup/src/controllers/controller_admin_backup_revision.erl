%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell
%% @doc Overview of the revisions of a resource.

%% Copyright 2012 Marc Worrell
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

-module(controller_admin_backup_revision).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    is_authorized/1,
    process/4,
    event/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

is_authorized(Context) ->
    case z_acl:is_allowed(use, mod_admin, Context) of
        true ->
            Id = m_rsc:rid(z_context:get_q(<<"id">>, Context), Context),
            case m_rsc:exists(Id, Context) of
                false ->
                    {true, Context};
                true ->
                    {z_acl:rsc_editable(Id, Context), Context}
            end;
        false ->
            {true, Context}
    end.


process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    Vars = [
        {id, m_rsc:rid(z_context:get_q(<<"id">>, Context), Context)},
        {page_admin_backup, true}
    ],
	Html = z_template:render("admin_backup_revision.tpl", Vars, Context),
	z_context:output(Html, Context).


event(#postback_notify{message= <<"rev-diff">>}, Context) ->
    Id = m_rsc:rid(z_context:get_q(<<"id">>, Context), Context),
    case z_acl:rsc_editable(Id, Context) of
        true ->
            A = z_context:get_q(<<"a">>, Context),
            B = z_context:get_q(<<"b">>, Context),
            PropsA = fetch_props(Id, A, Context),
            PropsB = fetch_props(Id, B, Context),
            case check_access(PropsA, PropsB, Context) of
                true ->
                    update_diff(Id, PropsA, PropsB, Context);
                false ->
                    z_render:growl_error(?__("You are not allowed to see the revisions", Context), Context)
            end;
        false ->
            z_render:growl_error(?__("You are not allowed to see the revisions", Context), Context)
    end;
event(#postback{message={revert, Args}}, Context) ->
    RscId = proplists:get_value(rsc_id, Args),
    RevId = proplists:get_value(rev_id, Args),
    case z_acl:rsc_editable(RscId, Context) of
        true ->
            do_revert(RscId, RevId, Context);
        false ->
            z_render:growl_error(?__("You are not allowed to see the revisions", Context), Context)
    end.


do_revert(Id, RevId, Context) ->
    case m_backup_revision:get_revision(RevId, Context) of
        {ok, Rev} ->
            case proplists:get_value(rsc_id, Rev) of
                Id ->
                    Props = proplists:get_value(data, Rev),
                    case m_rsc_update:update(Id, Props, [is_import], Context) of
                        {ok, NewId} ->
                            z_render:wire([{redirect, [{dispatch, admin_edit_rsc}, {id,NewId}]}], Context);
                        _Other ->
                            z_render:growl_error(?__("Sorry, there was an error replacing your page.", Context), Context)
                    end;
                _ ->
                    z_render:growl_error(?__("Sorry, this backup is not valid.", Context), Context)
            end;
        _ ->
            z_render:growl_error(?__("Sorry, this backup has been deleted.", Context), Context)
    end.


update_diff(_Id, undefined, undefined, Context) ->
    Context;
update_diff(Id, {ok, A}, undefined, Context) ->
    Vars = [
        {id, Id},
        {a, A},
        {b, undefined},
        {diff, format_diff(A, [], Context)}
    ],
    z_render:update("page-diff", #render{template="_admin_backup_diff.tpl", vars=Vars}, Context);
update_diff(Id, {ok, A}, {ok, B}, Context) ->
    Vars = [
        {id, Id},
        {a, A},
        {b, B},
        {diff, format_diff(A, B, Context)}
    ],
    z_render:update("page-diff", #render{template="_admin_backup_diff.tpl", vars=Vars}, Context).


fetch_props(_Id, undefined, _Context) ->
    undefined;
fetch_props(Id, "latest", Context) ->
    fetch_props(Id, <<"latest">>, Context);
fetch_props(Id, <<"latest">>, Context) ->
    {ok, [
            {id, 0},
            {rsc_id, Id},
            {user_id, m_rsc:p(Id, modifier_id, Context)},
            {created, m_rsc:p(Id, modified, Context)},
            {version, m_rsc:p(Id, version, Context)},
            {data, m_rsc:get(Id, Context)}
        ]};
fetch_props(Id, Rev, Context) ->
    RevId = z_convert:to_integer(Rev),
    case m_backup_revision:get_revision(RevId, Context) of
        {ok, Row} ->
            case proplists:get_value(rsc_id, Row) of
                Id -> {ok, Row};
                _ -> undefined
            end;
        _ ->
            undefined
    end.


check_access(undefined, undefined, _Context) ->
    true;
check_access({ok, PropsA}, undefined, Context) ->
    z_acl:rsc_editable(proplists:get_value(rsc_id,PropsA), Context);
check_access(undefined, {ok, PropsA}, Context) ->
    z_acl:rsc_editable(proplists:get_value(rsc_id,PropsA), Context);
check_access({ok, PropsA}, {ok, PropsB}, Context) ->
    case {proplists:get_value(rsc_id, PropsA),
          proplists:get_value(rsc_id, PropsB)}
    of
        {Id,Id} -> z_acl:rsc_editable(Id, Context);
        _ -> false
    end.


format_diff(A, B, Context) ->
    admin_rsc_diff:format(
                    proplists:get_value(data, A),
                    proplists:get_value(data, B),
                    Context).

