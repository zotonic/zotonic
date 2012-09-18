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
    is_authorized/2,
    event/2
]).

-include_lib("html_controller.hrl").

is_authorized(ReqData, Context0) ->
    Context = ?WM_REQ(ReqData, Context0),
    case z_acl:is_allowed(use, mod_admin, Context) of
        true ->
            Id = list_to_integer(z_context:get_q("id", Context)),
            case m_rsc:exists(Id, Context) of
                false -> 
                    z_acl:wm_is_authorized(true, Context);
                true ->
                    z_acl:wm_is_authorized(z_acl:rsc_editable(Id, Context), Context) 
            end;
        false ->
            z_acl:wm_is_authorized(true, Context)
    end.


html(Context) ->
    Vars = [
        {id, list_to_integer(z_context:get_q("id", Context))},
        {page_admin_backup, true}
    ],
	Html = z_template:render("admin_backup_revision.tpl", Vars, Context),
	z_context:output(Html, Context).


event(#postback_notify{message="rev-diff"}, Context) ->
    A = z_context:get_q("a", Context),
    B = z_context:get_q("b", Context),
    PropsA = fetch_props(A, Context),
    PropsB = fetch_props(B, Context),
    case check_access(PropsA, PropsB, Context) of
        true ->
            update_diff(PropsA, PropsB, Context); 
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


update_diff(undefined, undefined, Context) ->
    z_render:wire({reload, []}, Context);
update_diff({ok, A}, undefined, Context) ->
    Vars = [
        {a, A},
        {b, undefined},
        {diff, format_diff(A, [], Context)}
    ],
    z_render:update("page-diff", #render{template="_admin_backup_diff.tpl", vars=Vars}, Context);
update_diff({ok, A}, {ok, B}, Context) ->
    Vars = [
        {a, A},
        {b, B},
        {diff, format_diff(A, B, Context)}
    ],
    z_render:update("page-diff", #render{template="_admin_backup_diff.tpl", vars=Vars}, Context).


fetch_props("#="++N, Context) ->
    Id = z_convert:to_integer(N),
    {ok, [
            {id, 0},
            {rsc_id, Id},
            {user_id, m_rsc:p(Id, modifier_id, Context)},
            {created, m_rsc:p(Id, modified, Context)},
            {version, m_rsc:p(Id, version, Context)},
            {data, m_rsc:get(Id, Context)}
        ]};
fetch_props("#"++N, Context) ->
    RevId = z_convert:to_integer(N),
    case m_backup_revision:get_revision(RevId, Context) of
        {ok, Row} -> {ok, Row};
        _ -> undefined
    end;
fetch_props(undefined, _Context) ->
    undefined.


check_access(undefined, undefined, _Context) ->
    true;
check_access({ok, PropsA}, undefined, Context) ->
    z_acl:rsc_editable(proplists:get_value(id,PropsA), Context);
check_access(undefined, {ok, PropsA}, Context) ->
    z_acl:rsc_editable(proplists:get_value(id,PropsA), Context);
check_access({ok, PropsA}, {ok, PropsB}, Context) ->
    case {proplists:get_value(rsc_id, PropsA),
          proplists:get_value(rsc_id, PropsB)}
    of
        {Id,Id} -> z_acl:rsc_editable(Id, Context);
        _ -> false
    end.




format_diff(A, B, Context) ->
    backup_format:format(
                    proplists:get_value(data, A),
                    proplists:get_value(data, B),
                    Context).

