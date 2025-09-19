%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012-2025 Marc Worrell
%% @doc Overview of the revisions of a resource.
%% @end

%% Copyright 2012-2025 Marc Worrell
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
-moduledoc("
Shows the admin backup revisions screen where you can see older version for a [resource](/id/doc_glossary#term-resource).

Todo

Extend documentation
").
-author("Marc Worrell <marc@worrell.nl>").

-export([
    service_available/1,
    is_authorized/1,
    process/4,
    event/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

service_available(Context) ->
    Context1 = z_context:set_noindex_header(Context),
    Context2 = z_context:set_nocache_headers(Context1),
    {true, Context2}.

is_authorized(Context) ->
    case z_acl:is_allowed(use, mod_admin, Context) of
        true ->
            Id = m_rsc:rid(z_context:get_q(<<"id">>, Context), Context),
            case m_rsc:exists(Id, Context) of
                false ->
                    {z_acl:is_allowed(use, mod_backup, Context), Context};
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
event(#submit{message={revert, Args}}, Context) ->
    Context1 = z_render:wire([
            {unmask, [ {body, true} ]},
            {dialog_close, []}
        ], Context),
    RscId = proplists:get_value(rsc_id, Args),
    RevId = proplists:get_value(rev_id, Args),
    case z_acl:is_allowed(use, mod_backup, Context1)
        orelse (m_rsc:exists(RscId, Context1) andalso z_acl:rsc_editable(RscId, Context1))
    of
        true ->
            Options0 = case z_convert:to_bool(z_context:get_q(<<"incoming_edges">>, Context1)) of
                true -> [ incoming_edges ];
                false -> []
            end,
            Options1 = case z_convert:to_bool(z_context:get_q(<<"outgoing_edges">>, Context1)) of
                true -> [ outgoing_edges | Options0 ];
                false -> Options0
            end,
            Options2 = case z_convert:to_bool(z_context:get_q(<<"dependent">>, Context1)) of
                true -> [ dependent | Options1 ];
                false -> Options1
            end,
            do_revert(RscId, RevId, Options2, Context1);
        false ->
            z_render:growl_error(?__("You are not allowed to see the revisions", Context1), Context1)
    end.


do_revert(Id, RevId, Options, Context) ->
    case m_backup_revision:revert_resource(Id, RevId, Options, Context) of
        ok ->
            z_render:wire({redirect, [ {dispatch, admin_edit_rsc}, {id, Id} ]}, Context);
        {error, eacces} ->
            z_render:growl_error(?__("You are not allowed to recover this page.", Context), Context);
        {error, enoent} ->
            z_render:growl_error(?__("Sorry, this backup has been deleted.", Context), Context);
        {error, _} ->
            z_render:growl_error(?__("Sorry, there was an error replacing your page.", Context), Context)
    end.

update_diff(_Id, undefined, undefined, Context) ->
    Context;
update_diff(Id, {ok, A}, undefined, Context) ->
    Vars = [
        {id, Id},
        {a, A},
        {b, undefined},
        {diff, format_diff(A, #{}, Context)}
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
    {ok, #{
        <<"id">> => 0,
        <<"rsc_id">> => Id,
        <<"user_id">> => m_rsc:p(Id, <<"modifier_id">>, Context),
        <<"created">> => m_rsc:p(Id, <<"modified">>, Context),
        <<"version">> => m_rsc:p(Id, <<"version">>, Context),
        <<"data">> => m_rsc:get(Id, Context)
    }};
fetch_props(Id, Rev, Context) ->
    RevId = z_convert:to_integer(Rev),
    case m_backup_revision:get_revision(RevId, Context) of
        {ok, #{ <<"rsc_id">> := RscId }} = Ok when RscId =:= Id ->
            Ok;
        _ ->
            undefined
    end.


check_access(undefined, undefined, _Context) ->
    true;
check_access({ok, PropsA}, undefined, Context) ->
    z_acl:rsc_editable(maps:get(<<"rsc_id">>, PropsA), Context);
check_access(undefined, {ok, PropsA}, Context) ->
    z_acl:rsc_editable(maps:get(<<"rsc_id">>, PropsA), Context);
check_access({ok, PropsA}, {ok, PropsB}, Context) ->
    case {maps:get(<<"rsc_id">>, PropsA),
          maps:get(<<"rsc_id">>, PropsB)}
    of
        {Id,Id} -> z_acl:rsc_editable(Id, Context);
        _ -> false
    end.


format_diff(A, B, Context) ->
    admin_rsc_diff:format(
                    maps:get(<<"data">>, A, undefined),
                    maps:get(<<"data">>, B, undefined),
                    Context).

