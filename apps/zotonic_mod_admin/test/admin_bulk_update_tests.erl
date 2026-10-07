%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Test bulk property updates and incoming and outgoing connections.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(admin_bulk_update_tests).
-moduledoc("Integration tests for bulk connection directions, validation, permissions, and duplicate handling.").
-author("Marc Worrell <marc@worrell.nl>").
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([run/1]).

%% @doc Run against an existing local site using temporary resources.
run(Site) ->
    eunit:test(fun() -> bulk_update(z_acl:sudo(z_context:new(list_to_existing_atom(Site)))) end, [verbose]).

%% @doc Run the bulk update integration checks in the CI sandbox.
bulk_update_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    bulk_update(z_acl:sudo(z_context:new(zotonic_site_testsandbox))).

%% @doc Verify connection directions, unchanged existing edges, and validation before writes.
bulk_update(Context) ->
    Ids = [begin
        {ok, Id} = m_rsc:insert(#{<<"category_id">> => text, <<"is_published">> => true,
            <<"is_featured">> => false, <<"title">> => <<"Bulk connection test">>}, Context),
        Id
    end || _ <- lists:seq(1, 4)],
    [A, B, Subject, Object] = Ids,
    Fields = #{<<"bulk_subject_id">> => integer_to_binary(Subject),
        <<"bulk_subject_predicate">> => <<"relation">>,
        <<"bulk_object_id">> => integer_to_binary(Object),
        <<"bulk_object_predicate">> => <<"author">>, <<"is_featured">> => true},
    try
        ?assertEqual({error, connections}, z_admin_bulk_update:update([A],
            Fields#{<<"bulk_subject_predicate">> => <<>>}, Context)),
        ?assertEqual({error, connections}, z_admin_bulk_update:update([A],
            Fields#{<<"bulk_object_id">> => <<>>}, Context)),
        ?assertEqual({error, connections}, z_admin_bulk_update:update([A],
            Fields#{<<"bulk_object_predicate">> => integer_to_binary(Object)}, Context)),
        ?assertEqual({error, connections}, z_admin_bulk_update:update([A],
            Fields#{<<"bulk_object_id">> => #{}}, Context)),
        ?assertEqual(false, m_rsc:p(A, is_featured, Context)),
        ?assertEqual([], m_edge:objects(Subject, relation, Context)),
        {ok, Existing} = m_edge:insert(A, relation, B, Context),
        ?assertEqual({ok, 0}, z_admin_bulk_update:update([A, B, A], Fields, Context)),
        lists:foreach(fun(Id) ->
            ?assertEqual(true, m_rsc:p(Id, is_featured, Context)),
            ?assert(is_integer(m_edge:get_id(Subject, relation, Id, Context))),
            ?assert(is_integer(m_edge:get_id(Id, author, Object, Context))),
            ?assertEqual(undefined, m_rsc:p(Id, bulk_subject_id, Context)),
            ?assertEqual(undefined, m_rsc:p(Id, bulk_object_predicate, Context))
        end, [A, B]),
        ?assertEqual(Existing, m_edge:get_id(A, relation, B, Context)),
        Edge = m_edge:get_id(A, author, Object, Context),
        ?assertEqual({ok, 0}, z_admin_bulk_update:update([A, B], Fields, Context)),
        ?assertEqual(Edge, m_edge:get_id(A, author, Object, Context)),
        ?assertEqual({ok, 0}, z_admin_bulk_update:update([A], #{<<"is_featured">> => false,
            <<"bulk_subject_id">> => <<>>, <<"bulk_subject_predicate">> => <<>>}, Context)),
        ?assertEqual(false, m_rsc:p(A, is_featured, Context)),
        ReadOnly = Context#context{acl = undefined, acl_is_read_only = true},
        ?assertEqual({ok, 1}, z_admin_bulk_update:update([A], #{<<"is_featured">> => true}, ReadOnly)),
        ?assertEqual(false, m_rsc:p(A, is_featured, Context)),
        ?assertEqual({ok, 1}, z_admin_bulk_update:update([A], #{
            <<"bulk_object_id">> => integer_to_binary(Subject),
            <<"bulk_object_predicate">> => <<"relation">>}, ReadOnly)),
        ?assertEqual(undefined, m_edge:get_id(A, relation, Subject, Context)),
        {ok, _} = z_template:template_module(<<"_dialog_admin_bulk_update.tpl">>, #{}, Context),
        {Html, _} = z_template:render_to_iolist(<<"_dialog_admin_bulk_update.tpl">>, #{}, Context),
        HtmlBin = iolist_to_binary(Html),
        lists:foreach(fun(Name) ->
            ?assertNotEqual(nomatch, binary:match(HtmlBin, <<"name=\"", Name/binary, "\"">>))
        end, [<<"bulk_subject_id">>, <<"bulk_subject_predicate">>,
            <<"bulk_object_id">>, <<"bulk_object_predicate">>])
    after
        [m_rsc:delete(Id, Context) || Id <- Ids]
    end.
