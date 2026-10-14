%% @hidden

%% Copyright 2009-2026 Marc Worrell, Arjan Scherpenisse
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

-module(m_rsc_rollback_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([reject_update/3]).

%% A nested resource rejection must roll back its caller's writes, while the
%% outer public API returns a normal error instead of crashing on the wrapper.
resource_callers_test() ->
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    Uri = <<"https://example.org/rollback-authoritative">>,
    {ok, Id} = m_rsc:insert(#{<<"category_id">> => article,
        <<"uri">> => Uri, <<"is_authoritative">> => false}, Context),
    RejectContext = z_context:set(rollback_test_reject, true, Context),
    z_notifier:observe(rsc_update, {?MODULE, reject_update}, 1, Context),
    try
        ?assertEqual({error, rejected}, m_rsc:make_authoritative(Id, RejectContext)),
        ?assertEqual(false, m_rsc:p(Id, is_authoritative, Context)),
        ?assertEqual(Uri, m_rsc:p(Id, uri, Context)),
        ?assertEqual(undefined, z_db:q1("select rsc_id from rsc_uri_alias where uri=$1", [Uri], Context)),
        Png = base64:decode(<<"iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+ip1sAAAAASUVORK5CYII=">>),
        Upload = #upload{filename = <<"rollback.png">>, data = Png},
        ?assertEqual({error, rejected}, m_media:insert_file(Upload,
            #{<<"name">> => <<"rollback_media_test">>}, RejectContext)),
        ?assertEqual(undefined, m_rsc:rid(rollback_media_test, Context)),
        ?assertEqual({rollback, {error, rejected}}, z_db:transaction(fun(Tx) ->
            m_rsc:make_authoritative(Id, Tx),
            error(continued_after_rollback)
        end, RejectContext))
    after
        z_notifier:detach(rsc_update, Context),
        m_rsc:delete(Id, Context)
    end.

expected_version_flush_test() ->
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    {ok, Id} = m_rsc:insert(#{<<"category_id">> => article, <<"title">> => <<"Original">>}, Context),
    try
        Version = m_rsc:p(Id, version, Context),
        NewVersion = Version + 1,
        1 = z_db:q("update rsc set version=$2 where id=$1", [Id, NewVersion], Context),
        ?assertEqual(Version, m_rsc:p(Id, version, Context)),
        ?assertEqual({error, {expected, <<"version">>, NewVersion}},
            m_rsc:update(Id, #{<<"title">> => <<"Rejected">>},
                [{expected, [{<<"version">>, Version}]}], Context)),
        ?assertEqual(NewVersion, m_rsc:p(Id, version, Context)),
        ?assertEqual(<<"Original">>, m_rsc:p(Id, title, Context))
    after
        m_rsc:delete(Id, Context)
    end.

reserved_username_test() ->
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    {ok, Id} = m_rsc:insert(#{<<"category_id">> => person}, Context),
    try
        ?assertEqual({error, eexist}, m_identity:set_username_pw(Id, <<"admin">>, <<"test-password">>, Context)),
        ?assertEqual([], m_identity:get_rsc_by_type(Id, username_pw, Context))
    after
        m_rsc:delete(Id, Context)
    end.

reject_update(#rsc_update{}, Acc, Context) ->
    case z_context:get(rollback_test_reject, Context) of
        true -> {error, rejected};
        _ -> Acc
    end.
