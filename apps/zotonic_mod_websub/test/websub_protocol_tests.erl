-module(websub_protocol_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

headers_and_identity_test() ->
    Headers = [{"link", "</id/123>; rel=\"self\", <https://hub.test/>; rel=\"hub\""}],
    ?assertEqual({ok, #{topic => <<"https://source.test/id/123">>, hubs => [<<"https://hub.test/">>]}},
        z_websub_discovery:links(<<"https://source.test/api/model/rsc_export/get/123">>, Headers, <<>>, undefined)),
    ?assertEqual({error, no_websub}, z_websub_discovery:links(<<"https://source.test/">>,
        [{"link", "</id/123>; rel=self, </id/456>; rel=self, </hub>; rel=hub"}], <<>>, undefined)).

html_discovery_test() ->
    Body = <<"<html><head><link rel='hub' href='/hub'><link rel='self' href='/id/1'></head><body><link rel='hub' href='https://evil.test/'></body></html>">>,
    Expected = {ok, #{topic => <<"https://source.test/id/1">>, hubs => [<<"https://source.test/hub">>]}},
    ?assertEqual(Expected, z_websub_discovery:links(<<"https://source.test/page">>, [{"content-type", "text/html"}], Body, undefined)),
    ?assertEqual({error, no_websub}, z_websub_discovery:links(<<"https://source.test/page">>,
        [{"content-type", "text/html"}, {"link", "</other>; rel=alternate"}], Body, undefined)).

xml_discovery_test() ->
    Body = <<"<feed xmlns='http://www.w3.org/2005/Atom'><link rel='self' href='/feed'/><link rel='hub' href='/hub'/></feed>">>,
    ?assertMatch({ok, #{topic := <<"https://source.test/feed">>}},
        z_websub_discovery:links(<<"https://source.test/feed">>, [{"content-type", "application/atom+xml"}], Body, undefined)).

export_discovery_test() ->
    Body = z_json:encode(#{<<"result">> => #{<<"links">> => [
        #{<<"rel">> => <<"self">>, <<"target">> => <<"https://source.test/id/1">>},
        #{<<"rel">> => <<"hub">>, <<"target">> => <<"https://source.test/hub">>}]}}),
    ?assertMatch({ok, #{topic := <<"https://source.test/id/1">>}},
        z_websub_discovery:links(<<"https://source.test/export/1">>, [], Body, undefined)).

signature_test() ->
    Body = <<"payload">>, Secret = <<"secret">>,
    lists:foreach(fun({Name, Algorithm}) ->
        Signature = <<Name/binary, "=", (binary:encode_hex(crypto:mac(hmac, Algorithm, Secret, Body)))/binary>>,
        ?assert(m_websub:verify_push_signature(Signature, Secret, Body)),
        ?assertNot(m_websub:verify_push_signature(Signature, Secret, <<"changed">>))
    end, [{<<"sha1">>, sha}, {<<"sha256">>, sha256}, {<<"sha384">>, sha384}, {<<"sha512">>, sha512}]),
    ?assertNot(m_websub:verify_push_signature(undefined, Secret, Body)),
    ?assertNot(m_websub:verify_push_signature(<<"sha256=oops">>, Secret, Body)),
    ?assertNot(m_websub:verify_push_signature(<<"md5=abcd">>, Secret, Body)).

url_validation_test() ->
    ?assert(z_websub_discovery:is_url(<<"https://example.test/id/1">>)),
    lists:foreach(fun(Url) -> ?assertNot(z_websub_discovery:is_url(Url)) end,
        [<<"file:///etc/passwd">>, <<"ftp://host/file">>, <<"https://user:pass@host/">>, <<"https://host/#fragment">>, undefined]).

renewal_test() ->
    lists:foreach(fun(Lease) ->
        Renew = z_websub_subscription:renewal_seconds(Lease),
        ?assert(Renew > 0), ?assert(Renew < Lease)
    end, [2, 10, 60, 864000]).

redirect_discovery_test() ->
    Topic = <<"https://source.test/id/123">>,
    ok = meck:new(z_websub_http, [passthrough]),
    try
        meck:expect(z_websub_http, fetch, fun(get, Url, _, Options, _) ->
            ?assertEqual(false, proplists:get_value(autoredirect, Options)),
            {error, {303, Url, [{"link", "</id/123>; rel=self, </hub>; rel=hub"},
                {"location", "/export/123"}], 0, <<>>}}
        end),
        ?assertMatch({ok, #{topic := Topic}}, z_websub_discovery:discover(Topic, #context{})),
        meck:expect(z_websub_http, fetch, fun
            (get, TopicUrl, _, _, _) when TopicUrl =:= Topic ->
                {error, {303, TopicUrl, [{"location", "/export/123"}], 0, <<>>}};
            (get, <<"https://source.test/export/123">> = Url, _, _, _) ->
                {ok, {Url, [{"link", "</id/123>; rel=self, </hub>; rel=hub"}], 0, <<>>}}
        end),
        ?assertMatch({ok, #{topic := Topic}}, z_websub_discovery:discover(Topic, #context{}))
    after
        meck:unload(z_websub_http)
    end.

public_destination_test() ->
    lists:foreach(fun(IP) -> ?assertNot(z_websub_http:is_public(IP)) end,
        [{127,0,0,1}, {10,1,2,3}, {100,127,255,254}, {169,254,169,254},
         {192,168,1,1}, {224,0,0,1}, {0,0,0,0}, {0,0,0,0,0,0,0,1},
         {16#fc00,0,0,0,0,0,0,1}, {0,0,0,0,0,16#ffff,16#7f00,1},
         {16#2002,16#7f00,1,0,0,0,0,0}]),
    ?assert(z_websub_http:is_public({93,184,215,14})),
    ?assert(z_websub_http:is_public({16#2606,16#4700,0,0,0,0,0,16#1111})),
    ?assertEqual({error, unsafe_destination},
        z_websub_http:fetch(get, <<"http://127.0.0.1/private">>, <<>>, [], undefined)),
    ?assertEqual({error, unsafe_destination},
        z_websub_http:destination(<<"http://[::ffff:127.0.0.1]/private">>)).

percent_encoded_urls_test() ->
    ?assertEqual(<<"https://example.test/id/1?token=~%2F">>,
        z_websub_discovery:normalize_url(<<"https://example.test/id/%31?token=%7e%2f">>)),
    ?assertMatch({ok, #{topic := <<"https://source.test/id/1">>, hubs := [<<"https://source.test/hub">>]}},
        z_websub_discovery:links(<<"https://source.test/">>,
            [{"link", "</id/%31>; rel=self, </%68ub>; rel=hub"}], <<>>, undefined)).

%% The internal publisher boundary must reject imported resources, even if a
%% caller bypasses the HTTP controller. No database or network is needed here.
non_authoritative_publisher_test() ->
    Context = #context{site = websub_protocol_mock},
    ok = meck:new(m_rsc, [passthrough]),
    ok = meck:new(z_db, [passthrough]),
    try
        meck:expect(m_rsc, p_no_acl, fun
            (123, is_authoritative, _) -> false;
            (Id, Key, Ctx) -> meck:passthrough([Id, Key, Ctx])
        end),
        meck:expect(z_db, q, fun
            ("delete from websub_export where local_rsc_id = $1", [123], _) -> 1;
            (Query, Args, Ctx) -> meck:passthrough([Query, Args, Ctx])
        end),
        meck:expect(z_db, transaction, fun
            (F, #context{site = websub_protocol_mock} = Ctx) -> F(Ctx);
            (F, Ctx) -> meck:passthrough([F, Ctx])
        end),
        meck:expect(z_db, q1, fun
            ("select is_authoritative from rsc where id = $1 for update", [123], #context{site = websub_protocol_mock}) -> false;
            (Query, Args, Ctx) -> meck:passthrough([Query, Args, Ctx])
        end),
        ?assertEqual({error, not_authoritative},
            m_websub:update_export(<<"https://callback.test/">>, <<"https://source.test/topic">>, 123, 600, undefined, Context)),
        ?assertEqual(ok, m_websub:queue_push(123, 2, Context)),
        ?assert(meck:called(z_db, q, ["delete from websub_export where local_rsc_id = $1", [123], Context]))
    after meck:unload(z_db), meck:unload(m_rsc) end.
