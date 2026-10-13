%% @copyright 2026 Marc Worrell

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

-module(import_feed_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

%% These tests mock shared core modules. Run in a separate VM so that loading
%% and unloading mocks cannot crash the site used by the integration suite.
-export([
    start_dependencies/0,
    oldest_first_check/0,
    rss_full_content_check/0,
    atom_base_and_xhtml_check/0,
    rdf_check/0,
    discovery_relative_check/0,
    identity_check/0,
    xml_safety_check/0,
    url_check/0,
    wordpress_endpoint_check/0,
    rss_timezone_check/0,
    header_discovery_check/0,
    fetch_policy_check/0,
    largest_srcset_check/0,
    srcset_density_and_fallback_check/0,
    repeated_image_variants_check/0,
    wordpress_original_media_check/0,
    import_user_permissions_check/0,
    admin_poll_is_blocked_check/0,
    copyright_defaults_check/0,
    dependent_media_import_check/0,
    category_guess_excludes_meta_check/0,
    lazy_images_and_embeds_check/0,
    summary_entities_check/0,
    article_links_check/0,
    removal_patterns_check/0,
    delete_feed_content_permissions_check/0,
    fetch_resource_permissions_check/0,
    fetch_resource_pipeline_check/0,
    zotonic_atom_detection_check/0,
    zotonic_import_options_check/0,
    zotonic_native_import_check/0,
    zotonic_native_cleanup_check/0,
    fetch_credentials_and_development_check/0,
    configured_fetch_user_check/0,
    feed_type_detection_check/0
]).

isolated_test_() ->
    {setup,
        fun() ->
            {ok, Peer, _} = peer:start_link(#{
                connection => standard_io,
                args => ["-pa" | code:get_path()]
            }),
            ok = peer:call(Peer, ?MODULE, start_dependencies, []),
            Peer
        end,
        fun peer:stop/1,
        fun(Peer) ->
            [{atom_to_list(Test), {timeout, 30, fun() ->
                peer:call(Peer, ?MODULE, Test, [], 30000)
            end}} || Test <- [
        oldest_first_check,
        rss_full_content_check,
        atom_base_and_xhtml_check,
        rdf_check,
        discovery_relative_check,
        identity_check,
        xml_safety_check,
        url_check,
        wordpress_endpoint_check,
        rss_timezone_check,
        header_discovery_check,
        fetch_policy_check,
        largest_srcset_check,
        srcset_density_and_fallback_check,
        repeated_image_variants_check,
        wordpress_original_media_check,
        import_user_permissions_check,
        admin_poll_is_blocked_check,
        copyright_defaults_check,
        dependent_media_import_check,
        category_guess_excludes_meta_check,
        lazy_images_and_embeds_check,
        summary_entities_check,
        article_links_check,
        removal_patterns_check,
        delete_feed_content_permissions_check,
        fetch_resource_permissions_check,
        fetch_resource_pipeline_check,
        zotonic_atom_detection_check,
        zotonic_import_options_check,
        zotonic_native_import_check,
        zotonic_native_cleanup_check,
        fetch_credentials_and_development_check,
        configured_fetch_user_check,
        feed_type_detection_check
            ]]
        end}.

start_dependencies() ->
    {ok, _} = application:ensure_all_started(mimetypes),
    ok.


oldest_first_check() ->
    Old = #{id => old, published => {{2026,10,10},{8,0,0}}, modified => {{2026,10,14},{8,0,0}}},
    Middle = #{id => middle, published => undefined, modified => {{2026,10,11},{8,0,0}}},
    New = #{id => new, published => {{2026,10,12},{8,0,0}}},
    Sorted = [Old, Middle, New],
    ?assertEqual(Sorted, z_import_feed_poll:sort_entries([New, Middle, Old])),
    ?assertEqual(Sorted, z_import_feed_poll:sort_entries(Sorted)),
    ?assertEqual(Sorted, z_import_feed_poll:sort_entries([Middle, New, Old])),
    Equal = Old#{id := equal},
    Undated = #{id => undated, published => undefined, modified => undefined},
    Missing = #{id => missing},
    ?assertEqual([Old, Equal, New, Undated, Missing],
        z_import_feed_poll:sort_entries([Undated, New, Old, Missing, Equal])),
    ?assertEqual([], z_import_feed_poll:sort_entries([])).

rss_full_content_check() ->
    {ok, F} = parse(<<"<rss xmlns:c='http://purl.org/rss/1.0/modules/content/'><channel><title>News</title><item><title>A &amp; B</title><link>https://example.com/a</link><guid isPermaLink='false'>https://example.com/?p=1</guid><description>Short</description><c:encoded><![CDATA[<p>Full <img src='/photo.jpg'></p>]]></c:encoded><pubDate>Mon, 12 Oct 2026 08:11:31 +0000</pubDate><category>News</category></item></channel></rss>">>),
    [E] = maps:get(entries, F),
    ?assertEqual(<<"A & B">>, maps:get(title, E)),
    ?assertEqual(<<"https://example.com/?p=1">>, maps:get(uri, E)),
    ?assertEqual(<<"<p>Full <img src='/photo.jpg'></p>">>, maps:get(body, E)),
    ?assertEqual([<<"https://example.com/photo.jpg">>], maps:get(media, E)),
    ?assertEqual({{2026,10,12},{8,11,31}}, maps:get(published, E)).

atom_base_and_xhtml_check() ->
    {ok, F} = parse(<<"<feed xmlns='http://www.w3.org/2005/Atom' xml:base='https://example.com/blog/'><title>News</title><link rel='hub' href='/hub'/><entry xml:base='posts/'><id>urn:uuid:123</id><title>Entry</title><link href='one'/><author><name>Ada</name></author><published>2026-10-12T10:00:00+02:00</published><content type='xhtml'><div xmlns='http://www.w3.org/1999/xhtml'><p>Hello</p></div></content><category term='Culture'/><link rel='enclosure' href='one.jpg'/></entry></feed>">>),
    [E] = maps:get(entries, F),
    ?assertEqual(<<"https://example.com/blog/posts/one">>, maps:get(url, E)),
    ?assertEqual(<<"urn:uuid:123">>, maps:get(uri, E)),
    ?assertEqual(<<"Ada">>, maps:get(author, E)),
    ?assertEqual({{2026,10,12},{8,0,0}}, maps:get(published, E)),
    ?assertNotEqual(nomatch, binary:match(maps:get(body, E), <<"<p>Hello</p>">>)),
    ?assertEqual([<<"https://example.com/hub">>], maps:get(hubs, F)).

rdf_check() ->
    {ok, F} = parse(<<"<rdf:RDF xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'><channel><title>RDF</title></channel><item rdf:about='https://example.com/1'><title>One</title><description>Body</description></item></rdf:RDF>">>),
    [E] = maps:get(entries, F),
    ?assertEqual(<<"https://example.com/1">>, maps:get(uri, E)).

discovery_relative_check() ->
    {ok, [#{url := U, title := <<"Articles">>}]} = z_import_feed_parse:discover(
        <<"<!doctype html><html><head><base href='/blog/'><link rel='alternate' type='application/rss+xml' title='Articles' href='feed/'></head><body></body></html>">>,
        <<"https://example.com/">>),
    ?assertEqual(<<"https://example.com/blog/feed/">>, U).

identity_check() ->
    A = z_import_feed_parse:identity(<<"opaque-id">>, <<"https://example.com/a">>, <<"https://example.com/feed/">>),
    ?assertEqual(A, z_import_feed_parse:identity(<<"opaque-id">>, <<"https://example.com/renamed">>, <<"https://example.com/category/feed/">>)),
    ?assertNotEqual(A, z_import_feed_parse:identity(<<"opaque-id">>, <<>>, <<"https://other.example/feed/">>)),
    ?assertEqual(undefined, z_import_feed_parse:identity(<<>>, <<>>, <<"https://example.com/feed/">>)).

xml_safety_check() ->
    ?assertEqual({error, forbidden_xml_declaration}, parse(<<"<!DOCTYPE rss [<!ENTITY x SYSTEM 'file:///etc/passwd'>]><rss/>">>)),
    ?assertMatch({error, _}, parse(<<"<html><body>Not a feed</body></html>">>)).

url_check() ->
    ?assertNot(z_import_feed_http:url(<<"file:///tmp/file">>)),
    ?assertNot(z_import_feed_http:url(<<"https://user:secret@example.com/feed">>)),
    ?assertEqual(<<>>, z_import_feed_http:resolve(<<"javascript:alert(1)">>, <<"https://example.com/">>)),
    ?assert(z_import_feed_http:url(<<"https://example.com/feed/">>)).

wordpress_endpoint_check() ->
    ?assertEqual(<<"https://example.com/wp-json/wp/v2/posts/12?_embed=1">>,
        z_import_feed_wordpress:endpoint(<<"https://example.com/?p=12">>)),
    ?assertEqual(<<"https://example.com/blog/wp-json/wp/v2/posts/12?_embed=1">>,
        z_import_feed_wordpress:endpoint(<<"https://example.com/blog/?p=12">>)),
    ?assertEqual(undefined, z_import_feed_wordpress:endpoint(<<"https://example.com/?p=oops">>)).

parse(B) -> z_import_feed_parse:parse(B, <<"https://example.com/feed/">>).

rss_timezone_check() ->
    ?assertEqual({{2026,10,12},{8,0,0}}, z_import_feed_parse:date(<<"Mon, 12 Oct 2026 10:00:00 +0200">>)).

header_discovery_check() ->
    {ok, [#{url := <<"https://example.com/news.xml">>}]} = z_import_feed_parse:discover(
        <<"<html><head></head><body></body></html>">>, <<"https://example.com/">>,
        [{"content-type", "text/html"}, {"link", "</news.xml>; rel=\"alternate\"; type=\"application/rss+xml\""}]).

fetch_policy_check() ->
    ?assertEqual({error, unsafe_destination}, z_import_feed_http:get(<<"file:///feed">>, undefined)),
    meck:new(z_fetch, [no_link]),
    try
        meck:expect(z_fetch, fetch, fun(_, Options, undefined) ->
            ?assertEqual(none, proplists:get_value(authorization, Options)),
            ?assertEqual(undefined, proplists:get_value(autoredirect, Options)),
            ?assertEqual(undefined, proplists:get_value(insecure, Options)),
            ?assertEqual(none, proplists:get_value(authorization_redirect, Options)),
            ?assertEqual(15000, proplists:get_value(timeout, Options)),
            {error, timeout}
        end),
        %% Destination and TLS policy are delegated to the shared fetcher.
        ?assertEqual({error, timeout}, z_import_feed_http:get(<<"http://127.0.0.1/feed">>, undefined)),
        meck:expect(z_fetch, fetch, fun(Url, _, _) -> {ok, {Url, [], 4, <<"data">>}} end),
        ?assertEqual({error, response_too_large}, z_import_feed_http:get(<<"https://example.com/feed">>, 4, undefined))
    after meck:unload(z_fetch) end.

largest_srcset_check() ->
    Html = <<"<img src='/small.jpg' srcset='/large.jpg 1600w, /small.jpg 320w, /medium.jpg 800w'>">>,
    {Urls, Aliases} = z_import_feed_parse:image_variants(Html, <<"https://example.com/">>),
    ?assertEqual([<<"https://example.com/large.jpg">>], Urls),
    ?assertEqual(<<"https://example.com/large.jpg">>, maps:get(<<"https://example.com/small.jpg">>, Aliases)),
    ?assertEqual(3, map_size(Aliases)).

srcset_density_and_fallback_check() ->
    ?assertEqual([<<"https://example.com/3x.jpg">>], z_import_feed_parse:images(
        <<"<img src='/1x.jpg' srcset='/2x.jpg 2x, /3x.jpg 3x, /1x.jpg 1x'>">>, <<"https://example.com/">>)),
    ?assertEqual([<<"https://example.com/fallback.jpg">>], z_import_feed_parse:images(
        <<"<img src='/fallback.jpg' srcset='javascript:bad 4000w, /invalid.jpg nope'>">>, <<"https://example.com/">>)),
    ?assertEqual([<<"https://example.com/only.jpg">>], z_import_feed_parse:images(
        <<"<img srcset='/only.jpg 2x'>">>, <<"https://example.com/">>)).

repeated_image_variants_check() ->
    Html = <<"<img src='/small.jpg' srcset='/small.jpg 320w, /medium.jpg 800w'><img src='/small.jpg' srcset='/small.jpg 320w, /large.jpg 1600w'>">>,
    {Urls, Aliases} = z_import_feed_parse:image_variants(Html, <<"https://example.com/">>),
    ?assertEqual([<<"https://example.com/large.jpg">>], Urls),
    ?assertEqual(<<"https://example.com/large.jpg">>, maps:get(<<"https://example.com/medium.jpg">>, Aliases)).

wordpress_original_media_check() ->
    {ok, F} = parse(<<"<rss><channel><item><guid>https://example.com/?p=123</guid><link>https://example.com/article</link><title>Article</title><description>Summary</description></item></channel></rss>">>),
    [E] = maps:get(entries, F),
    Original = <<"https://example.com/original.jpg">>,
    Medium = <<"https://example.com/medium.jpg">>,
    Post = #{<<"id">> => 123,
        <<"content">> => #{<<"rendered">> => <<"<p>Full</p><img src='/small.jpg' srcset='/small.jpg 320w, /medium.jpg 1200w'>">>},
        <<"date_gmt">> => <<"2026-10-12T08:00:00">>, <<"modified_gmt">> => <<"2026-10-12T09:00:00">>,
        <<"_embedded">> => #{<<"author">> => [#{<<"name">> => <<"Patricia">>}],
            <<"wp:featuredmedia">> => [#{<<"source_url">> => Original,
                <<"media_details">> => #{<<"sizes">> => #{<<"large">> => #{<<"source_url">> => Medium}}}}]}},
    Json = z_json:encode(Post),
    meck:new(z_import_feed_http, [passthrough]),
    try
        meck:expect(z_import_feed_http, get, fun(Url, _) -> {ok, {Url, [], byte_size(Json), Json}} end),
        Result = z_import_feed_wordpress:enrich(E, #context{}),
        ?assertEqual([Original], maps:get(media, Result)),
        ?assertEqual(Original, maps:get(<<"https://example.com/small.jpg">>, maps:get(media_aliases, Result))),
        ?assertEqual(<<"Patricia">>, maps:get(author, Result)),
        ?assertEqual({{2026,10,12},{9,0,0}}, maps:get(modified, Result)),
        ?assertMatch(#{feed := _, wordpress := _}, maps:get(raw, Result))
    after meck:unload(z_import_feed_http) end.

import_user_permissions_check() ->
    %% Exercise the save boundary, including forged form fields and ordinary edits.
    Modules = [z_acl, z_context, z_db, m_rsc, m_identity, m_category, z_auth, z_module_manager],
    lists:foreach(fun(M) -> meck:new(M, [no_link]) end, Modules),
    Self = self(),
    Existing = #{id => 7, url => <<"https://example.org/feed">>, user_id => 9,
        user_groups => term_to_binary([90]), connections => term_to_binary([])},
    Input = #{<<"url">> => <<"https://example.org/feed">>},
    Editor = #context{props = #{user => 2, admin => false, accounts => false, groups => [20]}},
    try
        meck:expect(z_module_manager, active, fun(_, _) -> false end),
        meck:expect(z_acl, is_allowed, fun
            (use, mod_import_feed, _) -> true;
            (use, mod_admin_identity, C) -> maps:get(accounts, C#context.props);
            (insert, _, _) -> true
        end),
        meck:expect(z_acl, is_admin, fun(C) -> maps:get(admin, C#context.props) end),
        meck:expect(z_acl, is_read_only, fun(C) -> maps:get(readonly, C#context.props, false) end),
        meck:expect(z_acl, user, fun(C) -> maps:get(user, C#context.props) end),
        meck:expect(z_acl, user_groups, fun(C) -> maps:get(groups, C#context.props) end),
        meck:expect(z_acl, logon, fun(U, C) -> with_props(#{user => U, groups => [U * 10], admin => lists:member(U, [1,6])}, C) end),
        meck:expect(z_context, new, fun(C) -> C end),
        meck:expect(m_rsc, rid, fun
            (<<"article">>, _) -> 100;
            (B, _) when is_binary(B) -> binary_to_integer(B);
            (I, _) -> I
        end),
        meck:expect(m_rsc, exists, fun(I, _) -> lists:member(I, [1,2,3,4,5,6,9]) end),
        meck:expect(m_rsc, is_a, fun
            (100, category, _) -> true;
            (200, predicate, _) -> true;
            (_, _, _) -> false
        end),
        meck:expect(z_acl, rsc_visible, fun(I, _) -> I =/= 6 end),
        meck:expect(m_identity, is_user, fun(I, _) -> I =/= 4 end),
        meck:expect(z_auth, is_enabled, fun(I, _) -> I =/= 5 end),
        meck:expect(m_category, is_a, fun(_, _, _) -> false end),
        meck:expect(m_category, id_to_name, fun(_, _) -> article end),
        meck:expect(z_db, qmap, fun(_, [7], _, _) -> {ok, [Existing]} end),
        meck:expect(z_db, update, fun(import_feed, 7, P, _) -> Self ! {saved_user, P}, {ok,1} end),
        meck:expect(z_db, insert, fun(import_feed, P, _) -> Self ! {saved_user, P}, {ok,8} end),
        ?assertEqual({ok,{200,3}}, m_import_feed:validate_connection(200,3,Editor)),
        ?assertEqual({error,invalid_connections}, m_import_feed:validate_connection(200,6,Editor)),
        ?assertEqual({error,invalid_connections}, m_import_feed:validate_connection(100,3,Editor)),
        ?assertEqual({error,eacces}, m_import_feed:validate_connection(200,3,with_props(#{readonly => true}, Editor))),
        ?assertEqual({ok,7}, m_import_feed:save(7, Input#{<<"connections">> => [<<"200=3">>, <<"200=3">>]}, Editor)),
        receive {saved_user, ConnectionProps} ->
            ?assertEqual([{200,3}], binary_to_term(maps:get(connections, ConnectionProps)))
        after 1000 -> error(no_save) end,
        ?assertEqual({error,invalid_settings}, m_import_feed:save(7, Input#{<<"connections">> => [<<"200=6">>]}, Editor)),
        meck:expect(z_module_manager, active, fun(_, _) -> true end),
        ?assertEqual({ok,7}, m_import_feed:save(7, Input#{<<"rights">> => <<"BY,SA">>,
            <<"rights_attribution">> => <<" Publisher ">>, <<"rights_year">> => <<"2026">>}, Editor)),
        receive {saved_user, Copyright} ->
            ?assertEqual(<<"BY,SA">>, maps:get(rights, Copyright)),
            ?assertEqual(<<"Publisher">>, maps:get(rights_attribution, Copyright)),
            ?assertEqual(<<"2026">>, maps:get(rights_year, Copyright))
        after 1000 -> error(no_save) end,
        ?assertEqual({error,invalid_settings}, m_import_feed:save(7, Input#{<<"rights">> => <<"invalid">>}, Editor)),
        meck:expect(z_module_manager, active, fun(_, _) -> false end),
        ?assertMatch({error,{invalid_regexp,1,_}},
            m_import_feed:save(7, Input#{<<"remove_patterns">> => <<"[">>}, Editor)),
        ?assertNot(m_import_feed:can_select_user(Editor)),
        ?assertEqual({error,eacces}, m_import_feed:save(7, Input#{<<"user_id">> => 3}, Editor)),
        ?assertEqual({error,eacces}, m_import_feed:save(undefined, Input#{<<"user_id">> => 3}, Editor)),
        ?assertEqual({ok,7}, m_import_feed:save(7, Input, Editor)),
        receive {saved_user, P1} ->
            ?assertNot(maps:is_key(rights, P1)),
            ?assertNot(maps:is_key(user_id, P1)),
            ?assertNot(maps:is_key(user_groups, P1))
        after 1000 -> error(no_save) end,
        ?assertEqual({ok,7}, m_import_feed:save(7, Input#{<<"user_id">> => <<"9">>}, Editor)),
        receive {saved_user, SameUser} ->
            ?assertNot(maps:is_key(user_id, SameUser)),
            ?assertNot(maps:is_key(user_groups, SameUser))
        after 1000 -> error(no_save) end,
        ?assertEqual({ok,8}, m_import_feed:save(undefined, Input, Editor)),
        receive {saved_user, P2} ->
            ?assertEqual(2, maps:get(user_id, P2)),
            ?assertEqual([20], binary_to_term(maps:get(user_groups, P2)))
        after 1000 -> error(no_save) end,
        lists:foreach(fun(Authorized) ->
            ?assert(m_import_feed:can_select_user(Authorized)),
            ?assertEqual({error,admin_import_user}, m_import_feed:validate_user(1, Authorized)),
            ?assertEqual({error,admin_import_user}, m_import_feed:validate_user(6, Authorized)),
            ?assertEqual({error,admin_import_user}, m_import_feed:save(undefined, Input, with_props(#{user => 1}, Authorized))),
            ?assertEqual({error,admin_import_user}, m_import_feed:save(7, Input#{<<"user_id">> => 6}, Authorized)),
            ?assertEqual({error,invalid_import_user}, m_import_feed:validate_user(4, Authorized)),
            ?assertEqual({error,invalid_import_user}, m_import_feed:validate_user(5, Authorized)),
            ?assertEqual({error,eacces}, m_import_feed:validate_user(3, with_props(#{readonly => true}, Authorized))),
            ?assertEqual({ok,7}, m_import_feed:save(7, Input#{<<"user_id">> => <<"3">>}, Authorized)),
            receive {saved_user, P3} ->
                ?assertEqual(3, maps:get(user_id, P3)),
                ?assertEqual([30], binary_to_term(maps:get(user_groups, P3)))
            after 1000 -> error(no_save) end
        end, [with_props(#{admin => true}, Editor), with_props(#{accounts => true}, Editor)]),
        %% A previously safe account promoted to admin must not keep importing.
        meck:expect(z_acl, logon, fun(U, C) -> with_props(#{user => U, admin => U =:= 9}, C) end),
        ?assertEqual({error,admin_import_user}, m_import_feed:save(7, Input, Editor)),
        ?assertEqual({error,admin_import_user}, m_import_feed:queue(7, true, Editor)),
        ?assertEqual({error,admin_import_user}, m_import_feed:pause(7, false, Editor))
    after
        lists:foreach(fun meck:unload/1, Modules)
    end.

admin_poll_is_blocked_check() ->
    %% Existing persisted feeds must be blocked before any external request or import.
    meck:new(z_db, [no_link]),
    meck:new(z_import_feed_http, [no_link]),
    meck:new(m_import_feed, [passthrough, no_link]),
    try
        F = #{id => 1, user_id => 1, connections => term_to_binary([]),
            user_groups => term_to_binary([]), lease_token => <<"test">>,
            modified => {{2026,10,12},{0,0,0}}},
        meck:expect(z_db, qmap, 4, meck:seq([{ok,[F]}, {ok,[]}])),
        Self = self(),
        meck:expect(z_db, q, fun(_, Args, _) -> Self ! {poll_finished, Args}, [] end),
        meck:expect(m_import_feed, error_message, fun(admin_import_user, _) -> <<"Administrator imports blocked">> end),
        ?assertEqual(ok, z_import_feed_poll:run(#context{})),
        receive {poll_finished, [1,0,0,0,1,Message,_,_]} ->
            ?assertEqual(<<"Administrator imports blocked">>, Message)
        after 1000 -> error(no_poll_result) end,
        ?assertEqual(0, meck:num_calls(z_import_feed_http, get, '_'))
    after
        meck:unload(m_import_feed),
        meck:unload(z_import_feed_http),
        meck:unload(z_db)
    end.

copyright_defaults_check() ->
    meck:new(m_category, [no_link]),
    meck:expect(m_category, is_a, fun(image, media, _) -> true end),
    meck:new(z_module_manager, [no_link]),
    meck:new(m_rsc, [no_link]),
    try
        meck:expect(z_module_manager, active, fun(mod_copyright, _) -> true end),
        meck:expect(m_rsc, rid, fun(depiction, _) -> 102; (image, _) -> 103 end),
        {ok, Parsed} = parse(<<"<rss><channel><item><title>Article</title><link>https://example.com/a</link><description>&lt;img src='https://example.com/a.jpg'&gt;</description></item></channel></rss>">>),
        [Entry] = maps:get(entries, Parsed),
        Feed = #{category_id => 100, content_group_id => undefined,
            is_authoritative => false, media_mode => <<"dependent">>, connections => [],
            rights => <<"CR">>, rights_attribution => <<"Publisher">>, rights_year => <<"2026">>},
        {Item, [Media]} = z_import_feed_import:prepare(Entry, Feed, #context{db = {test, test}}),
        ?assertEqual(103, maps:get(<<"category_id">>, maps:get(props, Media))),
        lists:foreach(fun(#{props := Props}) ->
            ?assertEqual(<<"CR">>, maps:get(<<"rights">>, Props)),
            ?assertEqual(<<"Publisher">>, maps:get(<<"rights_attribution">>, Props)),
            ?assertEqual(<<"2026">>, maps:get(<<"rights_year">>, Props))
        end, [Item, Media]),
        {Empty, _} = z_import_feed_import:prepare(Entry, maps:without([rights, rights_attribution, rights_year], Feed), #context{db = {test, test}}),
        ?assertNot(maps:is_key(<<"rights">>, maps:get(props, Empty))),
        meck:expect(z_module_manager, active, fun(_, _) -> false end),
        {Disabled, [DisabledMedia]} = z_import_feed_import:prepare(Entry, Feed, #context{db = {test, test}}),
        ?assertNot(maps:is_key(<<"rights">>, maps:get(props, Disabled))),
        ?assertNot(maps:is_key(<<"rights">>, maps:get(props, DisabledMedia)))
    after meck:unload([z_module_manager, m_rsc, m_category]) end.

dependent_media_import_check() ->
    Modules = [z_module_manager, m_rsc, m_media, m_category, m_edge, z_db, z_notifier, z_acl,
        z_media_tag, z_import_feed_http],
    lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
    try
        Context = #context{db = {test, test}},
        Url = <<"https://example.com/photo.jpg">>,
        meck:expect(m_media, mime_to_category, fun(<<"image/jpeg">>) -> image end),
        meck:expect(m_category, is_a, fun(image, media, _) -> true end),
        meck:expect(z_module_manager, active, fun(_, _) -> false end),
        meck:expect(z_db, transaction, fun(Fun, C) -> Fun(C) end),
        meck:expect(z_db, q, fun(_, _) -> [] end),
        meck:expect(z_db, q, fun(_, _, _) -> [] end),
        meck:expect(z_db, q1, fun(_, _, _) -> undefined end),
        meck:expect(z_notifier, first, fun(_, _) -> undefined end),
        meck:expect(z_acl, rsc_editable, fun(202, _) -> true end),
        meck:expect(z_acl, rsc_visible, fun(201, _) -> true end),
        meck:expect(m_rsc, rid, fun(depiction, _) -> 102; (_, _) -> undefined end),
        meck:expect(m_rsc, is_a, fun(201, media, _) -> true end),
        meck:expect(z_media_tag, url, fun(201, [], _) -> {ok, <<"/image/photo.jpg">>} end),
        meck:expect(m_rsc, p, fun(201, feed_medium_url, _) -> Url end),
        meck:expect(z_import_feed_http, get, fun(U, _, C) ->
            ?assertEqual(Url, U),
            ?assertEqual(Context, C),
            {ok, {Url, [], 4, <<"data">>}}
        end),
        meck:expect(m_media, insert_file, fun(Upload, Props, Options, C) ->
            ?assertMatch(#upload{filename = <<"photo.jpg">>, data = <<"data">>}, Upload),
            ?assertNot(maps:is_key(<<"category_id">>, Props)),
            ?assertNot(maps:is_key(<<"category">>, Props)),
            ?assertEqual(Url, maps:get(<<"uri">>, Props)),
            ?assertEqual(Url, maps:get(<<"feed_medium_url">>, Props)),
            ?assertEqual(true, maps:get(<<"is_dependent">>, Props)),
            ?assertEqual([is_import, {tz, <<"UTC">>}], Options),
            ?assertEqual(Context, C),
            {ok, 201}
        end),
        meck:expect(m_rsc, insert, fun(Props, _, _) ->
            ?assertEqual(100, maps:get(<<"category_id">>, Props)),
            {ok, 202}
        end),
        meck:expect(m_edge, insert, fun(202, 102, 201, _) -> {ok, 1} end),
        Entry = #{uri => <<"https://example.com/article">>, url => <<"https://example.com/article">>,
            title => <<"Article">>, body => <<>>, summary => <<>>, author => <<>>,
            published => undefined, modified => undefined, media => [Url], raw => #{}},
        Feed = #{id => 1, category_id => 100, content_group_id => undefined,
            is_authoritative => false, media_mode => <<"dependent">>, connections => []},
        ?assertEqual(imported, z_import_feed_import:run(Entry, Feed, Context)),
        ?assertEqual(1, meck:num_calls(m_media, insert_file, '_')),
        ?assertEqual(1, meck:num_calls(z_import_feed_http, get, '_')),
        ?assertEqual(1, meck:num_calls(m_rsc, insert, '_')),
        %% A failed medium must not roll back or leave broken references in an article.
        FailedEntry = Entry#{body => <<"<p>Keep this text</p><a href='https://example.com/photo.jpg'><img src='https://example.com/small.jpg' srcset='https://example.com/small.jpg 1x, https://example.com/photo.jpg 2x'></a>">>,
            media_aliases => #{<<"https://example.com/small.jpg">> => Url}},
        CheckBody = fun(Props) ->
            Body = maps:get(<<"body">>, Props),
            ?assertNotEqual(nomatch, binary:match(Body, <<"Keep this text">>)),
            ?assertEqual(nomatch, binary:match(Body, <<"photo.jpg">>)),
            ?assertEqual(nomatch, binary:match(Body, <<"small.jpg">>)),
            ?assertEqual(nomatch, binary:match(Body, <<"z-media">>))
        end,
        meck:expect(m_rsc, p, fun(_, feed_medium_url, _) -> undefined end),
        meck:expect(m_rsc, update, fun(202, Props, _) -> CheckBody(Props), {ok, 202} end),
        meck:expect(m_media, insert_file, fun(_, _, _, _) -> {error, eacces} end),
        meck:expect(m_media, replace_file, fun(_, 202, _, _) -> {error, eacces} end),
        lists:foreach(fun(Mode) ->
            meck:expect(m_rsc, insert, fun(Props, _, _) ->
                case Mode of <<"dependent">> -> CheckBody(Props); _ -> ok end,
                {ok, 202}
            end),
            ?assertEqual(imported, z_import_feed_import:run(FailedEntry, Feed#{media_mode => Mode}, Context)),
            meck:expect(z_import_feed_http, get, fun(_, _, _) -> {error, timeout} end),
            ?assertEqual(imported, z_import_feed_import:run(FailedEntry, Feed#{media_mode => Mode}, Context)),
            meck:expect(z_import_feed_http, get, fun(_, _, _) -> {ok, {Url, [], 4, <<"data">>}} end)
        end, [<<"dependent">>, <<"attached">>]),
        ?assertEqual(1, meck:num_calls(m_edge, insert, '_')),
        ?assertEqual(4, meck:num_calls(z_db, q, ["rollback to savepoint import_feed_medium", '_']))
    after meck:unload(Modules) end.

category_guess_excludes_meta_check() ->
    Modules = [m_category, m_rsc, z_module_manager, z_context],
    lists:foreach(fun(M) -> meck:new(M, [no_link]) end, Modules),
    try
        meck:expect(z_context, languages, fun(_) -> [en] end),
        %% Meta and a descendant precede the content category in tree order.
        meck:expect(m_category, tree_flat, fun(_) ->
            [[{id, 10}], [{id, 11}], [{id, 12}]] end),
        meck:expect(m_category, is_a, fun(Id, meta, _) -> lists:member(Id, [10, 11]) end),
        meck:expect(m_rsc, p, fun
            (10, _, _) -> <<"Meta">>;
            (11, _, _) -> <<"Predicate">>;
            (12, name, _) -> <<"news">>;
            (12, title, _) -> <<"News">>
        end),
        meck:expect(m_rsc, rid, fun(article, _) -> 100 end),
        meck:expect(z_module_manager, active, fun(_, _) -> false end),
        {ok, Parsed} = parse(<<"<rss><channel><item><title>Article</title><link>https://example.com/a</link></item></channel></rss>">>),
        [Entry] = maps:get(entries, Parsed),
        Feed = #{guess_category => true, category_id => 100, content_group_id => undefined,
            is_authoritative => false, media_mode => <<"none">>, connections => []},
        Guess = fun(Tags, Default) ->
            {#{props := Props}, []} = z_import_feed_import:prepare(
                Entry#{tags => Tags}, Feed#{category_id => Default}, #context{db = {test, test}}),
            maps:get(<<"category_id">>, Props)
        end,
        ?assertEqual(12, Guess([<<"Meta">>, <<"Predicate">>, <<"NEWS">>], 100)),
        ?assertEqual(100, Guess([<<"Meta">>, <<"Predicate">>], 100)),
        ?assertEqual(100, Guess([<<"Unknown">>], 11)),
        ?assertEqual(100, Guess([], 10))
    after lists:foreach(fun meck:unload/1, Modules) end.

lazy_images_and_embeds_check() ->
    Html = <<"Leading &amp; text<figure><img class='lazyload' src='data:image/svg+xml;base64,AAAA' data-src='/small.jpg' data-srcset='/small.jpg 300w, /large.jpg 1200w'><figcaption>Caption</figcaption></figure>Trailing text">>,
    Base = <<"https://example.org/article">>,
    Large = <<"https://example.org/large.jpg">>,
    {Images, Aliases} = z_import_feed_parse:image_variants(Html, Base),
    ?assertEqual([Large], Images),
    ?assertEqual(Large, maps:get(<<"https://example.org/small.jpg">>, Aliases)),
    meck:new(z_media_tag, [no_link]),
    try
        meck:expect(z_media_tag, url, fun(123, [], _) -> {ok, <<"/image/local.jpg">>} end),
        Props = z_import_feed_import:local_body(#{<<"body">> => Html, <<"website">> => Base}, #{Large => 123}, #context{}),
        Body = maps:get(<<"body">>, Props),
        ?assertNotEqual(nomatch, binary:match(Body, <<"<!-- z-media 123 {\"size\":\"large\",\"align\":\"block\"} -->">>)),
        ?assertNotEqual(nomatch, binary:match(Body, <<"<figcaption>Caption</figcaption>">>)),
        ?assertNotEqual(nomatch, binary:match(Body, <<"Leading &amp; text">>)),
        ?assertEqual(nomatch, binary:match(Body, <<"<img">>)),
        ?assertEqual(nomatch, binary:match(Body, <<"data-src">>)),
        Wrapped = z_import_feed_import:local_body(#{<<"body">> =>
            <<"<a href='/image/local.jpg'><img src='data:image/svg+xml;base64,AAAA'></a>">>},
            #{<<"/image/local.jpg">> => 123}, #context{}),
        ?assertEqual(<<"<!-- z-media 123 {\"size\":\"large\",\"align\":\"block\"} -->">>,
            maps:get(<<"body">>, Wrapped))
    after meck:unload(z_media_tag) end.

summary_entities_check() ->
    meck:new(z_module_manager, [no_link]),
    try
        meck:expect(z_module_manager, active, fun(_, _) -> false end),
        {ok, Parsed} = parse(<<"<rss><channel><item><title>Article</title><link>https://example.com/a</link><description><![CDATA[<p>News &amp; views [&#8230;] &#x2014; &lt;example&gt;</p>]]></description></item></channel></rss>">>),
        [Entry] = maps:get(entries, Parsed),
        Feed = #{category_id => 100, content_group_id => undefined,
            is_authoritative => false, media_mode => <<"none">>, connections => []},
        {#{props := Props}, []} = z_import_feed_import:prepare(Entry, Feed, #context{db = {test, test}}),
        ?assertEqual(<<"News & views […] — <example>"/utf8>>,
            z_string:trim(maps:get(<<"summary">>, Props)))
    after meck:unload(z_module_manager) end.

article_links_check() ->
    Modules = [z_db, z_acl, m_rsc],
    lists:foreach(fun(M) -> meck:new(M, [no_link]) end, Modules),
    try
        meck:expect(z_db, q1, fun(_, [U], _) ->
            case U of
                <<"https://example.org/imported">> -> 123;
                <<"https://example.org/private">> -> 124;
                _ -> undefined
            end
        end),
        meck:expect(z_db, q, fun(_, _) -> [{123}] end),
        meck:expect(z_acl, rsc_visible, fun(Id, _) -> Id =:= 123 end),
        meck:expect(m_rsc, p, fun
            (123, page_url, _) -> <<"/page/123">>;
            (123, website, _) -> <<"https://example.org/canonical">>
        end),
        Body = z_import_feed_import:normalize_links(
            <<"<a href='/photo.JPG?size=large'><img src='/photo.JPG'></a><a href='/imported#section'>Local</a><a href='/canonical'>Canonical</a><a href='other'>Original</a><a href='/private'>Private</a><a href='mailto:editor@example.org'>Email</a>">>,
            <<"https://example.org/article">>, #context{}),
        ?assertEqual(nomatch, binary:match(Body, <<"href=\"https://example.org/photo">>)),
        ?assertNotEqual(nomatch, binary:match(Body, <<"<img src=\"/photo.JPG\"">>)),
        ?assertNotEqual(nomatch, binary:match(Body, <<"href=\"/page/123#section\"">>)),
        ?assertNotEqual(nomatch, binary:match(Body, <<"href=\"/page/123\">Canonical">>)),
        ?assertNotEqual(nomatch, binary:match(Body, <<"href=\"https://example.org/other\"">>)),
        ?assertNotEqual(nomatch, binary:match(Body, <<"href=\"https://example.org/private\"">>)),
        ?assertNotEqual(nomatch, binary:match(Body, <<"href=\"mailto:editor@example.org\"">>))
    after lists:foreach(fun meck:unload/1, Modules) end.

removal_patterns_check() ->
    {ok, Patterns} = z_import_feed_cleanup:compile(<<"\nHet bericht .*? verscheen eerst op De Westkrant\\.\r\n">>),
    Footer = <<"Het bericht West tegen de ratten: “Als ze binnenkomen is het oorlog” verscheen eerst op De Westkrant."/utf8>>,
    ?assertEqual(<<"Intro […] "/utf8>>, z_import_feed_cleanup:text(<<"Intro […] "/utf8, Footer/binary>>, Patterns)),
    Html = <<"<p>Keep &amp; enjoy.</p><p>Het bericht <a href='/article'>West tegen de ratten: “Als ze binnenkomen is het oorlog”</a> verscheen eerst op <a href='/'>De Westkrant</a>.</p><p><img src='/photo.jpg'></p>"/utf8>>,
    Clean = z_import_feed_cleanup:html(Html, Patterns),
    ?assertEqual(nomatch, binary:match(Clean, <<"Het bericht">>)),
    ?assertNotEqual(nomatch, binary:match(Clean, <<"<p>Keep &amp; enjoy.</p>">>)),
    ?assertNotEqual(nomatch, binary:match(Clean, <<"<img">>)),
    ?assertEqual(Html, z_import_feed_cleanup:html(Html, [])),
    ?assertMatch({error,{invalid_regexp,2,_}}, z_import_feed_cleanup:compile(<<"valid\n[">>)),
    {ok, Multiple} = z_import_feed_cleanup:compile(<<"one\ntwo">>),
    ?assertEqual(<<"  three">>, z_import_feed_cleanup:text(<<"one two three">>, Multiple)).

delete_feed_content_permissions_check() ->
    Modules = [z_db, z_acl, m_rsc, m_edge],
    lists:foreach(fun(M) -> meck:new(M, [no_link]) end, Modules),
    try
        meck:expect(z_acl, is_allowed, fun(use, mod_import_feed, C) -> maps:get(allowed, C#context.props, true) end),
        meck:expect(z_acl, is_read_only, fun(C) -> maps:get(readonly, C#context.props, false) end),
        meck:expect(z_db, transaction, fun(F, C) -> F(C) end),
        meck:expect(z_db, q_row, fun(_, [7], _) -> {undefined} end),
        meck:expect(z_db, update, fun(import_feed, 7, #{is_paused := true}, _) -> {ok, 1} end),
        meck:expect(z_db, q, fun(_, _) -> [] end),
        meck:expect(z_db, q, fun(Sql, [7], _) ->
            case lists:prefix("select distinct", Sql) of
                true -> [{10}, {11}, {12}, {13}, {14}];
                false -> []
            end
        end),
        meck:expect(m_rsc, p_no_acl, fun
            (11, is_protected, _) -> true;
            (15, is_dependent, _) -> true;
            (_, _, _) -> false
        end),
        meck:expect(m_rsc, is_deletable, fun(Id, _) -> not lists:member(Id, [12, 15]) end),
        meck:expect(m_edge, objects, fun(13, _) -> [15]; (_, _) -> [] end),
        meck:expect(m_rsc, delete, fun(10, _) -> ok; (14, _) -> {error, failed} end),
        ?assertEqual({ok, #{deleted => 1, skipped => 3, failed => 1}},
            m_import_feed:delete_content(7, #context{})),
        ?assertEqual(0, meck:num_calls(m_rsc, delete, [11, '_'])),
        ?assertEqual(0, meck:num_calls(m_rsc, delete, [12, '_'])),
        ?assertEqual(0, meck:num_calls(m_rsc, delete, [13, '_'])),
        ?assertEqual({error, eacces}, m_import_feed:delete_content(7, #context{props = #{readonly => true}})),
        ?assertEqual({error, eacces}, m_import_feed:delete_content(7, #context{props = #{allowed => false}})),
        meck:expect(z_db, delete, fun(import_feed, 7, _) -> {ok, 1} end),
        DeletesBefore = meck:num_calls(m_rsc, delete, '_'),
        ?assertEqual({ok, #{deleted => 0, skipped => 0, failed => 0}},
            m_import_feed:delete_feed(7, false, #context{})),
        ?assertEqual(DeletesBefore, meck:num_calls(m_rsc, delete, '_')),
        ?assertEqual({ok, #{deleted => 1, skipped => 3, failed => 1}},
            m_import_feed:delete_feed(7, true, #context{})),
        ?assertEqual(2, meck:num_calls(z_db, delete, [import_feed, 7, '_'])),
        ?assertEqual({error, eacces}, m_import_feed:delete_feed(7, true, #context{props = #{readonly => true}})),
        ?assertEqual({error, eacces}, m_import_feed:delete_feed(7, false, #context{props = #{allowed => false}})),
        meck:expect(z_db, q_row, fun(_, [7], _) -> {<<"running">>} end),
        ?assertEqual({error, feed_is_running}, m_import_feed:delete_content(7, #context{})),
        ?assertEqual({error, feed_is_running}, m_import_feed:delete_feed(7, false, #context{})),
        ?assertEqual({error, feed_is_running}, m_import_feed:delete_feed(7, true, #context{})),
        meck:expect(z_db, q_row, fun(_, [7], _) -> undefined end),
        ?assertEqual({error, enoent}, m_import_feed:delete_content(7, #context{}))
    after lists:foreach(fun meck:unload/1, Modules) end.

fetch_resource_permissions_check() ->
    Modules = [z_db, z_acl, m_rsc, z_import_feed_fetch],
    lists:foreach(fun(M) -> meck:new(M, [no_link]) end, Modules),
    try
        meck:expect(z_db, q_row, fun
            (_, [<<"unknown">>], _) -> undefined;
            (_, [<<"deleted">>], _) -> {1, undefined};
            (_, _, _) -> {1, 42}
        end),
        meck:expect(z_acl, is_allowed, fun(use, mod_import_feed, C) -> C#context.site =/= denied end),
        meck:expect(z_acl, is_read_only, fun(C) -> C#context.site =:= readonly end),
        meck:expect(z_acl, rsc_editable, fun(42, C) -> C#context.site =/= uneditable end),
        meck:expect(m_rsc, p_no_acl, fun(42, feed_is_zotonic, _) -> false end),
        meck:expect(z_import_feed_fetch, run, fun(1, 42, <<"known">>, #context{site = allowed}) -> {ok, 42} end),
        ?assertEqual(undefined, m_import_feed:fetch_resource(<<"unknown">>, #context{site = denied})),
        ?assertEqual({error, enoent}, m_import_feed:fetch_resource(<<"deleted">>, #context{site = allowed})),
        lists:foreach(fun(C) ->
            ?assertEqual({error, eacces}, m_import_feed:fetch_resource(<<"known">>, C))
        end, [#context{site = S} || S <- [denied, readonly, uneditable]]),
        ?assertEqual({ok, 42}, m_import_feed:fetch_resource(<<"known">>, #context{site = allowed})),
        ?assertEqual(1, meck:num_calls(z_import_feed_fetch, run, '_'))
    after lists:foreach(fun meck:unload/1, Modules) end.

fetch_resource_pipeline_check() ->
    Modules = [z_db, z_acl, z_auth, z_memo, m_import_feed,
        z_import_feed_http, z_import_feed_wordpress, z_import_feed_import],
    lists:foreach(fun(M) -> meck:new(M, [no_link, passthrough]) end, Modules),
    try
        Uri = <<"https://example.com/post">>,
        F = #{id => 1, url => <<"https://example.com/feed">>, user_id => 2,
            user_groups => [3], use_wordpress => true, is_paused => true},
        meck:expect(z_db, transaction, fun(Fun, C) -> Fun(C) end),
        meck:expect(z_db, qmap, fun(_, [1], _, _) -> {ok, [F]} end),
        meck:expect(z_db, q1, fun(_, [U], _) when U =:= Uri -> 42 end),
        meck:expect(m_import_feed, decode, fun(Row) -> Row end),
        meck:expect(m_import_feed, record_feed_type, fun(_, _, _) -> ok end),
        meck:expect(m_import_feed, is_admin_user, fun(2, _) -> false end),
        meck:expect(m_import_feed, allowed, fun(#context{site = editor}) -> true end),
        meck:expect(z_auth, is_enabled, fun(2, _) -> true end),
        meck:expect(z_acl, logon, fun(2, #{user_groups := [3]}, #context{site = caller}) -> #context{site = editor} end),
        meck:expect(z_acl, is_read_only, fun(#context{site = editor}) -> false end),
        meck:expect(z_acl, rsc_editable, fun(42, #context{site = editor}) -> true end),
        meck:expect(z_memo, set_userid, fun(#context{site = caller}) -> ok end),
        meck:expect(z_import_feed_http, context, fun(_, #context{site = editor}) -> #context{site = editor} end),
        meck:expect(z_import_feed_http, get, fun(_, #context{site = editor}) ->
            {ok, {"https://example.com/feed", [], 0,
                <<"<rss><channel><item><link>https://example.com/post</link><title>Fresh</title></item></channel></rss>">>}}
        end),
        meck:expect(z_import_feed_wordpress, enrich, fun(E, #context{site = editor}) -> E#{title := <<"Enriched">>} end),
        meck:expect(z_import_feed_import, run, fun
            (#{uri := U, title := <<"Enriched">>}, #{is_paused := true, feed := _}, true, #context{site = editor})
                when U =:= Uri -> updated
        end),
        ?assertEqual({ok, 42}, z_import_feed_fetch:run(1, 42, Uri, #context{site = caller})),
        ?assertEqual({error, feed_entry_not_found},
            z_import_feed_fetch:run(1, 42, <<"https://example.com/missing">>, #context{site = caller})),
        meck:expect(z_acl, rsc_editable, fun(42, #context{site = editor}) -> false end),
        ?assertEqual({error, eacces}, z_import_feed_fetch:run(1, 42, Uri, #context{site = caller})),
        meck:expect(z_auth, is_enabled, fun(2, _) -> false end),
        ?assertEqual({error, editor_disabled}, z_import_feed_fetch:run(1, 42, Uri, #context{site = caller})),
        meck:expect(m_import_feed, is_admin_user, fun(2, _) -> true end),
        ?assertEqual({error, admin_import_user}, z_import_feed_fetch:run(1, 42, Uri, #context{site = caller})),
        ?assertEqual(2, meck:num_calls(z_import_feed_http, get, '_')),
        ?assertEqual(1, meck:num_calls(z_import_feed_import, run, '_')),
        ?assertEqual(5, meck:num_calls(z_memo, set_userid, '_'))
    after lists:foreach(fun meck:unload/1, Modules) end.

zotonic_atom_detection_check() ->
    {ok, Feed} = parse(<<"<feed xmlns='http://www.w3.org/2005/Atom'><entry><id>https://example.com/id/42</id><title>Native</title><link rel='alternate' type='text/html' href='/article'/><link rel='https://zotonic.com/rel/resource' type='application/json' href='/api/model/rsc_export/get/42'/></entry><entry><id>https://example.com/id/43</id><link rel='alternate' type='application/json' href='/wordpress'/></entry></feed>">>),
    [Native, Ordinary] = maps:get(entries, Feed),
    ?assertEqual(<<"https://example.com/api/model/rsc_export/get/42">>, maps:get(zotonic_export, Native)),
    ?assertEqual(undefined, maps:get(zotonic_export, Ordinary)),
    ?assertEqual(Native, z_import_feed_wordpress:enrich(Native, #context{})).

zotonic_import_options_check() ->
    Defaults = z_import_feed_native:options(#{media_mode => <<"dependent">>}, #{}),
    ?assertEqual(1, proplists:get_value(import_edges, Defaults)),
    ?assertEqual([{authorization, none}], proplists:get_value(fetch_options, Defaults)),
    ?assertEqual(false, proplists:get_value(is_subscribe, Defaults)),
    ?assertEqual(false, proplists:get_value(is_subscribe_connections, Defaults)),
    Options = z_import_feed_native:options(#{media_mode => <<"none">>, import_depth => 0,
        is_subscribe => true, is_subscribe_connections => true, use_credentials => true}, #{<<"content_group_id">> => 42}),
    ?assertEqual(0, proplists:get_value(import_edges, Options)),
    ?assertEqual([], proplists:get_value(fetch_options, Options)),
    ?assertEqual(true, proplists:get_value(is_subscribe, Options)),
    ?assertEqual(true, proplists:get_value(is_subscribe_connections, Options)),
    ?assertEqual(true, proplists:get_value(is_no_medium_download, Options)),
    ?assertEqual(42, maps:get(<<"content_group_id">>, proplists:get_value(props_forced, Options))).

zotonic_native_import_check() ->
    Modules = [z_import_feed_http, m_rsc_import, z_context, z_db, m_rsc],
    lists:foreach(fun(M) -> meck:new(M, [no_link]) end, Modules),
    try
        Uri = <<"https://example.com/id/42">>,
        JSON = #{<<"uri">> => Uri, <<"resource">> => #{<<"title">> => <<"Native">>},
            <<"edges">> => #{<<"about">> => #{<<"objects">> => [#{<<"object_id">> => #{<<"uri">> => <<"https://example.com/id/43">>}}]}}},
        meck:expect(z_import_feed_http, get_json, fun(_, _) ->
            {ok, {"https://example.com/export", [], 0, z_json:encode(#{<<"status">> => <<"ok">>, <<"result">> => JSON})}}
        end),
        meck:expect(z_db, q1, fun(_, _, _) -> 0 end),
        meck:expect(z_context, set, fun(import_feed_native, Cached, #context{}) -> #context{props = #{native => Cached}} end),
        meck:expect(m_rsc, is_a, fun(Id, meta, _) -> Id =:= 99 end),
        meck:expect(m_rsc_import, import_uri_recursive, fun(U, Opts, #context{props = #{native := Cache}}) ->
            ?assertEqual(Uri, U),
            ?assertEqual(1, proplists:get_value(import_edges, Opts)),
            ?assertEqual({ok, #{<<"status">> => <<"ok">>, <<"result">> => JSON}},
                z_import_feed_native:fetch(U, Cache, #context{})),
            {ok, {42, #{U => 42, <<"https://example.com/id/43">> => 43, <<"meta">> => 99}}}
        end),
        Feed = #{media_mode => <<"dependent">>, connections => []},
        {Item, Deps} = z_import_feed_native:prepare(#{uri => Uri, zotonic_export => <<"https://example.com/export">>},
            #{uri => Uri, props => #{}, edges => [], medium => undefined}, [], Feed, #context{}),
        ?assertMatch([#{uri := <<"https://example.com/id/43">>}], Deps),
        ?assertEqual({42, #{<<"https://example.com/id/43">> => 43}}, z_import_feed_native:run(Item, undefined, #context{})),
        {ZeroItem, []} = z_import_feed_native:prepare(
            #{uri => Uri, zotonic_export => <<"https://example.com/export">>},
            #{uri => Uri, props => #{}, edges => [], medium => undefined}, [],
            Feed#{import_depth => 0}, #context{}),
        ?assertEqual([], maps:get(edges, ZeroItem)),
        ?assertThrow({import_error, invalid_zotonic_export},
            z_import_feed_native:prepare(#{uri => <<"wrong-uri">>, zotonic_export => <<"https://example.com/export">>},
                #{}, [], Feed, #context{})),
        meck:expect(z_db, q1, fun(_, _, _) -> 1 end),
        ?assertEqual({error, deleted}, z_import_feed_native:fetch(Uri, #{Uri => JSON}, #context{}))
    after lists:foreach(fun meck:unload/1, Modules) end.

zotonic_native_cleanup_check() ->
    Body = <<"<p>Keep</p><!-- z-media 42 {} --><p>Remove me</p>">>,
    {true, Clean} = z_import_feed_native:filter_resource(<<"Remove me">>,
        #{<<"body">> => #trans{tr = [{en, Body}]}, <<"summary">> => <<"Keep Remove me">>}, [], #context{}),
    ?assertEqual(<<"Keep ">>, maps:get(<<"summary">>, Clean)),
    #trans{tr = [{en, CleanBody}]} = maps:get(<<"body">>, Clean),
    ?assertNotEqual(nomatch, binary:match(CleanBody, <<"<!-- z-media 42 {} -->">>)),
    ?assertEqual(nomatch, binary:match(CleanBody, <<"Remove me">>)).

fetch_credentials_and_development_check() ->
    Modules = [z_fetch, z_acl],
    lists:foreach(fun(M) -> meck:new(M, [no_link]) end, Modules),
    try
        meck:expect(z_acl, logon, fun(User, C) -> C#context{user_id = User} end),
        meck:expect(z_acl, anondo, fun(C) -> C#context{user_id = undefined} end),
        Auth = #context{props = #{import_feed_use_credentials => true, import_feed_fetch_user => 42}},
        meck:expect(z_fetch, fetch, fun(Url, Options, C) ->
            ?assertEqual(42, C#context.user_id),
            ?assertEqual(undefined, proplists:get_value(authorization, Options)),
            ?assertEqual(undefined, proplists:get_value(insecure, Options)),
            ?assertEqual(none, proplists:get_value(authorization_redirect, Options)),
            {ok, {Url, [], 2, <<"ok">>}}
        end),
        ?assertMatch({ok, _}, z_import_feed_http:get(<<"https://example.com/feed">>, Auth)),
        meck:expect(z_fetch, fetch, fun(Url, Options, C) ->
            ?assertEqual(undefined, C#context.user_id),
            ?assertEqual(none, proplists:get_value(authorization, Options)),
            ?assertEqual(undefined, proplists:get_value(insecure, Options)),
            ?assertEqual(none, proplists:get_value(authorization_redirect, Options)),
            {ok, {Url, [], 2, <<"ok">>}}
        end),
        ?assertMatch({ok, _}, z_import_feed_http:get(<<"https://127.0.0.1/feed">>, #context{})),
        ?assertEqual({error, unsafe_destination}, z_import_feed_http:get(<<"https://user:pass@localhost/feed">>, #context{}))
    after meck:unload(Modules) end.

configured_fetch_user_check() ->
    Modules = [z_fetch, z_acl],
    lists:foreach(fun(M) -> meck:new(M, [no_link]) end, Modules),
    try
        meck:expect(z_acl, user, fun(C) -> C#context.user_id end),
        meck:expect(z_acl, logon, fun(User, C) -> C#context{user_id = User} end),
        meck:expect(z_acl, anondo, fun(C) -> C#context{user_id = undefined} end),
        meck:expect(z_acl, is_allowed, fun(use, mod_import_feed, C) ->
            ?assertEqual(1, C#context.user_id),
            true
        end),
        meck:expect(z_fetch, fetch, fun(Url, Options, C) ->
            ?assertEqual(42, C#context.user_id),
            ?assertEqual(undefined, proplists:get_value(authorization, Options)),
            Xml = <<"<rss><channel><title>Feed</title></channel></rss>">>,
            {ok, {Url, [], byte_size(Xml), Xml}}
        end),
        Caller = #context{user_id = 1},
        Feed = #{id => 7, user_id => 42, use_credentials => true, url => <<"https://example.com/feed">>},
        %% Preview ACLs use the caller; fetching uses the saved account.
        ?assertMatch({ok, _}, m_import_feed:preview(Feed, 3, Caller)),
        meck:expect(z_fetch, fetch, fun(Url, _, C) ->
            ?assertEqual(undefined, C#context.user_id),
            {ok, {Url, [], 0, <<>>}}
        end),
        lists:foreach(fun(Settings) ->
            C = z_import_feed_http:context(Settings, Caller),
            ?assertMatch({ok, _}, z_import_feed_http:get(maps:get(url, Feed), C))
        end, [maps:remove(user_id, Feed), Feed#{user_id => undefined}]),
        Discovery = z_import_feed_http:context(#{use_credentials => true}, Caller),
        ?assertEqual(1, z_context:get(import_feed_fetch_user, Discovery))
    after meck:unload(Modules) end.

feed_type_detection_check() ->
    {ok, Rss} = parse(<<"<rss><channel><title>Empty RSS</title></channel></rss>">>),
    ?assertEqual(false, maps:get(is_zotonic_feed, Rss)),
    {ok, EmptyAtom} = parse(<<"<feed xmlns='http://www.w3.org/2005/Atom'><title>Empty</title></feed>">>),
    ?assertEqual(undefined, maps:get(is_zotonic_feed, EmptyAtom)),
    {ok, Ordinary} = parse(<<"<feed xmlns='http://www.w3.org/2005/Atom'><entry><id>https://example.org/1</id></entry></feed>">>),
    ?assertEqual(false, maps:get(is_zotonic_feed, Ordinary)),
    {ok, Mixed} = parse(<<"<feed xmlns='http://www.w3.org/2005/Atom'><entry><id>https://example.org/1</id></entry><entry><id>https://example.org/2</id><link rel='https://zotonic.com/rel/resource' type='application/json' href='/api/model/rsc_export/get/2'/></entry></feed>">>),
    ?assertEqual(true, maps:get(is_zotonic_feed, Mixed)).

%% Mock-specific state stays inside a valid Zotonic context, so API contracts
%% remain checked by Dialyzer even when dependencies are replaced by meck.
-spec with_props(map(), z:context()) -> z:context().
with_props(Props, #context{props = Existing} = Context) ->
    Context#context{props = maps:merge(Existing, Props)}.
