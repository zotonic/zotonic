-module(import_feed_json_tests).

-include_lib("eunit/include/eunit.hrl").

parse(Feed) -> z_import_feed_parse:parse(z_json:encode(Feed), <<"https://example.org/feed.json">>).
feed(Items) -> #{<<"version">> => <<"https://jsonfeed.org/version/1.1">>, <<"title">> => <<"News">>, <<"items">> => Items}.
item() -> #{<<"id">> => <<"post-1">>, <<"content_text">> => <<"<script>literal</script> & text">>}.

plain_text_and_metadata_test() ->
    Item = (item())#{<<"title">> => <<"A < B &amp; C">>, <<"summary">> => <<"A <summary>">>,
        <<"url">> => <<"/post/1">>, <<"date_published">> => <<"2026-10-23T12:00:00+02:00">>,
        <<"tags">> => [<<"News">>, 42], <<"attachments">> => [#{<<"url">> => <<"/file.pdf">>}],
        <<"image">> => <<"/photo.jpg">>},
    {ok, Feed} = parse((feed([Item]))#{<<"authors">> => [#{<<"name">> => <<"Writer">>}]}),
    [Entry] = maps:get(entries, Feed),
    ?assertEqual(false, maps:get(is_zotonic_feed, Feed)),
    ?assertEqual(undefined, maps:get(zotonic_export, Entry)),
    ?assertEqual(<<"&lt;script&gt;literal&lt;/script&gt; &amp; text">>, maps:get(body, Entry)),
    ?assertEqual(<<"A < B &amp; C">>, z_html:unescape(maps:get(title, Entry))),
    ?assertEqual(<<"A <summary>">>, z_html:unescape(maps:get(summary, Entry))),
    ?assertEqual(<<"Writer">>, maps:get(author, Entry)),
    ?assertEqual({{2026, 10, 23}, {10, 0, 0}}, maps:get(published, Entry)),
    ?assertEqual([<<"News">>], maps:get(tags, Entry)),
    ?assertEqual([<<"https://example.org/file.pdf">>, <<"https://example.org/photo.jpg">>], maps:get(media, Entry)),
    ?assertMatch(<<"urn:zotonic:feed:", _/binary>>, maps:get(uri, Entry)).

version_one_and_html_test() ->
    Item = (item())#{<<"content_html">> => <<"<p>Hello <img src='/photo.jpg'></p>">>},
    {ok, Feed} = parse((feed([Item]))#{<<"version">> => <<"https://jsonfeed.org/version/1">>,
        <<"author">> => #{<<"name">> => <<"Old author">>}}),
    [Entry] = maps:get(entries, Feed),
    ?assertEqual(maps:get(<<"content_html">>, Item), maps:get(body, Entry)),
    ?assertEqual(<<"Old author">>, maps:get(author, Entry)),
    ?assertEqual([<<"https://example.org/photo.jpg">>], maps:get(media, Entry)).

malformed_and_limits_test() ->
    ?assertEqual({error, invalid_feed}, parse(#{<<"items">> => []})),
    ?assertEqual({error, invalid_feed}, parse((feed([]))#{<<"items">> => null})),
    ?assertEqual({error, invalid_feed}, z_import_feed_parse:parse(<<"{broken">>, <<"https://example.org/">>)),
    {ok, F} = parse(feed([null, #{}, (item())#{<<"id">> => 42}, item()])),
    ?assertEqual(1, length(maps:get(entries, F))),
    {ok, Limited} = parse(feed(lists:duplicate(101, item()))),
    ?assertEqual(100, length(maps:get(entries, Limited))),
    ?assertEqual(true, maps:get(truncated, Limited)),
    Large = binary:copy(<<" ">>, 4 * 1024 * 1024 + 1),
    ?assertEqual({error, response_too_large}, z_import_feed_parse:parse(Large, <<"https://example.org/">>)).

native_extension_and_urls_test() ->
    Item = (item())#{<<"_zotonic">> => #{<<"resource_url">> => <<"/api/model/rsc_export/get/42">>}},
    {ok, F} = parse(feed([Item])),
    ?assertEqual(true, maps:get(is_zotonic_feed, F)),
    [E] = maps:get(entries, F),
    ?assertEqual(<<"https://example.org/api/model/rsc_export/get/42">>, maps:get(zotonic_export, E)),
    Bad = Item#{<<"_zotonic">> => #{<<"resource_url">> => <<"file:///etc/passwd">>},
        <<"url">> => <<"javascript:alert(1)">>, <<"attachments">> => [#{<<"url">> => <<"file:///etc/passwd">>}]},
    {ok, Safe} = parse(feed([Bad])),
    [S] = maps:get(entries, Safe),
    ?assertEqual(undefined, maps:get(zotonic_export, S)),
    ?assertEqual(<<>>, maps:get(url, S)),
    ?assertEqual([], maps:get(media, S)).

discovery_test() ->
    Url = <<"https://example.org/">>,
    Html = <<"<html><head><link rel='alternate' type='application/feed+json' href='/feed.json' title='JSON news'></head></html>">>,
    ?assertEqual({ok, [#{url => <<"https://example.org/feed.json">>, title => <<"JSON news">>}]},
        z_import_feed_parse:discover(Html, Url)),
    ?assertEqual({ok, [#{url => Url, title => <<"News">>}]},
        z_import_feed_parse:discover(z_json:encode(feed([])), Url)).
