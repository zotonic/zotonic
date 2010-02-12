-module(z_xmpp_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").


parse_xmpp_uri_test() ->
    zotonic:ensure_started(exmpp),

    Jid = exmpp_jid:parse("foo"),
    ?assertEqual({Jid, [], [{"bar", true}]}, z_xmpp:parse_xmpp_uri("xmpp:foo?;bar")),
    ?assertEqual({Jid, [], []}, z_xmpp:parse_xmpp_uri("xmpp:foo")),

    Romeo = exmpp_jid:parse("romeo@montague.net"),
    ?assertEqual({Romeo, "roster", []}, z_xmpp:parse_xmpp_uri("xmpp:romeo@montague.net?roster")),
    ?assertEqual({Romeo, "message", [{"subject", "Test Message"}, {"body", "Here's a test message"}]},
                 z_xmpp:parse_xmpp_uri("xmpp:romeo@montague.net?message;subject=Test%20Message;body=Here%27s%20a%20test%20message")),

    MM = exmpp_jid:parse("pubsub.mediamatic.net"),
    ?assertEqual({MM, [], [{"node", "id/22661"}]}, z_xmpp:parse_xmpp_uri("xmpp:pubsub.mediamatic.net?;node=id/22661")),

    ok.


xmpp_uri_from_html_test() ->
    Html = "<html><head><link rel=\"stylesheet\" href=\"foo.js\" /><link rel=\"xmpp.feed\" href=\"xmpp:pubsub.mediamatic.net?;node=id/22661\"></head></html>",

    ?assertEqual("xmpp:pubsub.mediamatic.net?;node=id/22661", z_xmpp:xmpp_uri_from_html(Html)),

    Html2 = "<link rel=\"thumbnail\" href=\"http://fast.mediamatic.nl/f/sjnh/image/388/27597-55-55-crop,scale.jpg\" />
<meta name=\"types\" content=\"person,__user__\" />


	<link rel='openid2.provider' href='http://www.mediamatic.net/openid/provider' />
	<link rel='openid.server'    href='http://www.mediamatic.net/openid/provider' />

	<link rel='openid2.local_id' href='http://www.mediamatic.net/id/874' />
	<link rel='openid.delegate'  href='http://www.mediamatic.net/id/874' />

<link rel=\"alternate\" type=\"application/atom+xml;type=entry\" href=\"http://www.mediamatic.net/atom/874\"/>
<link rel=\"self\" href=\"http://www.mediamatic.net/id/874\"/>

<link rel=\"xmpp.feed\" href=\"xmpp:pubsub.mediamatic.net?;node=id/874\" title=\"XMPP updates for this item\" />



<link rel=\"meta\" type=\"application/rdf+xml\" title=\"FOAF\" href=\"http://www.mediamatic.net/foaf/874\" />
<link rel=\"alternate\" type=\"application/x-anymeta-thing+xml\" title=\"anyMeta thing XML export\" href=\"/thing/874/xml\" />",

    ?assertEqual("xmpp:pubsub.mediamatic.net?;node=id/874", z_xmpp:xmpp_uri_from_html(Html2)),
    ?assertEqual("http://www.mediamatic.net/id/874", z_xmpp:resource_uri_from_html(Html2)),

    Html3 = "<html><head><link rel=\"stylesheet\" href=\"foo.js\" /><link rel=\"other\" href=\"xmpp:pubsub.mediamatic.net?;node=id/22661\"></head></html>",

    ?assertEqual(undefined, z_xmpp:xmpp_uri_from_html(Html3)),


    ok.
