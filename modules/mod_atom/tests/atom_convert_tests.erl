-module(atom_convert_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("xmerl/include/xmerl.hrl").
%%-include_lib("zotonic.hrl").


-define(ATOM_NS, 'http://www.w3.org/2005/Atom').
-define(DEBUG, true).


rsc1() ->
    %% We need the calendar:universal_time_to_local_time functions
    %% here because we want the tests to be timezone independent.
    [{uri, <<"http://localhost/id/222">>},
     {rsc,  [{title, <<"Resource 1">>},
             {publication_start, calendar:universal_time_to_local_time({{2010,1,1},{12,11,0}})},
             {modified, calendar:universal_time_to_local_time({{2010,1,28},{12,14,4}})},
             {summary, <<"This is a summary.">>},
             {body, <<"This is the body.">>}
            ]
     },

     {edges, [
              [{object_id, 111},
               {object_uri, <<"http://localhost/id/111">>},
               {object_title, <<"An author">>},
               {predicate_id, 11},
               {predicate_uri, <<"http://localhost/id/11">>},
               {predicate_name, <<"author">>}
              ]
             ]
     }
    ].

-define(assertContent(Elem, Content), ?assertEqual((hd(Elem#xmlElement.content))#xmlText.value, Content)).

resource_to_atom_test() ->
    Xml = atom_convert:resource_to_atom(rsc1()),
    {Elem, _} = xmerl_scan:string(Xml),

    ?assertEqual(Elem#xmlElement.name, entry),
    NS = Elem#xmlElement.namespace,
    ?assertEqual(NS#xmlNamespace.default, ?ATOM_NS),

    %% Check the existence of the id field
    [UriElem] = xmerl_xpath:string("/entry/id", Elem),
    ?assertContent(UriElem, "http://localhost/id/222"),

    %% Check the existence of the title field
    [TitleElem] = xmerl_xpath:string("/entry/title", Elem),
    ?assertContent(TitleElem, "Resource 1"),

    %% Check the existence of the published field
    [PublElem] = xmerl_xpath:string("/entry/published", Elem),
    ?assertContent(PublElem, "2010-01-01T12:11:00Z"),

    %% Check the existence of the updated field
    [UpdElem] = xmerl_xpath:string("/entry/updated", Elem),
    ?assertContent(UpdElem, "2010-01-28T12:14:04Z"),

    %% Check the existence of the summary field
    [SumElem] = xmerl_xpath:string("/entry/summary", Elem),
    ?assertContent(SumElem, "This is a summary."),

    %% Check the existence of the content field
    [BodyElem] = xmerl_xpath:string("/entry/content", Elem),
    ?assertContent(BodyElem, "This is the body."),

    %% Check the existence of the author/name field
    [AuthorName] = xmerl_xpath:string("/entry/author/name", Elem),
    ?assertContent(AuthorName, "An author"),

    %% Check the existence of the author/name field
    [AuthorUri] = xmerl_xpath:string("/entry/author/uri", Elem),
    ?assertContent(AuthorUri, "http://localhost/id/111"),
    ok.


atom_to_resource_test() ->
    Atom1 = "<entry xmlns=\"http://www.w3.org/2005/Atom\" xmlns:anymeta=\"http://mediamatic.nl/ns/anymeta/\" xml:lang=\"en\">
  <id>http://www.mediamatic.net/id/22661</id>
  <updated>2010-01-19T18:29:39+01:00</updated>
  <link rel=\"alternate\" type=\"text/html\" href=\"http://www.mediamatic.net/person/22661/en\"/>
  <link rel=\"alternate\" type=\"text/html\" hreflang='nl' href=\"http://www.mediamatic.net/person/22661/nl\"/>
  <link rel=\"alternate\" type=\"application/rdf+xml\" href=\"http://www.mediamatic.net/foaf/22661\"/>
  <title type=\"html\">Arjan &lt;em&gt;Scherpenisse&lt;/em&gt;</title>
  <subtitle type=\"html\">new media artist, researcher, developer.</subtitle>
  <summary type=\"html\">&#60;p&#62;Besides being an Unstable Media student at the Gerrit Rietveld Academie, I work at Mediamatic Lab as software engineer. In this role I work on the anyMeta system, on fancy new technologies like XMPP and OpenID, and as general techie with the Mediamatic Foundation.&#60;br/&#62; &#60;br/&#62;My main interests are new media and technology (both software and hardware) and its relation to art. Keywords: soldering, A.I., max/msp/pd, ipod touch, wiimote, software development, OpenCV, OSC, Processing, face recognition.&#60;/p&#62;</summary>
  <anymeta:alternate xml:lang='nl'>
    <title type=\"html\">Arjan Scherpenisse</title>
  </anymeta:alternate>
  <link rel=\"enclosure\" type=\"image/jpeg\" title=\"FIGURE\" href=\"http://fast.mediamatic.nl/f/sjnh/image/766/27597-480-480-scale.jpg\" />
  <author><name>Arjan</name></author>
  <author><name>AnotherAuthor</name><uri>http://foo.com/</uri></author>
  </entry>",

    ImportedRsc = atom_convert:atom_to_resource(Atom1),
    ?assertEqual(<<"http://www.mediamatic.net/id/22661">>, proplists:get_value(uri, ImportedRsc)),

    Rsc = proplists:get_value(rsc, ImportedRsc),
    ?assertEqual(<<"Arjan  Scherpenisse ">>, proplists:get_value(title, Rsc)), %% note the strange spaces; z_html:strip gives use these..
    ?assertEqual(calendar:universal_time_to_local_time({{2010,1,19},{17,29,39}}), proplists:get_value(modified, Rsc)),

    Medium = proplists:get_value(medium, ImportedRsc),
    ?assertEqual(<<"image/jpeg">>, proplists:get_value(mime, Medium)),
    ?assertEqual(<<"http://fast.mediamatic.nl/f/sjnh/image/766/27597-480-480-scale.jpg">>, proplists:get_value(url, Medium)),

    Edges = proplists:get_value(edges, ImportedRsc),
    ?assertEqual(filter_edges(Edges, <<"author">>),
                 [ [{predicate_name, <<"author">>}, {object_uri, <<>>}, {object_title, <<"Arjan">>}],
                   [{predicate_name, <<"author">>}, {object_uri, <<"http://foo.com/">>}, {object_title, <<"AnotherAuthor">>}]
                  ]),

    ?assertEqual(filter_edges(Edges, <<"depiction">>),
                 [ [{predicate_name, <<"depiction">>}, {object_uri, <<"http://fast.mediamatic.nl/f/sjnh/image/766/27597-480-480-scale.jpg">>}, {object_title, <<"FIGURE">>}]]),

    ok.

filter_edges(Edges, PredicateName) ->
    lists:filter(fun(X) -> proplists:get_value(predicate_name, X) == PredicateName end, Edges).


resource_to_resource_test() ->
    Rsc1 = rsc1(),
    Rsc2 = atom_convert:atom_to_resource(atom_convert:resource_to_atom(rsc1())),

    %% Verify the equality of base elements
    L = [uri],
    [?assertEqual(proplists:get_value(X, Rsc1), proplists:get_value(X, Rsc2)) || X <- L],

    %% Verify the equality of the rsc elements
    L2 = [title, uri, modified, publication_start],
    [?assertEqual(proplists:get_value(X, proplists:get_value(rsc, Rsc1)), proplists:get_value(X, proplists:get_value(rsc, Rsc2))) || X <- L2],
    ok.

