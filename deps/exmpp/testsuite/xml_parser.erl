%% Copyright ProcessOne 2006-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

-module(xml_parser).

-include_lib("eunit/include/eunit.hrl").

-include("exmpp.hrl").

-define(SETUP, fun()  -> exmpp:start(), error_logger:tty(false) end).
-define(CLEANUP, fun(_) -> application:stop(exmpp) end).

-define(SOURCE1, "<stream:stream xmlns:stream='ns_stream' xmlns='ns_default'><iq xml:lang='fr'>Content</iq></stream:stream>").

-define(TREE1_NO_NS, [
  {xmlelement, "stream:stream", [
    {"xmlns:stream", "ns_stream"},
    {"xmlns", "ns_default"}
  ], [
    {xmlelement, "iq", [
      {"xml:lang", "fr"}
    ], [
      {xmlcdata, <<"Content">>}
    ]}
  ]}
]).

-define(TREE1_NO_NS_ATOM, [
  {xmlelement, 'stream:stream', [
    {'xmlns:stream', "ns_stream"},
    {'xmlns', "ns_default"}
  ], [
    {xmlelement, 'iq', [
      {'xml:lang', "fr"}
    ], [
      {xmlcdata, <<"Content">>}
    ]}
  ]}
]).

-define(TREE1_NS, [
  {xmlel, "ns_stream", [{"ns_stream", "stream"}, {"ns_default", none}],
    "stream", [], [
    {xmlel, "ns_default", [], "iq", [
      {xmlattr, ?NS_XML_s, "lang", <<"fr">>}
    ], [
      {xmlcdata, <<"Content">>}
    ]}
  ]}
]).

-define(TREE1_NS_ATOM, [
  {xmlel, 'ns_stream', [{'ns_stream', "stream"}, {'ns_default', none}],
    'stream', [], [
    {xmlel, 'ns_default', [], 'iq', [
      {xmlattr, ?NS_XML, 'lang', <<"fr">>}
    ], [
      {xmlcdata, <<"Content">>}
    ]}
  ]}
]).

-define(TREE1_ROOT_DEPTH, [
  {xmlelement, "stream:stream", [
    {"xmlns:stream", "ns_stream"},
    {"xmlns", "ns_default"}
  ], undefined},
  {xmlelement, "iq", [
    {"xml:lang", "fr"}
  ], [
    {xmlcdata, <<"Content">>}
  ]}
]).

-define(TREE1_NS_ROOT_DEPTH, [
  {xmlel, "ns_stream", [{"ns_stream", "stream"}, {"ns_default", none}],
    "stream", [], undefined},
  {xmlel, "ns_default", [], "iq", [
    {xmlattr, ?NS_XML_s, "lang", <<"fr">>}
  ], [
    {xmlcdata, <<"Content">>}
  ]}
]).

-define(TREE1_NS_END_EL, [
  {xmlel, "ns_stream", [{"ns_stream", "stream"}, {"ns_default", none}],
    "stream", [], undefined},
  {xmlel, "ns_default", [], "iq", [
    {xmlattr, ?NS_XML_s, "lang", <<"fr">>}
  ], [
    {xmlcdata, <<"Content">>}
  ]},
  {xmlendtag, "ns_stream", "stream"}
]).

-define(SOURCE2, "<element xmlns='unknown_ns' xmlns:stream='http://etherx.jabber.org/streams' xml:lang='fr' stream:version='1.0'/>").

-define(TREE2_NS_CHECK, [
  {xmlel, "unknown_ns",
    [{"unknown_ns", none}, {'http://etherx.jabber.org/streams',"stream"}],
    element, [
    {xmlattr, ?NS_XML, lang, <<"fr">>},
    {xmlattr, ?NS_XMPP, version, <<"1.0">>}
  ], []}
]).

-define(SOURCE3, "<message><unknown/></message>").

-define(TREE3_NS_CHECK, [
  {xmlel, undefined, [], 'message', [], [
      {xmlel, undefined, [], "unknown", [], []}
  ]}
]).

-define(FRAGMENTS3, [
  {xmlelement, "message", [], undefined},
  {xmlelement, "unknown", [], undefined}
]).

-define(SOURCE4, "<stream version='1.0' foo='bar'/>").

-define(TREE4_NS_CHECK, [
  {xmlel, undefined, [], 'stream', [
    {xmlattr, undefined, 'version', <<"1.0">>},
    {xmlattr, undefined, "foo", <<"bar">>}
  ], []}
]).

-define(CHUNK1, "").
-define(CHUNK2, "<stream xml:lang='fr' version='1.0'>Content</strea").
-define(CHUNK3, "m>").

-define(CHUNK1_TREE, continue).
-define(CHUNK2_TREE, continue).
-define(CHUNK3_TREE, [
    {xmlel, undefined, [], "stream", [
    {xmlattr, ?NS_XML_s, "lang", <<"fr">>},
    {xmlattr, undefined, "version", <<"1.0">>}
  ], [
    {xmlcdata, <<"Content">>}
  ]}
]).

-define(BAD_SOURCE1, "<stream attr=>").
-define(BAD_SOURCE2, "</stream>").

start_stop_test_() ->
    Tests = [
      ?_assertMatch(ok, exmpp_xml:stop_parser(exmpp_xml:start_parser()))
    ],
    {setup, ?SETUP, ?CLEANUP, Tests}.

unknown_options_test_() ->
    Tests = [
      ?_assertThrow(
        {xml_parser, options, invalid, _Infos},
        exmpp_xml:start_parser([bad_option])
      ),
      ?_assertThrow(
        {xml_parser, options, invalid, _Infos},
        exmpp_xml:parse_document("", [bad_option])
      )
    ],
    {setup, ?SETUP, ?CLEANUP, Tests}.

options_test_() ->
    Tests = [
      ?_assertMatch(
        ?TREE1_NO_NS,
        exmpp_xml:parse_document(?SOURCE1,
          [{engine, expat_legacy}, {names_as_atom, false}])
      ),
      ?_assertMatch(
        ?TREE1_NO_NS_ATOM,
        exmpp_xml:parse_document(?SOURCE1,
          [{engine, expat_legacy}])
      ),
      ?_assertMatch(
        ?TREE1_NS,
        exmpp_xml:parse_document(?SOURCE1,
          [{names_as_atom, false}])
      ),
      ?_assertMatch(
        ?TREE1_NS_ATOM,
        exmpp_xml:parse_document(?SOURCE1,
          [names_as_atom])
      ),
      ?_assertMatch(
        ?TREE2_NS_CHECK,
        exmpp_xml:parse_document(?SOURCE2,
          [names_as_atom, {check_nss, xmpp}])
      ),
      ?_assertMatch(
        ?TREE3_NS_CHECK,
        exmpp_xml:parse_document(?SOURCE3,
          [names_as_atom, {check_elems, xmpp}])
      ),
      ?_assertMatch(
        ?TREE4_NS_CHECK,
        exmpp_xml:parse_document(?SOURCE4,
          [names_as_atom, {check_attrs, xmpp}])
      ),
      ?_assertMatch(
        ?TREE1_NO_NS,
        exmpp_xml:parse_document(?SOURCE1,
          [{engine, expat_legacy}, {names_as_atom, false},
            {max_size, length(?SOURCE1)}])
      ),
      ?_assertMatch(
        ?TREE1_NO_NS,
        exmpp_xml:parse_document(?SOURCE1,
          [{engine, expat_legacy}, {names_as_atom, false},
            {max_size, infinity}])
      ),
      ?_assertThrow(
        {xml_parser, parsing, stanza_too_big, undefined},
        exmpp_xml:parse_document(?SOURCE1,
          [{max_size, length(?SOURCE1) - 1}])
      ),
      ?_assertMatch(
        ?TREE1_ROOT_DEPTH,
        exmpp_xml:parse_document(?SOURCE1,
          [{engine, expat_legacy}, {names_as_atom, false}, {root_depth, 1}])
      ),
      ?_assertMatch(
        ?TREE1_NS_ROOT_DEPTH,
        exmpp_xml:parse_document(?SOURCE1,
          [{names_as_atom, false},
            {root_depth, 1}])
      ),
      ?_assertMatch(
        ?TREE1_NS_END_EL,
        exmpp_xml:parse_document(?SOURCE1,
          [{names_as_atom, false},
            {root_depth, 1}, emit_endtag])
      )
    ],
    {setup, ?SETUP, ?CLEANUP, Tests}.

chunk_by_chunk_test_() ->
    Setup = fun() ->
        exmpp:start(),
        error_logger:tty(false),
        exmpp_xml:start_parser([{names_as_atom, false}])
    end,
    Cleanup = fun(P) ->
        exmpp_xml:stop_parser(P),
        application:stop(exmpp)
    end,
    Inst = {with, [
        fun(P) ->
            ?assertMatch(?CHUNK1_TREE, exmpp_xml:parse(P, ?CHUNK1))
        end,
        fun(P) ->
            ?assertMatch(?CHUNK2_TREE, exmpp_xml:parse(P, ?CHUNK2))
        end,
        fun(P) ->
            ?assertMatch(?CHUNK3_TREE, exmpp_xml:parse(P, ?CHUNK3))
        end
    ]},
    {setup, Setup, Cleanup, Inst}.

bad_xml_test_() ->
    Setup = fun() ->
        exmpp:start(),
        error_logger:tty(false),
        exmpp_xml:start_parser()
    end,
    Cleanup = fun(P) ->
        exmpp_xml:stop_parser(P),
        application:stop(exmpp)
    end,
    Inst = {with, [
        fun(P) ->
            ?assertThrow(
              {xml_parser, parsing, parsing_failed, _},
              exmpp_xml:parse(P, ?BAD_SOURCE1)
            )
        end,
        fun(P) ->
            ?assertThrow(
              {xml_parser, parsing, parsing_failed, _},
              exmpp_xml:parse_final(P, ?BAD_SOURCE2)
            )
        end
    ]},
    {setup, Setup, Cleanup, Inst}.


empty_uri_test_() ->
    String = "<a xmlns='test'><b xmlns=''><c/></b></a>",
    Expected = [{xmlel,test,[{test,none}],a,[],
                    [{xmlel,undefined,[],b,[],
                        [{xmlel,undefined,[],c,[],[]}]}]}],
    Setup = fun() ->
        exmpp:start(),
        error_logger:tty(false),
        exmpp_xml:start_parser()
    end,
    Cleanup = fun(P) ->
        exmpp_xml:stop_parser(P),
        application:stop(exmpp)
    end,
    Inst = {with, [
        fun(P) ->
            ?assertMatch(Expected, exmpp_xml:parse(P, String))
        end
    ]},
    {setup, Setup, Cleanup, Inst}.
