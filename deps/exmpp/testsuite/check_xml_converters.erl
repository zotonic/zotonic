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

-module(check_xml_converters).

-include("exmpp.hrl").

-export([check/0, do_check/0]).

check() ->
    testsuite:run(fun do_check/0).

do_check() ->
    test_escape_using_entities(),
    test_xmlel_to_xmlelement(),
    case exmpp_xml:internal_escaping_function_name() of
        escape_using_entities ->
            test_document_to_list_without_namespace(),
            test_document_to_list_with_namespace(),
            test_node_to_list_with_namespace(),
            test_node_to_list_with_namespace2();
        escape_using_cdata ->
            ok
    end,
    test_clear_endtag_tuples(),
    ok.

% --------------------------------------------------------------------
% Serializer testsuite.
% --------------------------------------------------------------------

-define(TREE0_NO_NS,
  {xmlelement, "stream:stream", [
      {"xmlns:stream", "ns_stream"}
    ], undefined}
).

-define(TREE0_NS,
  {xmlel, 'ns_stream', [{'ns_stream', "stream"}],
    "stream", [], undefined}
).

-define(TREE0_NS_NAA,
  {xmlel, 'ns_stream', [{'ns_stream', "stream"}],
    'stream', [], undefined}
).

-define(SOURCE0, "<stream:stream xmlns:stream=\"ns_stream\">").

-define(TREE1_NO_NS,
  {xmlelement, "stream:stream", [
      {"xmlns:stream", "ns_stream"}
    ], []}
).

-define(TREE1_NS,
  {xmlel, 'ns_stream', [{'ns_stream', "stream"}],
    "stream", [], []}
).

-define(SOURCE1, "<stream:stream xmlns:stream=\"ns_stream\"/>").

-define(TREE2_NO_NS,
  {xmlelement, "stream:stream", [
      {"xmlns:stream", "ns_stream"},
      {"xmlns", "ns_default"}
    ], [
      {xmlelement, "iq", [
          {"xml:lang", "fr"},
          {"version", "1.0"}
        ], [
          {xmlcdata, <<"Content">>}
        ]}
    ]}
).

-define(TREE2_NS,
  {xmlel, 'ns_stream', [{'ns_stream', "stream"}, {'ns_default', none}],
    "stream", [], [
      {xmlel, 'ns_default', [], "iq", [
          {xmlattr, ?NS_XML, undefined, "lang", "fr"},
          {xmlattr, undefined, undefined, "version", "1.0"}
        ], [
          {xmlcdata, <<"Content">>}
        ]}
    ]}
).

-define(TREE2_NS_NAA,
  {xmlel, 'ns_stream', [{'ns_stream', "stream"}, {'ns_default', none}],
    'stream', [], [
      {xmlel, 'ns_default', [], 'iq', [
          {xmlattr, ?NS_XML, undefined, 'lang', "fr"},
          {xmlattr, undefined, undefined, 'version', "1.0"}
        ], [
          {xmlcdata, <<"Content">>}
        ]}
    ]}
).

-define(SOURCE2, "<stream:stream xmlns:stream=\"ns_stream\" xmlns=\"ns_default\"><iq xml:lang=\"fr\" version=\"1.0\">Content</iq></stream:stream>").

-define(TREE3_NS,
  {xmlel, 'ns_iq', [], "iq", [
      {xmlattr, ?NS_XML, undefined, "lang", "fr"}
    ], [
      {xmlcdata, <<"Binary">>},
      {xmlcdata, "List"},
      {xmlcdata, <<"& < > \" '">>}
    ]}
).

-define(TREE3_DEFAULT_NS, [
    'ns_iq'
  ]).

-define(TREE3_PREFIXED_NS, [
  ]).

-define(SOURCE3, "<iq xml:lang=\"fr\">BinaryList&amp; &lt; &gt; &quot; &apos;</iq>").

-define(TREE3_DEFAULT_NS_2, [
  ]).

-define(TREE3_PREFIXED_NS_2, [
    {'ns_iq', "jabber"}
  ]).

-define(SOURCE3_2, "<jabber:iq xml:lang=\"fr\">BinaryList&amp; &lt; &gt; &quot; &apos;</jabber:iq>").

-define(TREE4_NO_NS,
  {xmlelement, "stream", [
      {"xmlns:pfx", "ns_attr"},
      {"pfx:foo", "bar"}
    ], []}
).

-define(TREE4_NS,
  {xmlel, undefined, [], "stream", [
      {xmlattr, 'ns_attr', "pfx", "foo", "bar"}
    ], []}
).

-define(TREE5_NO_NS,
  {xmlendtag, undefined, undefined, "stream:stream"}
).

-define(TREE5_NO_NS2,
  {xmlendtag, undefined, undefined, "stream"}
).

-define(TREE5_NO_NS3,
  {xmlendtag, undefined, undefined, "stream2:stream"}
).

-define(TREE5_NS,
  {xmlendtag, 'ns_stream', "stream", "stream"}
).

-define(TREE5_NS2,
  {xmlendtag, 'ns_stream', undefined, "stream"}
).

-define(TREE5_NS3,
  {xmlendtag, undefined, undefined, "stream"}
).

-define(TREE5_NS_NAA,
  {xmlendtag, 'ns_stream', "stream", 'stream'}
).

-define(SOURCE5, "</stream:stream>").


-define(TREE6_NO_NS,
  {xmlelement, "stream:stream", [
      {"xmlns", "ns_default"},
      {"xmlns:stream", "ns_stream"},
      {"xmlns:other", "ns_other"}
    ], []}
).

-define(TREE6_NS,
  {xmlel, 'ns_stream', [
    {'ns_default', none},
    {'ns_stream', "stream"},
    {'ns_other', "other"}
  ], "stream", [], []}
).

-define(SOURCE6, "<stream:stream xmlns=\"ns_default\" xmlns:stream=\"ns_stream\" xmlns:other=\"ns_other\"/>").

test_xmlel_to_xmlelement() ->
    testsuite:is(exmpp_xml:xmlel_to_xmlelement(?TREE0_NO_NS),
      ?TREE0_NO_NS),
    testsuite:is(exmpp_xml:xmlel_to_xmlelement(?TREE0_NS),
      ?TREE0_NO_NS),
    testsuite:is(exmpp_xml:xmlel_to_xmlelement(?TREE0_NS_NAA),
      ?TREE0_NO_NS),
    testsuite:is(exmpp_xml:xmlel_to_xmlelement(?TREE1_NO_NS),
      ?TREE1_NO_NS),
    testsuite:is(exmpp_xml:xmlel_to_xmlelement(?TREE1_NS),
      ?TREE1_NO_NS),
    testsuite:is(exmpp_xml:xmlel_to_xmlelement(?TREE2_NO_NS),
      ?TREE2_NO_NS),
    testsuite:is(exmpp_xml:xmlel_to_xmlelement(?TREE2_NS),
      ?TREE2_NO_NS),
    testsuite:is(exmpp_xml:xmlel_to_xmlelement(?TREE2_NS_NAA),
      ?TREE2_NO_NS),
    testsuite:is(exmpp_xml:xmlel_to_xmlelement(?TREE4_NO_NS),
      ?TREE4_NO_NS),
    testsuite:is(exmpp_xml:xmlel_to_xmlelement(?TREE4_NS),
      ?TREE4_NO_NS),
    testsuite:is(exmpp_xml:xmlel_to_xmlelement(?TREE5_NO_NS),
      ?TREE5_NO_NS),
    testsuite:is(exmpp_xml:xmlel_to_xmlelement(?TREE5_NS),
      ?TREE5_NO_NS),
    testsuite:is(exmpp_xml:xmlel_to_xmlelement(?TREE5_NS_NAA),
      ?TREE5_NO_NS),
    testsuite:is(exmpp_xml:xmlel_to_xmlelement(?TREE5_NS,
        ['ns_stream', 'other'], []),
      ?TREE5_NO_NS2),
    testsuite:is(exmpp_xml:xmlel_to_xmlelement(?TREE5_NS,
        [], [{'ns_stream', "stream2"}]),
      ?TREE5_NO_NS3),
    testsuite:is(exmpp_xml:xmlel_to_xmlelement(?TREE5_NS2),
      ?TREE5_NO_NS2),
    testsuite:is(exmpp_xml:xmlel_to_xmlelement(?TREE5_NS3),
      ?TREE5_NO_NS2),
    ok.

test_document_to_list_without_namespace() ->
    testsuite:is(
      lists:flatten(exmpp_xml:document_to_list(?TREE0_NO_NS)),
      ?SOURCE0),
    testsuite:is(
      lists:flatten(exmpp_xml:document_to_list(?TREE1_NO_NS)),
      ?SOURCE1),
    testsuite:is(
      lists:flatten(exmpp_xml:document_to_list(?TREE2_NO_NS)),
      ?SOURCE2),
    testsuite:is(
      lists:flatten(exmpp_xml:document_to_list(?TREE5_NO_NS)),
      ?SOURCE5),
    testsuite:is(
      lists:flatten(exmpp_xml:document_to_list(?TREE6_NO_NS)),
      ?SOURCE6),
    ok.

test_document_to_list_with_namespace() ->
    testsuite:is(
      lists:flatten(exmpp_xml:document_to_list(?TREE0_NS)),
      ?SOURCE0),
    testsuite:is(
      lists:flatten(exmpp_xml:document_to_list(?TREE1_NS)),
      ?SOURCE1),
    testsuite:is(
      lists:flatten(exmpp_xml:document_to_list(?TREE2_NS)),
      ?SOURCE2),
    testsuite:is(
      lists:flatten(exmpp_xml:document_to_list(?TREE5_NS)),
      ?SOURCE5),
    testsuite:is(
      lists:flatten(exmpp_xml:document_to_list(?TREE5_NS_NAA)),
      ?SOURCE5),
    testsuite:is(
      lists:flatten(exmpp_xml:document_to_list(?TREE6_NS)),
      ?SOURCE6),
    ok.

test_node_to_list_with_namespace() ->
    testsuite:is(
      lists:flatten(exmpp_xml:node_to_list(?TREE3_NS,
          ?TREE3_DEFAULT_NS, ?TREE3_PREFIXED_NS)),
      ?SOURCE3),
    ok.

test_node_to_list_with_namespace2() ->
    testsuite:is(
      lists:flatten(exmpp_xml:node_to_list(?TREE3_NS,
          ?TREE3_DEFAULT_NS_2, ?TREE3_PREFIXED_NS_2)),
      ?SOURCE3_2),
    ok.

test_escape_using_entities() ->
    testsuite:is(exmpp_xml:escape_using_entities("Entities: &<>\"'"),
      "Entities: &amp;&lt;&gt;&quot;&apos;"),
    testsuite:is(exmpp_xml:escape_using_entities(<<"Entities: &<>\"'">>),
      <<"Entities: &amp;&lt;&gt;&quot;&apos;">>),
    ok.

test_clear_endtag_tuples() ->
    testsuite:is(exmpp_xml:clear_endtag_tuples([?TREE5_NO_NS]), []),
    testsuite:is(exmpp_xml:clear_endtag_tuples(
        [?TREE0_NO_NS, ?TREE5_NO_NS]), [?TREE0_NO_NS]),
    testsuite:is(exmpp_xml:clear_endtag_tuples([?TREE5_NS]), []),
    testsuite:is(exmpp_xml:clear_endtag_tuples(
        [?TREE0_NS, ?TREE5_NS]), [?TREE0_NS]),
    ok.
