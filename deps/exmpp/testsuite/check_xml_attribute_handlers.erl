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

-module(check_xml_attribute_handlers).

-include("exmpp.hrl").

-export([check/0, do_check/0]).

check() ->
    testsuite:run(fun do_check/0).

do_check() ->
    test_get_attribute_node_from_list2(),
    test_get_attribute_node_from_list3(),
    test_get_attribute_node2(),
    test_get_attribute_node3(),
    test_get_attribute_from_list2(),
    test_get_attribute_from_list3(),
    test_get_attribute2(),
    test_get_attribute3(),
    test_set_attribute_in_list3(),
    test_set_attribute_in_list4(),
    test_set_attribute3(),
    test_set_attribute4(),
    test_set_attributes(),
    test_set_attributes_ns(),
    test_has_attribute_in_list2(),
    test_has_attribute_in_list3(),
    test_has_attribute2(),
    test_has_attribute3(),
    test_remove_attribute_from_list2(),
    test_remove_attribute_from_list3(),
    test_remove_attribute2(),
    test_remove_attribute3(),
    ok.

% --------------------------------------------------------------------
% Attribute handlers testsuite.
% --------------------------------------------------------------------

-define(ATTRIBUTE_LIST1, []).

-define(ATTRIBUTE_LIST2_1, [
    {"version", "1.0"}
  ]).
-define(ATTRIBUTE_LIST2_2, [
    {"version", "1.0"},
    {"xml:lang", "fr"}
  ]).

-define(ATTRIBUTE_LIST3_1, [
    {xmlattr, undefined, undefined, "version", "1.0"}
  ]).
-define(ATTRIBUTE_LIST3_2, [
    {xmlattr, undefined, undefined, "version", "1.0"},
    {xmlattr, ?NS_XML, undefined, "lang", "fr"}
  ]).

-define(ATTRIBUTE_LIST4, [
    bad_data
  ]).

-define(ELEMENT1, {xmlel,
    undefined, [], "element",
    ?ATTRIBUTE_LIST1,
    []}
).

-define(ELEMENT2_1, {xmlelement,
    "element",
    ?ATTRIBUTE_LIST2_1,
    []}
).

-define(ELEMENT2_2, {xmlelement,
    "element",
    ?ATTRIBUTE_LIST2_2,
    []}
).

-define(ELEMENT3_1, {xmlel,
    undefined, [], "element",
    ?ATTRIBUTE_LIST3_1,
    []}
).

-define(ELEMENT3_2, {xmlel,
    undefined, [], "element",
    ?ATTRIBUTE_LIST3_2,
    []}
).

-define(ELEMENT4, {xmlel,
    undefined, [], "bad_element",
    ?ATTRIBUTE_LIST4,
    []}
).

-define(ELEMENT5, {xmlelement,
    "element",
    [],
    [
      ?ELEMENT1
    ]}
).

-define(ELEMENT6, {xmlel,
    undefined, [], "element",
    [],
    [
      {cdata, <<"Content 1">>}
    ]}
).

-define(ELEMENT7, {xmlel,
    undefined, [], "element",
    [],
    [
      ?ELEMENT1,
      {cdata, <<"Content 1">>},
      ?ELEMENT1
    ]}
).

test_get_attribute_node_from_list2() ->
    testsuite:is(exmpp_xml:get_attribute_node_from_list(
        ?ATTRIBUTE_LIST1, "xml:lang"),
      undefined),
    testsuite:is(exmpp_xml:get_attribute_node_from_list(
        ?ATTRIBUTE_LIST2_1, "xml:lang"),
      undefined),
    testsuite:is(exmpp_xml:get_attribute_node_from_list(
        ?ATTRIBUTE_LIST2_2, "xml:lang"),
      {"xml:lang", "fr"}),
    testsuite:is(exmpp_xml:get_attribute_node_from_list(
        ?ATTRIBUTE_LIST3_1, "lang"),
      undefined),
    testsuite:is(exmpp_xml:get_attribute_node_from_list(
        ?ATTRIBUTE_LIST3_2, "lang"),
      {xmlattr, ?NS_XML, undefined, "lang", "fr"}),
    testsuite:is(exmpp_xml:get_attribute_node_from_list(
        ?ATTRIBUTE_LIST4, "lang"),
      undefined),
    ok.

test_get_attribute_node_from_list3() ->
    testsuite:is(exmpp_xml:get_attribute_node_from_list(
        ?ATTRIBUTE_LIST1, ?NS_XML, "lang"),
      undefined),
    testsuite:is(exmpp_xml:get_attribute_node_from_list(
        ?ATTRIBUTE_LIST3_1, ?NS_XML, "lang"),
      undefined),
    testsuite:is(exmpp_xml:get_attribute_node_from_list(
        ?ATTRIBUTE_LIST3_2, ?NS_XML, "lang"),
      {xmlattr, ?NS_XML, undefined, "lang", "fr"}),
    testsuite:is(exmpp_xml:get_attribute_node_from_list(
        ?ATTRIBUTE_LIST4, ?NS_XML, "lang"),
      undefined),
    ok.

test_get_attribute_node2() ->
    testsuite:is(exmpp_xml:get_attribute_node(
        undefined, "xml:lang"),
      undefined),
    testsuite:is(exmpp_xml:get_attribute_node(
        ?ELEMENT1, "xml:lang"),
      undefined),
    testsuite:is(exmpp_xml:get_attribute_node(
        ?ELEMENT2_1, "xml:lang"),
      undefined),
    testsuite:is(exmpp_xml:get_attribute_node(
        ?ELEMENT2_2, "xml:lang"),
      {"xml:lang", "fr"}),
    testsuite:is(exmpp_xml:get_attribute_node(
        ?ELEMENT3_1, "lang"),
      undefined),
    testsuite:is(exmpp_xml:get_attribute_node(
        ?ELEMENT3_2, "lang"),
      {xmlattr, ?NS_XML, undefined, "lang", "fr"}),
    testsuite:is(exmpp_xml:get_attribute_node(
        ?ELEMENT4, "lang"),
      undefined),
    ok.

test_get_attribute_node3() ->
    testsuite:is(exmpp_xml:get_attribute_node(
        undefined, ?NS_XML, "lang"),
      undefined),
    testsuite:is(exmpp_xml:get_attribute_node(
        ?ELEMENT1, ?NS_XML, "lang"),
      undefined),
    testsuite:is(exmpp_xml:get_attribute_node(
        ?ELEMENT3_1, ?NS_XML, "lang"),
      undefined),
    testsuite:is(exmpp_xml:get_attribute_node(
        ?ELEMENT3_2, ?NS_XML, "lang"),
      {xmlattr, ?NS_XML, undefined, "lang", "fr"}),
    testsuite:is(exmpp_xml:get_attribute_node(
        ?ELEMENT4, ?NS_XML, "lang"),
      undefined),
    ok.

test_get_attribute_from_list2() ->
    testsuite:is(exmpp_xml:get_attribute_from_list(
        ?ATTRIBUTE_LIST1, "xml:lang"),
      ""),
    testsuite:is(exmpp_xml:get_attribute_from_list(
        ?ATTRIBUTE_LIST2_1, "xml:lang"),
      ""),
    testsuite:is(exmpp_xml:get_attribute_from_list(
        ?ATTRIBUTE_LIST2_2, "xml:lang"),
      "fr"),
    testsuite:is(exmpp_xml:get_attribute_from_list(
        ?ATTRIBUTE_LIST3_1, "lang"),
      ""),
    testsuite:is(exmpp_xml:get_attribute_from_list(
        ?ATTRIBUTE_LIST3_2, "lang"),
      "fr"),
    testsuite:is(exmpp_xml:get_attribute_from_list(
        ?ATTRIBUTE_LIST4, "lang"),
      ""),
    ok.

test_get_attribute_from_list3() ->
    testsuite:is(exmpp_xml:get_attribute_from_list(
        ?ATTRIBUTE_LIST1, ?NS_XML, "lang"),
      ""),
    testsuite:is(exmpp_xml:get_attribute_from_list(
        ?ATTRIBUTE_LIST3_1, ?NS_XML, "lang"),
      ""),
    testsuite:is(exmpp_xml:get_attribute_from_list(
        ?ATTRIBUTE_LIST3_2, ?NS_XML, "lang"),
      "fr"),
    testsuite:is(exmpp_xml:get_attribute_from_list(
        ?ATTRIBUTE_LIST4, ?NS_XML, "lang"),
      ""),
    ok.

test_get_attribute2() ->
    testsuite:is(exmpp_xml:get_attribute(
        undefined, "xml:lang"),
      ""),
    testsuite:is(exmpp_xml:get_attribute(
        ?ELEMENT1, "xml:lang"),
      ""),
    testsuite:is(exmpp_xml:get_attribute(
        ?ELEMENT2_1, "xml:lang"),
      ""),
    testsuite:is(exmpp_xml:get_attribute(
        ?ELEMENT2_2, "xml:lang"),
      "fr"),
    testsuite:is(exmpp_xml:get_attribute(
        ?ELEMENT3_1, "lang"),
      ""),
    testsuite:is(exmpp_xml:get_attribute(
        ?ELEMENT3_2, "lang"),
      "fr"),
    testsuite:is(exmpp_xml:get_attribute(
        ?ELEMENT4, "lang"),
      ""),
    ok.

test_get_attribute3() ->
    testsuite:is(exmpp_xml:get_attribute(
        undefined, ?NS_XML, "lang"),
      ""),
    testsuite:is(exmpp_xml:get_attribute(
        ?ELEMENT1, ?NS_XML, "lang"),
      ""),
    testsuite:is(exmpp_xml:get_attribute(
        ?ELEMENT3_1, ?NS_XML, "lang"),
      ""),
    testsuite:is(exmpp_xml:get_attribute(
        ?ELEMENT3_2, ?NS_XML, "lang"),
      "fr"),
    testsuite:is(exmpp_xml:get_attribute(
        ?ELEMENT4, ?NS_XML, "lang"),
      ""),
    ok.

test_has_attribute_in_list2() ->
    testsuite:is(exmpp_xml:has_attribute_in_list(
        ?ATTRIBUTE_LIST1, "xml:lang"), false),
    testsuite:is(exmpp_xml:has_attribute_in_list(
        ?ATTRIBUTE_LIST2_1, "xml:lang"), false),
    testsuite:is(exmpp_xml:has_attribute_in_list(
        ?ATTRIBUTE_LIST2_2, "xml:lang"), true),
    testsuite:is(exmpp_xml:has_attribute_in_list(
        ?ATTRIBUTE_LIST3_1, "lang"), false),
    testsuite:is(exmpp_xml:has_attribute_in_list(
        ?ATTRIBUTE_LIST3_2, "lang"), true),
    testsuite:is(exmpp_xml:has_attribute_in_list(
        ?ATTRIBUTE_LIST4, "lang"), false),
    ok.

test_has_attribute_in_list3() ->
    testsuite:is(exmpp_xml:has_attribute_in_list(
        ?ATTRIBUTE_LIST1, ?NS_XML, "lang"), false),
    testsuite:is(exmpp_xml:has_attribute_in_list(
        ?ATTRIBUTE_LIST3_1, ?NS_XML, "lang"), false),
    testsuite:is(exmpp_xml:has_attribute_in_list(
        ?ATTRIBUTE_LIST3_2, ?NS_XML, "lang"), true),
    testsuite:is(exmpp_xml:has_attribute_in_list(
        ?ATTRIBUTE_LIST4, ?NS_XML, "lang"), false),
    ok.

test_has_attribute2() ->
    testsuite:is(exmpp_xml:has_attribute(
        undefined, "xml:lang"), false),
    testsuite:is(exmpp_xml:has_attribute(
        ?ELEMENT1, "xml:lang"), false),
    testsuite:is(exmpp_xml:has_attribute(
        ?ELEMENT2_1, "xml:lang"), false),
    testsuite:is(exmpp_xml:has_attribute(
        ?ELEMENT2_2, "xml:lang"), true),
    testsuite:is(exmpp_xml:has_attribute(
        ?ELEMENT3_1, "lang"), false),
    testsuite:is(exmpp_xml:has_attribute(
        ?ELEMENT3_2, "lang"), true),
    testsuite:is(exmpp_xml:has_attribute(
        ?ELEMENT4, "lang"), false),
    ok.

test_has_attribute3() ->
    testsuite:is(exmpp_xml:has_attribute(
        undefined, ?NS_XML, "lang"), false),
    testsuite:is(exmpp_xml:has_attribute(
        ?ELEMENT1, ?NS_XML, "lang"), false),
    testsuite:is(exmpp_xml:has_attribute(
        ?ELEMENT3_1, ?NS_XML, "lang"), false),
    testsuite:is(exmpp_xml:has_attribute(
        ?ELEMENT3_2, ?NS_XML, "lang"), true),
    testsuite:is(exmpp_xml:has_attribute(
        ?ELEMENT4, ?NS_XML, "lang"), false),
    ok.

test_set_attribute_in_list3() ->
    New1 = exmpp_xml:set_attribute_in_list(?ATTRIBUTE_LIST1,
      "lang", "en"),
    testsuite:is(New1, [
        {xmlattr, undefined, undefined, "lang", "en"}
      ]),
    New2_1 = exmpp_xml:set_attribute_in_list(?ATTRIBUTE_LIST2_1,
      "xml:lang", "en"),
    testsuite:is(New2_1, [
        {"version", "1.0"},
        {"xml:lang","en"}
      ]),
    New2_2 = exmpp_xml:set_attribute_in_list(?ATTRIBUTE_LIST2_2,
      "xml:lang", "en"),
    testsuite:is(New2_2, [
        {"version", "1.0"},
        {"xml:lang","en"}
      ]),
    New3_1 = exmpp_xml:set_attribute_in_list(?ATTRIBUTE_LIST3_1,
      "lang", "en"),
    testsuite:is(New3_1, [
        {xmlattr, undefined, undefined, "version", "1.0"},
        {xmlattr, undefined, undefined, "lang", "en"}
      ]),
    New3_2 = exmpp_xml:set_attribute_in_list(?ATTRIBUTE_LIST3_2,
      "lang", "en"),
    testsuite:is(New3_2, [
        {xmlattr, undefined, undefined, "version", "1.0"},
        {xmlattr, ?NS_XML, undefined, "lang", "en"}
      ]),
    New4 = exmpp_xml:set_attribute_in_list(?ATTRIBUTE_LIST4,
      "lang", "en"),
    testsuite:is(New4, [
        bad_data,
        {xmlattr, undefined, undefined, "lang", "en"}
      ]),
    ok.

test_set_attribute_in_list4() ->
    New1 = exmpp_xml:set_attribute_in_list(?ATTRIBUTE_LIST1,
      ?NS_XML, "lang", "en"),
    testsuite:is(New1, [
        {xmlattr, ?NS_XML, undefined, "lang", "en"}
      ]),
    New3_1 = exmpp_xml:set_attribute_in_list(?ATTRIBUTE_LIST3_1,
      ?NS_XML, "lang", "en"),
    testsuite:is(New3_1, [
        {xmlattr, undefined, undefined, "version", "1.0"},
        {xmlattr, ?NS_XML, undefined, "lang", "en"}
      ]),
    New3_2 = exmpp_xml:set_attribute_in_list(?ATTRIBUTE_LIST3_2,
      ?NS_XML, "lang", "en"),
    testsuite:is(New3_2, [
        {xmlattr, undefined, undefined, "version", "1.0"},
        {xmlattr, ?NS_XML, undefined, "lang", "en"}
      ]),
    New3_3 = exmpp_xml:set_attribute_in_list(?ATTRIBUTE_LIST3_2,
      'some_other_ns', "lang", "en"),
    testsuite:is(New3_3, [
        {xmlattr, undefined, undefined, "version", "1.0"},
        {xmlattr, ?NS_XML, undefined, "lang", "fr"},
        {xmlattr, 'some_other_ns', undefined, "lang", "en"}
      ]),
    New4 = exmpp_xml:set_attribute_in_list(?ATTRIBUTE_LIST4,
      ?NS_XML, "lang", "en"),
    testsuite:is(New4, [
        bad_data,
        {xmlattr, ?NS_XML, undefined, "lang", "en"}
      ]),
    ok.

test_set_attribute3() ->
    New1 = exmpp_xml:set_attribute(?ELEMENT1,
      "lang", "en"),
    testsuite:is(New1, {xmlel,
        undefined, [], "element", [
          {xmlattr, undefined, undefined, "lang", "en"}
        ], []
      }),
    New2_1 = exmpp_xml:set_attribute(?ELEMENT2_1,
      "xml:lang", "en"),
    testsuite:is(New2_1, {xmlelement,
        "element", [
          {"version", "1.0"},
          {"xml:lang","en"}
        ], []
      }),
    New2_2 = exmpp_xml:set_attribute(?ELEMENT2_2,
      "xml:lang", "en"),
    testsuite:is(New2_2, {xmlelement,
        "element", [
          {"version", "1.0"},
          {"xml:lang","en"}
        ], []
      }),
    New3_1 = exmpp_xml:set_attribute(?ELEMENT3_1,
      "lang", "en"),
    testsuite:is(New3_1, {xmlel,
        undefined, [], "element", [
          {xmlattr, undefined, undefined, "version", "1.0"},
          {xmlattr, undefined, undefined, "lang", "en"}
        ], []
      }),
    New3_2 = exmpp_xml:set_attribute(?ELEMENT3_2,
      "lang", "en"),
    testsuite:is(New3_2, {xmlel,
        undefined, [], "element", [
          {xmlattr, undefined, undefined, "version", "1.0"},
          {xmlattr, ?NS_XML, undefined, "lang", "en"}
        ], []
      }),
    New4 = exmpp_xml:set_attribute(?ELEMENT4,
      "lang", "en"),
    testsuite:is(New4, {xmlel,
        undefined, [], "bad_element", [
          bad_data,
          {xmlattr, undefined, undefined, "lang", "en"}
        ], []
      }),
    ok.

test_set_attribute4() ->
    New1 = exmpp_xml:set_attribute(?ELEMENT1,
      ?NS_XML, "lang", "en"),
    testsuite:is(New1, {xmlel,
        undefined, [], "element", [
          {xmlattr, ?NS_XML, undefined, "lang", "en"}
        ], []
      }),
    New3_1 = exmpp_xml:set_attribute(?ELEMENT3_1,
      ?NS_XML, "lang", "en"),
    testsuite:is(New3_1, {xmlel,
        undefined, [], "element", [
          {xmlattr, undefined, undefined, "version", "1.0"},
          {xmlattr, ?NS_XML, undefined, "lang", "en"}
        ], []
      }),
    New3_2 = exmpp_xml:set_attribute(?ELEMENT3_2,
      ?NS_XML, "lang", "en"),
    testsuite:is(New3_2, {xmlel,
        undefined, [], "element", [
          {xmlattr, undefined, undefined, "version", "1.0"},
          {xmlattr, ?NS_XML, undefined, "lang", "en"}
        ], []
      }),
    New3_3 = exmpp_xml:set_attribute(?ELEMENT3_2,
      'some_other_ns', "lang", "en"),
    testsuite:is(New3_3, {xmlel,
        undefined, [], "element", [
          {xmlattr, undefined, undefined, "version", "1.0"},
          {xmlattr, ?NS_XML, undefined, "lang", "fr"},
          {xmlattr, 'some_other_ns', undefined, "lang", "en"}
        ], []
      }),
    New4 = exmpp_xml:set_attribute(?ELEMENT4,
      ?NS_XML, "lang", "en"),
    testsuite:is(New4, {xmlel,
        undefined, [], "bad_element", [
          bad_data,
          {xmlattr, ?NS_XML, undefined, "lang", "en"}
        ], []
      }),
    ok.

test_set_attributes() ->
    New1 = exmpp_xml:set_attributes(?ELEMENT1, []),
    testsuite:is(New1, ?ELEMENT1),
    New3_1 = exmpp_xml:set_attributes(?ELEMENT3_1,
      [{"version", "2.0"}, {"lang", "en"}]),
    testsuite:is(New3_1, {xmlel,
        undefined, [], "element", [
          {xmlattr, undefined, undefined, "version", "2.0"},
          {xmlattr, undefined, undefined, "lang", "en"}
        ], []
      }),
    New3_2 = exmpp_xml:set_attributes(?ELEMENT3_2,
      [{"version", "2.0"}, {"lang", "en"}]),
    testsuite:is(New3_2, {xmlel,
        undefined, [], "element", [
          {xmlattr, undefined, undefined, "version", "2.0"},
          {xmlattr, ?NS_XML, undefined, "lang", "en"}
        ], []
      }),
    ok.

test_set_attributes_ns() ->
    New3_1 = exmpp_xml:set_attributes(?ELEMENT3_1,
      [{undefined, "version", "2.0"}, {?NS_XML, "lang", "en"}]),
    testsuite:is(New3_1, {xmlel,
        undefined, [], "element", [
          {xmlattr, undefined, undefined, "version", "2.0"},
          {xmlattr, ?NS_XML, undefined, "lang", "en"}
        ], []
      }),
    New3_2 = exmpp_xml:set_attributes(?ELEMENT3_2,
      [{undefined, "version", "2.0"}, {?NS_XML, "lang", "en"}]),
    testsuite:is(New3_2, {xmlel,
        undefined, [], "element", [
          {xmlattr, undefined, undefined, "version", "2.0"},
          {xmlattr, ?NS_XML, undefined, "lang", "en"}
        ], []
      }),
    ok.

test_remove_attribute_from_list2() ->
    New1 = exmpp_xml:remove_attribute_from_list(?ATTRIBUTE_LIST1,
      "lang"),
    testsuite:is(New1, []),
    New2_1 = exmpp_xml:remove_attribute_from_list(?ATTRIBUTE_LIST2_1,
      "xml:lang"),
    testsuite:is(New2_1, [
        {"version", "1.0"}
      ]),
    New2_2 = exmpp_xml:remove_attribute_from_list(?ATTRIBUTE_LIST2_2,
      "xml:lang"),
    testsuite:is(New2_2, [
        {"version", "1.0"}
      ]),
    New3_1 = exmpp_xml:remove_attribute_from_list(?ATTRIBUTE_LIST3_1,
      "lang"),
    testsuite:is(New3_1, [
        {xmlattr, undefined, undefined, "version", "1.0"}
      ]),
    New3_2 = exmpp_xml:remove_attribute_from_list(?ATTRIBUTE_LIST3_2,
      "lang"),
    testsuite:is(New3_2, [
        {xmlattr, undefined, undefined, "version", "1.0"}
      ]),
    New4 = exmpp_xml:remove_attribute_from_list(?ATTRIBUTE_LIST4,
      "lang"),
    testsuite:is(New4, [
        bad_data
      ]),
    ok.

test_remove_attribute_from_list3() ->
    New1 = exmpp_xml:remove_attribute_from_list(?ATTRIBUTE_LIST1,
      ?NS_XML, "lang"),
    testsuite:is(New1, []),
    New3_1 = exmpp_xml:remove_attribute_from_list(?ATTRIBUTE_LIST3_1,
      ?NS_XML, "lang"),
    testsuite:is(New3_1, [
        {xmlattr, undefined, undefined, "version", "1.0"}
      ]),
    New3_2 = exmpp_xml:remove_attribute_from_list(?ATTRIBUTE_LIST3_2,
      ?NS_XML, "lang"),
    testsuite:is(New3_2, [
        {xmlattr, undefined, undefined, "version", "1.0"}
      ]),
    New3_3 = exmpp_xml:remove_attribute_from_list(?ATTRIBUTE_LIST3_2,
      'some_other_ns', "lang"),
    testsuite:is(New3_3, [
        {xmlattr, undefined, undefined, "version", "1.0"},
        {xmlattr, ?NS_XML, undefined, "lang", "fr"}
      ]),
    New4 = exmpp_xml:remove_attribute_from_list(?ATTRIBUTE_LIST4,
      ?NS_XML, "lang"),
    testsuite:is(New4, [
        bad_data
      ]),
    ok.

test_remove_attribute2() ->
    New1 = exmpp_xml:remove_attribute(?ELEMENT1,
      "lang"),
    testsuite:is(New1, {xmlel,
        undefined, [], "element", [
        ], []
      }),
    New2_1 = exmpp_xml:remove_attribute(?ELEMENT2_1,
      "xml:lang"),
    testsuite:is(New2_1, {xmlelement,
        "element", [
          {"version", "1.0"}
        ], []
      }),
    New2_2 = exmpp_xml:remove_attribute(?ELEMENT2_2,
      "xml:lang"),
    testsuite:is(New2_2, {xmlelement,
        "element", [
          {"version", "1.0"}
        ], []
      }),
    New3_1 = exmpp_xml:remove_attribute(?ELEMENT3_1,
      "lang"),
    testsuite:is(New3_1, {xmlel,
        undefined, [], "element", [
          {xmlattr, undefined, undefined, "version", "1.0"}
        ], []
      }),
    New3_2 = exmpp_xml:remove_attribute(?ELEMENT3_2,
      "lang"),
    testsuite:is(New3_2, {xmlel,
        undefined, [], "element", [
          {xmlattr, undefined, undefined, "version", "1.0"}
        ], []
      }),
    New4 = exmpp_xml:remove_attribute(?ELEMENT4,
      "lang"),
    testsuite:is(New4, {xmlel,
        undefined, [], "bad_element", [
          bad_data
        ], []
      }),
    ok.

test_remove_attribute3() ->
    New1 = exmpp_xml:remove_attribute(?ELEMENT1,
      ?NS_XML, "lang"),
    testsuite:is(New1, {xmlel,
        undefined, [], "element", [
        ], []
      }),
    New3_1 = exmpp_xml:remove_attribute(?ELEMENT3_1,
      ?NS_XML, "lang"),
    testsuite:is(New3_1, {xmlel,
        undefined, [], "element", [
          {xmlattr, undefined, undefined, "version", "1.0"}
        ], []
      }),
    New3_2 = exmpp_xml:remove_attribute(?ELEMENT3_2,
      ?NS_XML, "lang"),
    testsuite:is(New3_2, {xmlel,
        undefined, [], "element", [
          {xmlattr, undefined, undefined, "version", "1.0"}
        ], []
      }),
    New3_3 = exmpp_xml:remove_attribute(?ELEMENT3_2,
      'some_other_ns', "lang"),
    testsuite:is(New3_3, {xmlel,
        undefined, [], "element", [
          {xmlattr, undefined, undefined, "version", "1.0"},
          {xmlattr, ?NS_XML, undefined, "lang", "fr"}
        ], []
      }),
    New4 = exmpp_xml:remove_attribute(?ELEMENT4,
      ?NS_XML, "lang"),
    testsuite:is(New4, {xmlel,
        undefined, [], "bad_element", [
          bad_data
        ], []
      }),
    ok.
