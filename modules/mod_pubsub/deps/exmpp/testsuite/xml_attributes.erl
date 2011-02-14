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

-module(xml_attributes).

-include_lib("eunit/include/eunit.hrl").

-include("exmpp.hrl").

% --------------------------------------------------------------------
% Data.
% --------------------------------------------------------------------

-define(ATTRS1, []).

-define(ATTRS2_1, [
    {"version", "1.0"}
  ]).
-define(ATTRS2_2, [
    {"version", "1.0"},
    {"xml:lang", "fr"}
  ]).

-define(ATTRS3_1, [
    {xmlattr, undefined, "version", <<"1.0">>}
  ]).
-define(ATTRS3_2, [
    {xmlattr, undefined, "version", <<"1.0">>},
    {xmlattr, ?NS_XML, "lang", <<"fr">>}
  ]).

-define(ATTRS4, [
    bad_data
  ]).

-define(ELEMENT1, {xmlel,
    undefined, [], "element",
    ?ATTRS1,
    []}
).

-define(ELEMENT2_1, {xmlelement,
    "element",
    ?ATTRS2_1,
    []}
).

-define(ELEMENT2_2, {xmlelement,
    "element",
    ?ATTRS2_2,
    []}
).

-define(ELEMENT3_1, {xmlel,
    undefined, [], "element",
    ?ATTRS3_1,
    []}
).

-define(ELEMENT3_2, {xmlel,
    undefined, [], "element",
    ?ATTRS3_2,
    []}
).

-define(ELEMENT4, {xmlel,
    undefined, [], "bad_element",
    ?ATTRS4,
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

% --------------------------------------------------------------------
% Testsuite.
% --------------------------------------------------------------------

creation_test_() ->
    [
      ?_assertMatch(
        {xmlattr, undefined, "name", <<>>},
        exmpp_xml:attribute("name", <<>>)
      ),
      ?_assertMatch(
        {xmlattr, "ns", "name", <<>>},
        exmpp_xml:attribute("ns", "name", <<>>)
      )
    ].

match_test_() ->
    [
      ?_assert(exmpp_xml:attribute_matches(
          exmpp_xml:attribute("name", <<>>), "name")),
      ?_assert(exmpp_xml:attribute_matches(
          exmpp_xml:attribute('name', <<>>), 'name')),
      ?_assert(exmpp_xml:attribute_matches(
          exmpp_xml:attribute("name", <<>>), 'name')),
      ?_assert(exmpp_xml:attribute_matches(
          exmpp_xml:attribute('name', <<>>), "name")),

      ?_assert(exmpp_xml:attribute_matches(
          exmpp_xml:attribute('ns', "name", <<>>), 'ns', "name")),
      ?_assert(exmpp_xml:attribute_matches(
          exmpp_xml:attribute('ns', 'name', <<>>), 'ns', 'name')),
      ?_assert(exmpp_xml:attribute_matches(
          exmpp_xml:attribute('ns', "name", <<>>), 'ns', 'name')),
      ?_assert(exmpp_xml:attribute_matches(
          exmpp_xml:attribute('ns', 'name', <<>>), 'ns', "name")),

      ?_assert(exmpp_xml:attribute_matches(
          exmpp_xml:attribute("ns", 'name', <<>>), "ns", 'name')),
      ?_assert(exmpp_xml:attribute_matches(
          exmpp_xml:attribute('ns', 'name', <<>>), 'ns', 'name')),
      ?_assert(exmpp_xml:attribute_matches(
          exmpp_xml:attribute("ns", 'name', <<>>), 'ns', 'name')),
      ?_assert(exmpp_xml:attribute_matches(
          exmpp_xml:attribute('ns', 'name', <<>>), "ns", 'name')),

      ?_assertNot(exmpp_xml:attribute_matches(
          exmpp_xml:attribute("name", <<>>), "other")),
      ?_assertNot(exmpp_xml:attribute_matches(
          exmpp_xml:attribute('name', <<>>), 'other')),
      ?_assertNot(exmpp_xml:attribute_matches(
          exmpp_xml:attribute("name", <<>>), 'other')),
      ?_assertNot(exmpp_xml:attribute_matches(
          exmpp_xml:attribute('name', <<>>), "other")),

      ?_assertNot(exmpp_xml:attribute_matches(
          exmpp_xml:attribute('ns', "name", <<>>), 'ns', "other")),
      ?_assertNot(exmpp_xml:attribute_matches(
          exmpp_xml:attribute('ns', 'name', <<>>), 'ns', 'other')),
      ?_assertNot(exmpp_xml:attribute_matches(
          exmpp_xml:attribute('ns', "name", <<>>), 'ns', 'other')),
      ?_assertNot(exmpp_xml:attribute_matches(
          exmpp_xml:attribute('ns', 'name', <<>>), 'ns', "other")),

      ?_assertNot(exmpp_xml:attribute_matches(
          exmpp_xml:attribute("ns", 'name', <<>>), "other", 'name')),
      ?_assertNot(exmpp_xml:attribute_matches(
          exmpp_xml:attribute('ns', 'name', <<>>), 'other', 'name')),
      ?_assertNot(exmpp_xml:attribute_matches(
          exmpp_xml:attribute("ns", 'name', <<>>), 'other', 'name')),
      ?_assertNot(exmpp_xml:attribute_matches(
          exmpp_xml:attribute('ns', 'name', <<>>), "other", 'name'))
    ].

get_attribute_node_from_list_test_() ->
    [
      ?_assertMatch(
        undefined,
        exmpp_xml:get_attribute_node_from_list(?ATTRS1, "xml:lang")),
      ?_assertMatch(
        undefined,
        exmpp_xml:get_attribute_node_from_list(?ATTRS2_1, "xml:lang")),
      ?_assertMatch(
        {"xml:lang", "fr"},
        exmpp_xml:get_attribute_node_from_list(?ATTRS2_2, "xml:lang")),
      ?_assertMatch(
        undefined,
        exmpp_xml:get_attribute_node_from_list(?ATTRS3_1, "lang")),
      ?_assertMatch(
        {xmlattr, ?NS_XML, "lang", <<"fr">>},
        exmpp_xml:get_attribute_node_from_list(?ATTRS3_2, "lang")),
      ?_assertMatch(
        undefined,
        exmpp_xml:get_attribute_node_from_list(?ATTRS4, "lang")),

      ?_assertMatch(
        undefined,
        exmpp_xml:get_attribute_node_from_list(?ATTRS1, ?NS_XML, "lang")),
      ?_assertMatch(
        undefined,
        exmpp_xml:get_attribute_node_from_list(?ATTRS3_1, ?NS_XML, "lang")),
      ?_assertMatch(
        {xmlattr, ?NS_XML, "lang", <<"fr">>},
        exmpp_xml:get_attribute_node_from_list(?ATTRS3_2, ?NS_XML, "lang")),
      ?_assertMatch(
        undefined,
        exmpp_xml:get_attribute_node_from_list(?ATTRS4, ?NS_XML, "lang"))
    ].

get_attribute_node_test_() ->
    [
      ?_assertMatch(
        undefined,
        exmpp_xml:get_attribute_node(undefined, "xml:lang")),

      ?_assertMatch(
        undefined,
        exmpp_xml:get_attribute_node(?ELEMENT1, "xml:lang")),
      ?_assertMatch(
        undefined,
        exmpp_xml:get_attribute_node(?ELEMENT2_1, "xml:lang")),
      ?_assertMatch(
        {"xml:lang", "fr"},
        exmpp_xml:get_attribute_node(?ELEMENT2_2, "xml:lang")),
      ?_assertMatch(
        undefined,
        exmpp_xml:get_attribute_node(?ELEMENT3_1, "lang")),
      ?_assertMatch(
        {xmlattr, ?NS_XML, "lang", <<"fr">>},
        exmpp_xml:get_attribute_node(?ELEMENT3_2, "lang")),
      ?_assertMatch(
        undefined,
        exmpp_xml:get_attribute_node(?ELEMENT4, "lang")),

      ?_assertMatch(
        undefined,
        exmpp_xml:get_attribute_node(undefined, ?NS_XML, "lang")),

      ?_assertMatch(
        undefined,
        exmpp_xml:get_attribute_node(?ELEMENT1, ?NS_XML, "lang")),
      ?_assertMatch(
        undefined,
        exmpp_xml:get_attribute_node(?ELEMENT3_1, ?NS_XML, "lang")),
      ?_assertMatch(
        {xmlattr, ?NS_XML, "lang", <<"fr">>},
        exmpp_xml:get_attribute_node(?ELEMENT3_2, ?NS_XML, "lang")),
      ?_assertMatch(
        undefined,
        exmpp_xml:get_attribute_node(?ELEMENT4, ?NS_XML, "lang"))
    ].

get_attribute_from_list_test_() ->
    [
      ?_assertMatch(
        default_value,
        exmpp_xml:get_attribute_from_list(?ATTRS1, "xml:lang",
          default_value)),
      ?_assertMatch(
        default_value,
        exmpp_xml:get_attribute_from_list(?ATTRS2_1, "xml:lang",
          default_value)),
      ?_assertMatch(
        "fr",
        exmpp_xml:get_attribute_from_list(?ATTRS2_2, "xml:lang",
          default_value)),
      ?_assertMatch(
        default_value,
        exmpp_xml:get_attribute_from_list(?ATTRS3_1, "lang",
          default_value)),
      ?_assertMatch(
        <<"fr">>,
        exmpp_xml:get_attribute_from_list(?ATTRS3_2, "lang",
          default_value)),
      ?_assertMatch(
        default_value,
        exmpp_xml:get_attribute_from_list(?ATTRS4, "lang",
          default_value)),

      ?_assertMatch(
        default_value,
        exmpp_xml:get_attribute_from_list(?ATTRS1, ?NS_XML, "lang",
          default_value)),
      ?_assertMatch(
        default_value,
        exmpp_xml:get_attribute_from_list(?ATTRS3_1, ?NS_XML, "lang",
          default_value)),
      ?_assertMatch(
        <<"fr">>,
        exmpp_xml:get_attribute_from_list(?ATTRS3_2, ?NS_XML, "lang",
          default_value)),
      ?_assertMatch(
        default_value,
        exmpp_xml:get_attribute_from_list(?ATTRS4, ?NS_XML, "lang",
          default_value))
    ].

get_attribute_test_() ->
    [
      ?_assertMatch(
        default_value,
        exmpp_xml:get_attribute(undefined, "xml:lang", default_value)),

      ?_assertMatch(
        default_value,
        exmpp_xml:get_attribute(?ELEMENT1, "xml:lang", default_value)),
      ?_assertMatch(
        default_value,
        exmpp_xml:get_attribute(?ELEMENT2_1, "xml:lang", default_value)),
      ?_assertMatch(
        "fr",
        exmpp_xml:get_attribute(?ELEMENT2_2, "xml:lang", default_value)),
      ?_assertMatch(
        default_value,
        exmpp_xml:get_attribute(?ELEMENT3_1, "lang", default_value)),
      ?_assertMatch(
        <<"fr">>,
        exmpp_xml:get_attribute(?ELEMENT3_2, "lang", default_value)),
      ?_assertMatch(
        default_value,
        exmpp_xml:get_attribute(?ELEMENT4, "lang", default_value)),

      ?_assertMatch(
        default_value,
        exmpp_xml:get_attribute(undefined, ?NS_XML, "lang", default_value)),

      ?_assertMatch(
        default_value,
        exmpp_xml:get_attribute(?ELEMENT1, ?NS_XML, "lang", default_value)),
      ?_assertMatch(
        default_value,
        exmpp_xml:get_attribute(?ELEMENT3_1, ?NS_XML, "lang", default_value)),
      ?_assertMatch(
        <<"fr">>,
        exmpp_xml:get_attribute(?ELEMENT3_2, ?NS_XML, "lang", default_value)),
      ?_assertMatch(
        default_value,
        exmpp_xml:get_attribute(?ELEMENT4, ?NS_XML, "lang", default_value))
    ].

has_attribute_in_list_test_() ->
    [
      ?_assertNot(exmpp_xml:has_attribute_in_list(?ATTRS1, "xml:lang")),
      ?_assertNot(exmpp_xml:has_attribute_in_list(?ATTRS2_1, "xml:lang")),
      ?_assert(exmpp_xml:has_attribute_in_list(?ATTRS2_2, "xml:lang")),
      ?_assertNot(exmpp_xml:has_attribute_in_list(?ATTRS3_1, "lang")),
      ?_assert(exmpp_xml:has_attribute_in_list(?ATTRS3_2, "lang")),
      ?_assertNot(exmpp_xml:has_attribute_in_list(?ATTRS4, "lang")),

      ?_assertNot(exmpp_xml:has_attribute_in_list(?ATTRS1, ?NS_XML, "lang")),
      ?_assertNot(exmpp_xml:has_attribute_in_list(?ATTRS3_1, ?NS_XML, "lang")),
      ?_assert(exmpp_xml:has_attribute_in_list(?ATTRS3_2, ?NS_XML, "lang")),
      ?_assertNot(exmpp_xml:has_attribute_in_list(?ATTRS4, ?NS_XML, "lang"))
    ].

has_attribute_test_() ->
    [
      ?_assertNot(exmpp_xml:has_attribute(undefined, "xml:lang")),
      ?_assertNot(exmpp_xml:has_attribute(?ELEMENT1, "xml:lang")),
      ?_assertNot(exmpp_xml:has_attribute(?ELEMENT2_1, "xml:lang")),
      ?_assert(exmpp_xml:has_attribute(?ELEMENT2_2, "xml:lang")),
      ?_assertNot(exmpp_xml:has_attribute(?ELEMENT3_1, "lang")),
      ?_assert(exmpp_xml:has_attribute(?ELEMENT3_2, "lang")),
      ?_assertNot(exmpp_xml:has_attribute(?ELEMENT4, "lang")),

      ?_assertNot(exmpp_xml:has_attribute(undefined, ?NS_XML, "lang")),
      ?_assertNot(exmpp_xml:has_attribute(?ELEMENT1, ?NS_XML, "lang")),
      ?_assertNot(exmpp_xml:has_attribute(?ELEMENT3_1, ?NS_XML, "lang")),
      ?_assert(exmpp_xml:has_attribute(?ELEMENT3_2, ?NS_XML, "lang")),
      ?_assertNot(exmpp_xml:has_attribute(?ELEMENT4, ?NS_XML, "lang"))
    ].

set_attribute_in_list_test_() ->
    [
      ?_assertMatch(
        [
          #xmlattr{name = "lang", value = <<"en">>}
        ],
        exmpp_xml:set_attribute_in_list(?ATTRS1, "lang", "en")),
      ?_assertMatch(
        [
          {"version", "1.0"},
          {"xml:lang","en"}
        ],
        exmpp_xml:set_attribute_in_list(?ATTRS2_1, "xml:lang", "en")),
      ?_assertMatch(
        [
          {"version", "1.0"},
          {"xml:lang","en"}
        ],
        exmpp_xml:set_attribute_in_list(?ATTRS2_2, "xml:lang", "en")),
      ?_assertMatch(
        [
          #xmlattr{name = "version", value = <<"1.0">>},
          #xmlattr{name = "lang", value = <<"en">>}
        ],
        exmpp_xml:set_attribute_in_list(?ATTRS3_1, "lang", "en")),
      ?_assertMatch(
        [
          #xmlattr{name = "version", value = <<"1.0">>},
          #xmlattr{ns = ?NS_XML, name = "lang", value = <<"en">>}
        ],
        exmpp_xml:set_attribute_in_list(?ATTRS3_2, "lang", "en")),
      ?_assertMatch(
        [
          bad_data,
          #xmlattr{name = "lang", value = <<"en">>}
        ],
        exmpp_xml:set_attribute_in_list(?ATTRS4, "lang", "en")),

      ?_assertMatch(
        [
          #xmlattr{ns = ?NS_XML, name = "lang", value = <<"en">>}
        ],
        exmpp_xml:set_attribute_in_list(?ATTRS1, ?NS_XML, "lang", "en")),
      ?_assertMatch(
        [
          #xmlattr{name = "version", value = <<"1.0">>},
          #xmlattr{ns = ?NS_XML, name = "lang", value = <<"en">>}
        ],
        exmpp_xml:set_attribute_in_list(?ATTRS3_1, ?NS_XML, "lang", "en")),
      ?_assertMatch(
        [
          #xmlattr{name = "version", value = <<"1.0">>},
          #xmlattr{ns = ?NS_XML, name = "lang", value = <<"en">>}
        ],
        exmpp_xml:set_attribute_in_list(?ATTRS3_2, ?NS_XML, "lang", "en")),
      ?_assertMatch(
        [
          #xmlattr{name = "version", value = <<"1.0">>},
          #xmlattr{ns = ?NS_XML, name = "lang", value = <<"fr">>},
          #xmlattr{ns = 'some_other_ns', name = "lang", value = <<"en">>}
        ],
        exmpp_xml:set_attribute_in_list(?ATTRS3_2,
          'some_other_ns', "lang", "en")),
      ?_assertMatch(
        [
          bad_data,
          #xmlattr{ns = ?NS_XML, name = "lang", value = <<"en">>}
        ],
        exmpp_xml:set_attribute_in_list(?ATTRS4, ?NS_XML, "lang", "en"))
    ].

set_attribute_test_() ->
    [
      ?_assertMatch(
        #xmlel{name = "element", attrs = [
            #xmlattr{name = "lang", value = <<"en">>}
          ]},
        exmpp_xml:set_attribute(?ELEMENT1, "lang", "en")),
      ?_assertMatch(
        #xmlelement{name = "element", attrs = [
            {"version", "1.0"},
            {"xml:lang","en"}
          ]},
        exmpp_xml:set_attribute(?ELEMENT2_1, "xml:lang", "en")),
      ?_assertMatch(
        #xmlelement{name = "element", attrs = [
            {"version", "1.0"},
            {"xml:lang","en"}
          ]},
        exmpp_xml:set_attribute(?ELEMENT2_2, "xml:lang", "en")),
      ?_assertMatch(
        #xmlel{name = "element", attrs = [
            #xmlattr{name = "version", value = <<"1.0">>},
            #xmlattr{name = "lang", value = <<"en">>}
          ]},
        exmpp_xml:set_attribute(?ELEMENT3_1, "lang", "en")),
      ?_assertMatch(
        #xmlel{name = "element", attrs = [
            #xmlattr{name = "version", value = <<"1.0">>},
            #xmlattr{ns = ?NS_XML, name = "lang", value = <<"en">>}
          ]},
        exmpp_xml:set_attribute(?ELEMENT3_2, "lang", "en")),
      ?_assertMatch(
        #xmlel{name = "bad_element", attrs = [
            bad_data,
            #xmlattr{name = "lang", value = <<"en">>}
          ]},
        exmpp_xml:set_attribute(?ELEMENT4, "lang", "en"))
    ].

% TODO: Port the remaining tests.
