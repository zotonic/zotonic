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

-module(check_xml_element_handlers).

-include("exmpp.hrl").

-export([check/0, do_check/0]).

check() ->
    testsuite:run(fun do_check/0).

do_check() ->
    test_get_element_by_name2(),
    test_get_element_by_name3(),
    test_get_elements_by_name2(),
    test_get_elements_by_name3(),
    test_get_element_by_ns(),
    test_get_child_elements(),
    test_append_child(),
    test_append_children(),
    test_prepend_child(),
    test_prepend_children(),
    test_replace_child(),
    test_set_children(),
    test_get_lname(),
    ok.

% --------------------------------------------------------------------
% Element handlers testsuite.
% --------------------------------------------------------------------

-define(TARGET, {xmlelement, "target",
    [],
    []}
).

-define(ELEMENT0, {xmlelement, "element",
    [],
    undefined}
).

-define(ELEMENT1, {xmlelement, "element",
    [],
    []}
).

-define(ELEMENT2, {xmlelement, "element",
    [],
    [?TARGET]}
).

-define(ELEMENT3, {xmlelement, "element",
    [],
    [?ELEMENT2]}
).

-define(ELEMENT4, {xmlelement, "element",
    [],
    [?ELEMENT1,?ELEMENT2]}
).

-define(TARGET_NS, {xmlel,
    ?NS_XML, [], "target",
    [],
    []}
).

-define(ELEMENT0_NS, {xmlel,
    ?NS_XML, [], "element",
    [],
    undefined}
).

-define(ELEMENT1_NS, {xmlel,
    ?NS_XML, [], "element",
    [],
    []}
).

-define(ELEMENT2_NS, {xmlel,
    ?NS_XML, [], "element",
    [],
    [?TARGET_NS]}
).

-define(ELEMENT3_NS, {xmlel,
    ?NS_XML, [], "element",
    [],
    [?ELEMENT2_NS]}
).

-define(ELEMENT4_NS, {xmlel,
    ?NS_XML, [], "element",
    [],
    [?ELEMENT1_NS,?ELEMENT2_NS]}
).

-define(CDATA, {xmlcdata, "some text"}).

-define(CHILD, {xmlelement, "child",
    [],
    []}
).

-define(OTHER_CHILD, {xmlelement, "other",
    [],
    []}
).

-define(ELEMENT5, {xmlelement, "element",
    [],
    [
      ?CHILD,
      ?CDATA,
      ?CHILD,
      ?CHILD,
      ?OTHER_CHILD
    ]}
).

-define(CHILD1_NS, {xmlel,
    ?NS_XML, [], "child",
    [],
    []}
).

-define(CHILD2_NS, {xmlel,
    'some_other_ns', [], "child",
    [],
    []}
).

-define(OTHER_CHILD_NS, {xmlel,
    ?NS_XML, [], "other",
    [],
    []}
).

-define(ELEMENT5_NS, {xmlel,
    ?NS_XML, [], "element",
    [],
    [
      ?CHILD1_NS,
      ?CDATA,
      ?CHILD2_NS,
      ?CHILD1_NS,
      ?OTHER_CHILD_NS
    ]}
).

test_get_element_by_name2() ->
    testsuite:is(exmpp_xml:get_element_by_name(undefined,
        "target"), undefined),
    testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT0,
        "target"), undefined),
    testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT1,
        "target"), undefined),
    testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT2,
        "target"), ?TARGET),
    testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT3,
        "target"), undefined),
    testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT1_NS,
        "target"), undefined),
    testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT2_NS,
        "target"), ?TARGET_NS),
    testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT3_NS,
        "target"), undefined),
    ok.

test_get_element_by_name3() ->
    testsuite:is(exmpp_xml:get_element_by_name(undefined,
        ?NS_XML, "target"), undefined),
    testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT0_NS,
        ?NS_XML, "target"), undefined),
    testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT1_NS,
        ?NS_XML, "target"), undefined),
    testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT2_NS,
        ?NS_XML, "target"), ?TARGET_NS),
    testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT3_NS,
        ?NS_XML, "target"), undefined),
    testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT1_NS,
        'some_other_ns', "target"), undefined),
    testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT2_NS,
        'some_other_ns', "target"), undefined),
    testsuite:is(exmpp_xml:get_element_by_name(?ELEMENT3_NS,
        'some_other_ns', "target"), undefined),
    ok.

test_get_elements_by_name2() ->
    testsuite:is(exmpp_xml:get_elements_by_name(undefined,
        "child"), []),
    testsuite:is(exmpp_xml:get_elements_by_name(?ELEMENT0,
        "child"), []),
    testsuite:is(exmpp_xml:get_elements_by_name(?ELEMENT1,
        "child"), []),
    testsuite:is(exmpp_xml:get_elements_by_name(?ELEMENT2,
        "child"), []),
    testsuite:is(exmpp_xml:get_elements_by_name(?ELEMENT5,
        "target"), []),
    testsuite:is(exmpp_xml:get_elements_by_name(?ELEMENT5,
        "child"), [?CHILD, ?CHILD, ?CHILD]),
    testsuite:is(exmpp_xml:get_elements_by_name(?ELEMENT5,
        "other"), [?OTHER_CHILD]),
    testsuite:is(exmpp_xml:get_elements_by_name(?ELEMENT0_NS,
        "child"), []),
    testsuite:is(exmpp_xml:get_elements_by_name(?ELEMENT1_NS,
        "child"), []),
    testsuite:is(exmpp_xml:get_elements_by_name(?ELEMENT2_NS,
        "child"), []),
    testsuite:is(exmpp_xml:get_elements_by_name(?ELEMENT5_NS,
        "target"), []),
    testsuite:is(exmpp_xml:get_elements_by_name(?ELEMENT5_NS,
        "child"), [?CHILD1_NS, ?CHILD2_NS, ?CHILD1_NS]),
    testsuite:is(exmpp_xml:get_elements_by_name(?ELEMENT5_NS,
        "other"), [?OTHER_CHILD_NS]),
    ok.

test_get_elements_by_name3() ->
    testsuite:is(exmpp_xml:get_elements_by_name(undefined,
        ?NS_XML, "child"), []),
    testsuite:is(exmpp_xml:get_elements_by_name(?ELEMENT0_NS,
        ?NS_XML, "child"), []),
    testsuite:is(exmpp_xml:get_elements_by_name(?ELEMENT1_NS,
        ?NS_XML, "child"), []),
    testsuite:is(exmpp_xml:get_elements_by_name(?ELEMENT2_NS,
        ?NS_XML, "child"), []),
    testsuite:is(exmpp_xml:get_elements_by_name(?ELEMENT5_NS,
        ?NS_XML, "target"), []),
    testsuite:is(exmpp_xml:get_elements_by_name(?ELEMENT5_NS,
        ?NS_XML, "child"), [?CHILD1_NS, ?CHILD1_NS]),
    testsuite:is(exmpp_xml:get_elements_by_name(?ELEMENT5_NS,
        ?NS_XML, "other"), [?OTHER_CHILD_NS]),
    testsuite:is(exmpp_xml:get_elements_by_name(?ELEMENT5_NS,
        'some_other_ns', "child"), [?CHILD2_NS]),
    testsuite:is(exmpp_xml:get_elements_by_name(?ELEMENT5_NS,
        'some_other_ns', "other"), []),
    ok.

test_get_element_by_ns() ->
    testsuite:is(exmpp_xml:get_element_by_ns(undefined,
        ?NS_XML), undefined),
    testsuite:is(exmpp_xml:get_element_by_ns(?ELEMENT0_NS,
        ?NS_XML), undefined),
    testsuite:is(exmpp_xml:get_element_by_ns(?ELEMENT1_NS,
        ?NS_XML), undefined),
    testsuite:is(exmpp_xml:get_element_by_ns(?ELEMENT2_NS,
        ?NS_XML), ?TARGET_NS),
    testsuite:is(exmpp_xml:get_element_by_ns(?ELEMENT2_NS,
        'some_other_ns'), undefined),
    ok.

test_get_child_elements() ->
    testsuite:is(exmpp_xml:get_child_elements(undefined),
      []),
    testsuite:is(exmpp_xml:get_child_elements(?ELEMENT0),
      []),
    testsuite:is(exmpp_xml:get_child_elements(?ELEMENT1),
      []),
    testsuite:is(exmpp_xml:get_child_elements(?ELEMENT2),
      [?TARGET]),
    testsuite:is(exmpp_xml:get_child_elements(?ELEMENT5),
      [?CHILD, ?CHILD, ?CHILD, ?OTHER_CHILD]),
    testsuite:is(exmpp_xml:get_child_elements(?ELEMENT0_NS),
      []),
    testsuite:is(exmpp_xml:get_child_elements(?ELEMENT1_NS),
      []),
    testsuite:is(exmpp_xml:get_child_elements(?ELEMENT2_NS),
      [?TARGET_NS]),
    testsuite:is(exmpp_xml:get_child_elements(?ELEMENT5_NS),
      [?CHILD1_NS, ?CHILD2_NS, ?CHILD1_NS, ?OTHER_CHILD_NS]),
    ok.

test_append_child() ->
    testsuite:is(exmpp_xml:append_child(?ELEMENT0, ?TARGET),
      ?ELEMENT2),
    testsuite:is(exmpp_xml:append_child(?ELEMENT1, ?TARGET),
      ?ELEMENT2),
    testsuite:is(exmpp_xml:append_child(?ELEMENT0_NS, ?TARGET_NS),
      ?ELEMENT2_NS),
    testsuite:is(exmpp_xml:append_child(?ELEMENT1_NS, ?TARGET_NS),
      ?ELEMENT2_NS),
    ok.

test_append_children() ->
    testsuite:is(exmpp_xml:append_children(?ELEMENT0, [?TARGET]),
      ?ELEMENT2),
    testsuite:is(exmpp_xml:append_children(?ELEMENT1, [?TARGET]),
      ?ELEMENT2),
    testsuite:is(exmpp_xml:append_children(?ELEMENT0_NS, [?TARGET_NS]),
      ?ELEMENT2_NS),
    testsuite:is(exmpp_xml:append_children(?ELEMENT1_NS, [?TARGET_NS]),
      ?ELEMENT2_NS),
    ok.

test_prepend_child() ->
    testsuite:is(exmpp_xml:prepend_child(?ELEMENT0, ?TARGET),
      ?ELEMENT2),
    testsuite:is(exmpp_xml:prepend_child(?ELEMENT3, ?ELEMENT1),
      ?ELEMENT4),
    testsuite:is(exmpp_xml:prepend_child(?ELEMENT0_NS, ?TARGET_NS),
      ?ELEMENT2_NS),
    testsuite:is(exmpp_xml:prepend_child(?ELEMENT3_NS, ?ELEMENT1_NS),
      ?ELEMENT4_NS),
    ok.

test_prepend_children() ->
    testsuite:is(exmpp_xml:prepend_children(?ELEMENT0, [?TARGET]),
      ?ELEMENT2),
    testsuite:is(exmpp_xml:prepend_children(?ELEMENT3, [?ELEMENT1]),
      ?ELEMENT4),
    testsuite:is(exmpp_xml:prepend_children(?ELEMENT0_NS, [?TARGET_NS]),
      ?ELEMENT2_NS),
    testsuite:is(exmpp_xml:prepend_children(?ELEMENT3_NS, [?ELEMENT1_NS]),
      ?ELEMENT4_NS),
    ok.

test_replace_child() ->
    testsuite:is(exmpp_xml:replace_child(?ELEMENT0, ?TARGET, ?ELEMENT2),
      ?ELEMENT0),
    testsuite:is(exmpp_xml:replace_child(?ELEMENT1, ?TARGET, ?ELEMENT2),
      ?ELEMENT1),
    testsuite:is(exmpp_xml:replace_child(?ELEMENT2, ?TARGET, ?ELEMENT2),
      ?ELEMENT3),
    testsuite:is(exmpp_xml:replace_child(?ELEMENT3, ?TARGET, ?ELEMENT2),
      ?ELEMENT3),
    testsuite:is(exmpp_xml:replace_child(?ELEMENT0_NS,
        ?TARGET_NS, ?ELEMENT2_NS),
      ?ELEMENT0_NS),
    testsuite:is(exmpp_xml:replace_child(?ELEMENT1_NS,
        ?TARGET_NS, ?ELEMENT2_NS),
      ?ELEMENT1_NS),
    testsuite:is(exmpp_xml:replace_child(?ELEMENT2_NS,
        ?TARGET_NS, ?ELEMENT2_NS),
      ?ELEMENT3_NS),
    testsuite:is(exmpp_xml:replace_child(?ELEMENT3_NS,
        ?TARGET_NS, ?ELEMENT2_NS),
      ?ELEMENT3_NS),
    ok.

test_set_children() ->
    testsuite:is(exmpp_xml:set_children(?ELEMENT0, [?TARGET]),
      ?ELEMENT2),
    testsuite:is(exmpp_xml:set_children(?ELEMENT1, [?TARGET]),
      ?ELEMENT2),
    testsuite:is(exmpp_xml:set_children(?ELEMENT2, [?TARGET]),
      ?ELEMENT2),
    testsuite:is(exmpp_xml:set_children(?ELEMENT0_NS, [?TARGET_NS]),
      ?ELEMENT2_NS),
    testsuite:is(exmpp_xml:set_children(?ELEMENT1_NS, [?TARGET_NS]),
      ?ELEMENT2_NS),
    testsuite:is(exmpp_xml:set_children(?ELEMENT2_NS, [?TARGET_NS]),
      ?ELEMENT2_NS),
    ok.

test_get_lname() ->
    testsuite:is(exmpp_xml:get_lname_as_list(?TARGET), "element"),
    testsuite:is(exmpp_xml:get_lname_as_list(?TARGET_NS), "element"),
    ok.
