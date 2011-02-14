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

-module(check_xml_path).

-include("exmpp.hrl").

-export([check/0, do_check/0]).

check() ->
    testsuite:run(fun do_check/0).

do_check() ->
    test_get_path_special_cases(),
    test_get_path_element(),
    test_get_path_ns_element(),
    test_get_path_attribute(),
    test_get_path_ns_attribute(),
    test_get_path_cdata(),
    test_get_path_ns_cdata(),
    ok.

% --------------------------------------------------------------------
% Path testsuite.
% --------------------------------------------------------------------

-define(ATTRIBUTE, {xmlattr, ?NS_XML, undefined, "lang", "fr"}).
-define(CDATA, {xmlcdata, <<"Content">>}).

-define(TARGET, {xmlel,
    ?NS_XML, [], "target",
    [?ATTRIBUTE],
    [?CDATA]}
).

-define(ELEMENT1, {xmlel,
    ?NS_XML, [], "element",
    [],
    []}
).

-define(ELEMENT2, {xmlel,
    ?NS_XML, [], "element",
    [?ATTRIBUTE],
    [?TARGET, ?CDATA]}
).

-define(ELEMENT3, {xmlel,
    ?NS_XML, [], "element",
    [],
    [?ELEMENT2]}
).

test_get_path_special_cases() ->
    testsuite:is(exmpp_xml:get_path(?ELEMENT1, []), ?ELEMENT1),
    try
        exmpp_xml:get_path(?ELEMENT1, [{attribute, "lang"} | bad_data]),
        testsuite:fail()
    catch
        throw:{xml, path, ending_component_not_at_the_end,
          [{attribute,"lang"} | bad_data]} ->
            ok
    end,
    try
        exmpp_xml:get_path(?ELEMENT1, [cdata | bad_data]),
        testsuite:fail()
    catch
        throw:{xml, path, ending_component_not_at_the_end,
          [cdata | bad_data]} ->
            ok
    end,
    try
        exmpp_xml:get_path(?ELEMENT1, bad_data),
        testsuite:fail()
    catch
        throw:{xml, path, invalid_component, bad_data} ->
            ok
    end,
    ok.

test_get_path_element() ->
    testsuite:is(exmpp_xml:get_path(?ELEMENT1,
        [{element, "target"}]),
      undefined),
    testsuite:is(exmpp_xml:get_path(?ELEMENT2,
        [{element, "target"}]),
      ?TARGET),
    testsuite:is(exmpp_xml:get_path(?ELEMENT3,
        [{element, "target"}]),
      undefined),
    testsuite:is(exmpp_xml:get_path(?ELEMENT3,
        [{element, "element"}, {element, "target"}]),
      ?TARGET),
    ok.

test_get_path_ns_element() ->
    testsuite:is(exmpp_xml:get_path(?ELEMENT2,
        [{element, ?NS_XML, "target"}]),
      ?TARGET),
    testsuite:is(exmpp_xml:get_path(?ELEMENT2,
        [{element, 'some_other_ns', "target"}]),
      undefined),
    testsuite:is(exmpp_xml:get_path(?ELEMENT3,
        [{element, ?NS_XML, "target"}]),
      undefined),
    testsuite:is(exmpp_xml:get_path(?ELEMENT3,
        [{element, 'some_other_ns', "target"}]),
      undefined),
    testsuite:is(exmpp_xml:get_path(?ELEMENT3,
        [{element, ?NS_XML, "element"},
          {element, ?NS_XML, "target"}]),
      ?TARGET),
    testsuite:is(exmpp_xml:get_path(?ELEMENT3,
        [{element, ?NS_XML, "element"},
          {element, 'some_other_ns', "target"}]),
      undefined),
    ok.

test_get_path_attribute() ->
    testsuite:is(exmpp_xml:get_path(?ELEMENT1,
        [{element, "target"}, {attribute, "lang"}]),
      ""),
    testsuite:is(exmpp_xml:get_path(?ELEMENT2,
        [{element, "target"}, {attribute, "lang"}]),
      "fr"),
    testsuite:is(exmpp_xml:get_path(?ELEMENT3,
        [{element, "target"}, {attribute, "lang"}]),
      ""),
    testsuite:is(exmpp_xml:get_path(?ELEMENT3,
        [{element, "element"}, {element, "target"}, {attribute, "lang"}]),
      "fr"),
    ok.

test_get_path_ns_attribute() ->
    testsuite:is(exmpp_xml:get_path(?ELEMENT2,
        [{element, ?NS_XML, "target"}, {attribute, "lang"}]),
      "fr"),
    testsuite:is(exmpp_xml:get_path(?ELEMENT2,
        [{element, "target"}, {attribute, ?NS_XML, "lang"}]),
      "fr"),
    testsuite:is(exmpp_xml:get_path(?ELEMENT2,
        [{element, 'some_other_ns', "target"}, {attribute, "lang"}]),
      ""),
    testsuite:is(exmpp_xml:get_path(?ELEMENT2,
        [{element, "target"}, {attribute, 'some_other_ns', "lang"}]),
      ""),
    testsuite:is(exmpp_xml:get_path(?ELEMENT3,
        [{element, ?NS_XML, "target"}, {attribute, "lang"}]),
      ""),
    testsuite:is(exmpp_xml:get_path(?ELEMENT3,
        [{element, "target"}, {attribute, ?NS_XML, "lang"}]),
      ""),
    testsuite:is(exmpp_xml:get_path(?ELEMENT3,
        [{element, 'some_other_ns', "target"}, {attribute, "lang"}]),
      ""),
    testsuite:is(exmpp_xml:get_path(?ELEMENT3,
        [{element, "target"}, {attribute, 'some_other_ns', "lang"}]),
      ""),
    testsuite:is(exmpp_xml:get_path(?ELEMENT3,
        [{element, ?NS_XML, "element"},
          {element, ?NS_XML, "target"}, {attribute, "lang"}]),
      "fr"),
    testsuite:is(exmpp_xml:get_path(?ELEMENT3,
        [{element, ?NS_XML, "element"},
          {element, "target"}, {attribute, ?NS_XML, "lang"}]),
      "fr"),
    testsuite:is(exmpp_xml:get_path(?ELEMENT3,
        [{element, ?NS_XML, "element"},
          {element, 'some_other_ns', "target"}, {attribute, "lang"}]),
      ""),
    testsuite:is(exmpp_xml:get_path(?ELEMENT3,
        [{element, ?NS_XML, "element"},
          {element, "target"}, {attribute, 'some_other_ns', "lang"}]),
      ""),
    ok.

test_get_path_cdata() ->
    testsuite:is(exmpp_xml:get_path(?ELEMENT1,
        [{element, "target"}, cdata]),
      <<>>),
    testsuite:is(exmpp_xml:get_path(?ELEMENT2,
        [{element, "target"}, cdata]),
      <<"Content">>),
    testsuite:is(exmpp_xml:get_path(?ELEMENT3,
        [{element, "target"}, cdata]),
      <<>>),
    testsuite:is(exmpp_xml:get_path(?ELEMENT3,
        [{element, "element"}, {element, "target"}, cdata]),
      <<"Content">>),
    ok.

test_get_path_ns_cdata() ->
    testsuite:is(exmpp_xml:get_path(?ELEMENT2,
        [{element, ?NS_XML, "target"}, cdata]),
      <<"Content">>),
    testsuite:is(exmpp_xml:get_path(?ELEMENT2,
        [{element, 'some_other_ns', "target"}, cdata]),
      <<>>),
    testsuite:is(exmpp_xml:get_path(?ELEMENT3,
        [{element, ?NS_XML, "target"}, cdata]),
      <<>>),
    testsuite:is(exmpp_xml:get_path(?ELEMENT3,
        [{element, 'some_other_ns', "target"}, cdata]),
      <<>>),
    testsuite:is(exmpp_xml:get_path(?ELEMENT3,
        [{element, ?NS_XML, "element"},
          {element, ?NS_XML, "target"}, cdata]),
      <<"Content">>),
    testsuite:is(exmpp_xml:get_path(?ELEMENT3,
        [{element, ?NS_XML, "element"},
          {element, 'some_other_ns', "target"}, cdata]),
      <<>>),
    ok.
