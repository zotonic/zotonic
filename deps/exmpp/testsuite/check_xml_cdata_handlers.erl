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

-module(check_xml_cdata_handlers).

-include("exmpp.hrl").

-export([check/0, do_check/0]).

check() ->
    testsuite:run(fun do_check/0).

do_check() ->
    test_get_cdata_from_list(),
    test_get_cdata(),
    test_normalize_cdata_in_list(),
    test_normalize_cdata(),
    test_set_cdata_in_list(),
    test_set_cdata(),
    test_remove_cdata_from_list(),
    test_remove_cdata(),
    ok.

% --------------------------------------------------------------------
% CDATA handlers testsuite.
% --------------------------------------------------------------------

-define(CONTENT, <<"Content">>).
-define(CONTENT_S, "Content").
-define(CDATA, {xmlcdata, <<"Content">>}).
-define(CDATA_DOUBLE, {xmlcdata, <<"ContentContent">>}).

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
    [?CDATA]}
).

-define(ELEMENT3, {xmlelement, "element",
    [],
    [?ELEMENT2]}
).

-define(ELEMENT3_2, {xmlelement, "element",
    [],
    [?ELEMENT2, ?CDATA]}
).

-define(ELEMENT4, {xmlelement, "element",
    [],
    [?CDATA, ?CDATA]}
).

-define(ELEMENT4_NORM, {xmlelement, "element",
    [],
    [?CDATA_DOUBLE]}
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
    [?CDATA]}
).

-define(ELEMENT3_NS, {xmlel,
    ?NS_XML, [], "element",
    [],
    [?ELEMENT2_NS]}
).

-define(ELEMENT3_2_NS, {xmlel,
    ?NS_XML, [], "element",
    [],
    [?ELEMENT2_NS, ?CDATA]}
).

-define(ELEMENT4_NS, {xmlel,
    ?NS_XML, [], "element",
    [],
    [?CDATA, ?CDATA]}
).

-define(ELEMENT4_NS_NORM, {xmlel,
    ?NS_XML, [], "element",
    [],
    [?CDATA_DOUBLE]}
).

test_get_cdata_from_list() ->
    testsuite:is(exmpp_xml:get_cdata_from_list([]),
      <<>>),
    testsuite:is(exmpp_xml:get_cdata_from_list([?CDATA]),
      <<"Content">>),
    testsuite:is(exmpp_xml:get_cdata_from_list([?ELEMENT0]),
      <<>>),
    testsuite:is(exmpp_xml:get_cdata_from_list([?ELEMENT1]),
      <<>>),
    testsuite:is(exmpp_xml:get_cdata_from_list([?ELEMENT0, ?CDATA]),
      <<"Content">>),
    testsuite:is(exmpp_xml:get_cdata_from_list([?ELEMENT1, ?CDATA]),
      <<"Content">>),
    ok.

test_get_cdata() ->
    testsuite:is(exmpp_xml:get_cdata(undefined),
      <<>>),
    testsuite:is(exmpp_xml:get_cdata(?ELEMENT0),
      <<>>),
    testsuite:is(exmpp_xml:get_cdata(?ELEMENT1),
      <<>>),
    testsuite:is(exmpp_xml:get_cdata(?ELEMENT2),
      <<"Content">>),
    testsuite:is(exmpp_xml:get_cdata(?ELEMENT3),
      <<>>),
    testsuite:is(exmpp_xml:get_cdata(?ELEMENT1_NS),
      <<>>),
    testsuite:is(exmpp_xml:get_cdata(?ELEMENT2_NS),
      <<"Content">>),
    testsuite:is(exmpp_xml:get_cdata(?ELEMENT3_NS),
      <<>>),
    ok.

test_normalize_cdata_in_list() ->
    testsuite:is(exmpp_xml:normalize_cdata_in_list(undefined),
      undefined),
    testsuite:is(exmpp_xml:normalize_cdata_in_list([]),
      []),
    testsuite:is(exmpp_xml:normalize_cdata_in_list([?CDATA]),
      [?CDATA]),
    testsuite:is(exmpp_xml:normalize_cdata_in_list([?ELEMENT1]),
      [?ELEMENT1]),
    testsuite:is(exmpp_xml:normalize_cdata_in_list([?ELEMENT1, ?CDATA]),
      [?ELEMENT1, ?CDATA]),
    testsuite:is(exmpp_xml:normalize_cdata_in_list([?CDATA, ?CDATA]),
      [?CDATA_DOUBLE]),
    ok.

test_normalize_cdata() ->
    testsuite:is(exmpp_xml:normalize_cdata(?ELEMENT0),
      ?ELEMENT0),
    testsuite:is(exmpp_xml:normalize_cdata(?ELEMENT1),
      ?ELEMENT1),
    testsuite:is(exmpp_xml:normalize_cdata(?ELEMENT2),
      ?ELEMENT2),
    testsuite:is(exmpp_xml:normalize_cdata(?ELEMENT3),
      ?ELEMENT3),
    testsuite:is(exmpp_xml:normalize_cdata(?ELEMENT4),
      ?ELEMENT4_NORM),
    testsuite:is(exmpp_xml:normalize_cdata(?ELEMENT0_NS),
      ?ELEMENT0_NS),
    testsuite:is(exmpp_xml:normalize_cdata(?ELEMENT1_NS),
      ?ELEMENT1_NS),
    testsuite:is(exmpp_xml:normalize_cdata(?ELEMENT2_NS),
      ?ELEMENT2_NS),
    testsuite:is(exmpp_xml:normalize_cdata(?ELEMENT3_NS),
      ?ELEMENT3_NS),
    testsuite:is(exmpp_xml:normalize_cdata(?ELEMENT4_NS),
      ?ELEMENT4_NS_NORM),
    ok.

test_set_cdata_in_list() ->
    testsuite:is(exmpp_xml:set_cdata_in_list(undefined, ?CONTENT),
      [?CDATA]),
    testsuite:is(exmpp_xml:set_cdata_in_list(undefined, ?CONTENT_S),
      [?CDATA]),
    testsuite:is(exmpp_xml:set_cdata_in_list([], ?CONTENT),
      [?CDATA]),
    testsuite:is(exmpp_xml:set_cdata_in_list([?CDATA], ?CONTENT),
      [?CDATA]),
    testsuite:is(exmpp_xml:set_cdata_in_list([?CDATA], ?CONTENT_S),
      [?CDATA]),
    testsuite:is(exmpp_xml:set_cdata_in_list([?ELEMENT0], ?CONTENT),
      [?ELEMENT0, ?CDATA]),
    ok.

test_set_cdata() ->
    testsuite:is(exmpp_xml:set_cdata(?ELEMENT0, ?CONTENT),
      ?ELEMENT2),
    testsuite:is(exmpp_xml:set_cdata(?ELEMENT1, ?CONTENT),
      ?ELEMENT2),
    testsuite:is(exmpp_xml:set_cdata(?ELEMENT2, ?CONTENT),
      ?ELEMENT2),
    testsuite:is(exmpp_xml:set_cdata(?ELEMENT3, ?CONTENT),
      ?ELEMENT3_2),
    testsuite:is(exmpp_xml:set_cdata(?ELEMENT4, ?CONTENT),
      ?ELEMENT2),
    testsuite:is(exmpp_xml:set_cdata(?ELEMENT0_NS, ?CONTENT),
      ?ELEMENT2_NS),
    testsuite:is(exmpp_xml:set_cdata(?ELEMENT1_NS, ?CONTENT),
      ?ELEMENT2_NS),
    testsuite:is(exmpp_xml:set_cdata(?ELEMENT2_NS, ?CONTENT),
      ?ELEMENT2_NS),
    testsuite:is(exmpp_xml:set_cdata(?ELEMENT3_NS, ?CONTENT),
      ?ELEMENT3_2_NS),
    testsuite:is(exmpp_xml:set_cdata(?ELEMENT4_NS, ?CONTENT),
      ?ELEMENT2_NS),
    ok.

test_remove_cdata_from_list() ->
    testsuite:is(exmpp_xml:remove_cdata_from_list([]),
      []),
    testsuite:is(exmpp_xml:remove_cdata_from_list([?CDATA]),
      []),
    testsuite:is(exmpp_xml:remove_cdata_from_list([?ELEMENT0]),
      [?ELEMENT0]),
    testsuite:is(exmpp_xml:remove_cdata_from_list([?ELEMENT1]),
      [?ELEMENT1]),
    testsuite:is(exmpp_xml:remove_cdata_from_list([?ELEMENT1, ?CDATA]),
      [?ELEMENT1]),
    ok.

test_remove_cdata() ->
    testsuite:is(exmpp_xml:remove_cdata(?ELEMENT0),
      ?ELEMENT0),
    testsuite:is(exmpp_xml:remove_cdata(?ELEMENT1),
      ?ELEMENT1),
    testsuite:is(exmpp_xml:remove_cdata(?ELEMENT2),
      ?ELEMENT1),
    testsuite:is(exmpp_xml:remove_cdata(?ELEMENT3),
      ?ELEMENT3),
    testsuite:is(exmpp_xml:remove_cdata(?ELEMENT0_NS),
      ?ELEMENT0_NS),
    testsuite:is(exmpp_xml:remove_cdata(?ELEMENT1_NS),
      ?ELEMENT1_NS),
    testsuite:is(exmpp_xml:remove_cdata(?ELEMENT2_NS),
      ?ELEMENT1_NS),
    testsuite:is(exmpp_xml:remove_cdata(?ELEMENT3_NS),
      ?ELEMENT3_NS),
    ok.
