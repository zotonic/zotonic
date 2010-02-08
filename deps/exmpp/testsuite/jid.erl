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

-module(jid).

-include_lib("eunit/include/eunit.hrl").

-include("exmpp.hrl").
-include("internal/exmpp_xmpp.hrl").

-define(SETUP, fun()  -> exmpp:start(), error_logger:tty(false) end).
-define(CLEANUP, fun(_) -> application:stop(exmpp) end).

-define(NODE, "n").
-define(DOMAIN, "d").
-define(RESOURCE, "r").

-define(FJ1, #jid{
    orig_jid = <<"John@example.org/Work">>,
    prep_node = <<"john">>,
    prep_domain = <<"example.org">>,
    prep_resource = <<"Work">>
  }).
-define(FJ1_S, "John@example.org/Work").
-define(FJ1_B, <<"John@example.org/Work">>).
-define(FJ1_S_BAD1, "John" ++ [0] ++ "@example.org/Work").
-define(FJ1_S_BAD2, "John@example.org" ++ [128] ++ "/Work").
-define(FJ1_S_BAD3, "John@example.org/Work" ++ [0]).

-define(FJ2, #jid{
    orig_jid = <<"example2.org/Work">>,
    prep_node = undefined,
    prep_domain = <<"example2.org">>,
    prep_resource = <<"Work">>
  }).
-define(FJ2_S, "example2.org/Work").
-define(FJ2_B, <<"example2.org/Work">>).
-define(FJ2_S_BAD1, "example2.org" ++ [128] ++ "/Work").
-define(FJ2_S_BAD2, "example2.org/Work" ++ [0]).

-define(BJ1, #jid{
    orig_jid = <<"John@example.org">>,
    prep_node = <<"john">>,
    prep_domain = <<"example.org">>,
    prep_resource = undefined
  }).
-define(BJ1_S, "John@example.org").
-define(BJ1_B, <<"John@example.org">>).
-define(BJ1_S_BAD1, "John" ++ [0] ++ "@example.org").
-define(BJ1_S_BAD2, "John@example.org" ++ [128]).

-define(BJ2, #jid{
    orig_jid = <<"example2.org">>,
    prep_node = undefined,
    prep_domain = <<"example2.org">>,
    prep_resource = undefined
  }).
-define(BJ2_S, "example2.org").
-define(BJ2_B, <<"example2.org">>).
-define(BJ2_S_BAD1, "example2.org" ++ [128]).

-define(RES, "Work").
-define(RES_BAD, "Work" ++ [0]).

too_long_identifiers_test_() ->
    Node_TL = string:chars($n, 1024),
    Domain_TL = string:chars($d, 1024),
    Resource_TL = string:chars($r, 1024),
    JID_TL = Node_TL ++ [$@] ++ Domain_TL ++ [$/] ++ Resource_TL,
    Node_TL_B = list_to_binary(Node_TL),
    Domain_TL_B = list_to_binary(Domain_TL),
    Resource_TL_B = list_to_binary(Resource_TL),
    JID_TL_B = list_to_binary(JID_TL),
    Tests = [
      ?_assertThrow(
        {jid, make, too_long, {domain, Domain_TL}},
        exmpp_jid:make(?NODE, Domain_TL, ?RESOURCE)
      ),
      ?_assertThrow(
        {jid, make, too_long, {node, Node_TL}},
        exmpp_jid:make(Node_TL, ?DOMAIN, ?RESOURCE)
      ),
      ?_assertThrow(
        {jid, make, too_long, {resource, Resource_TL}},
        exmpp_jid:make(?NODE, ?DOMAIN, Resource_TL)
      ),
      ?_assertThrow(
        {jid, parse, too_long, {jid, JID_TL}},
        exmpp_jid:parse(JID_TL)
      ),
      ?_assertThrow(
        {jid, make, too_long, {domain, Domain_TL_B}},
        exmpp_jid:make(list_to_binary(?NODE),
          Domain_TL_B, list_to_binary(?RESOURCE))
      ),
      ?_assertThrow(
        {jid, make, too_long, {node, Node_TL_B}},
        exmpp_jid:make(Node_TL_B,
          list_to_binary(?DOMAIN), list_to_binary(?RESOURCE))
      ),
      ?_assertThrow(
        {jid, make, too_long, {resource, Resource_TL_B}},
        exmpp_jid:make(list_to_binary(?NODE),
          list_to_binary(?DOMAIN), Resource_TL_B)
      ),
      ?_assertThrow(
        {jid, parse, too_long, {jid, JID_TL_B}},
        exmpp_jid:parse(JID_TL_B)
      )
    ],
    {setup, ?SETUP, ?CLEANUP, Tests}.

jid_creation_with_bad_syntax_test_() ->
    Tests = [
      ?_assertThrow(
        {jid, parse, unexpected_end_of_string, {jid, ""}},
        exmpp_jid:parse("")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator, {jid, "@"}},
        exmpp_jid:parse("@")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator, {jid, "@Domain"}},
        exmpp_jid:parse("@Domain")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator, {jid, "@Domain@Domain"}},
        exmpp_jid:parse("@Domain@Domain")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator, {jid, "@Domain/Resource"}},
        exmpp_jid:parse("@Domain/Resource")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_end_of_string, {jid, "Node@"}},
        exmpp_jid:parse("Node@")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator, {jid, "Node@Domain@"}},
        exmpp_jid:parse("Node@Domain@")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator, {jid, "Node@@Domain"}},
        exmpp_jid:parse("Node@@Domain")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_end_of_string, {jid, "Domain/"}},
        exmpp_jid:parse("Domain/")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_end_of_string, {jid, "Node@Domain/"}},
        exmpp_jid:parse("Node@Domain/")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator, {jid, "@/"}},
        exmpp_jid:parse("@/")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_resource_separator, {jid, "Node@/"}},
        exmpp_jid:parse("Node@/")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_resource_separator, {jid, "Node@/Resource"}},
        exmpp_jid:parse("Node@/Resource")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_resource_separator, {jid, "/"}},
        exmpp_jid:parse("/")
      ),
      ?_assertThrow(
        {jid, parse, unexpected_resource_separator, {jid, "/Resource"}},
        exmpp_jid:parse("/Resource")
      )
    ],
    TestsBinaryParsing =  [
      ?_assertThrow(
        {jid, parse, unexpected_end_of_string, {jid, <<>>}},
        exmpp_jid:parse(<<>>)
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator, {jid, <<"@">>}},
        exmpp_jid:parse(<<"@">>)
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator, {jid, <<"@Domain">>}},
        exmpp_jid:parse(<<"@Domain">>)
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator, {jid, <<"@Domain@Domain">>}},
        exmpp_jid:parse(<<"@Domain@Domain">>)
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator, {jid, <<"@Domain/Resource">>}},
        exmpp_jid:parse(<<"@Domain/Resource">>)
      ),
      ?_assertThrow(
        {jid, parse, unexpected_end_of_string, {jid, <<"Node@">>}},
        exmpp_jid:parse(<<"Node@">>)
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator, {jid, <<"Node@Domain@">>}},
        exmpp_jid:parse(<<"Node@Domain@">>)
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator, {jid, <<"Node@@Domain">>}},
        exmpp_jid:parse(<<"Node@@Domain">>)
      ),
      ?_assertThrow(
        {jid, parse, unexpected_end_of_string, {jid, <<"Domain/">>}},
        exmpp_jid:parse(<<"Domain/">>)
      ),
      ?_assertThrow(
        {jid, parse, unexpected_end_of_string, {jid, <<"Node@Domain/">>}},
        exmpp_jid:parse(<<"Node@Domain/">>)
      ),
      ?_assertThrow(
        {jid, parse, unexpected_node_separator, {jid, <<"@/">>}},
        exmpp_jid:parse(<<"@/">>)
      ),
      ?_assertThrow(
        {jid, parse, unexpected_resource_separator, {jid, <<"Node@/">>}},
        exmpp_jid:parse(<<"Node@/">>)
      ),
      ?_assertThrow(
        {jid, parse, unexpected_resource_separator,
          {jid, <<"Node@/Resource">>}},
        exmpp_jid:parse(<<"Node@/Resource">>)
      ),
      ?_assertThrow(
        {jid, parse, unexpected_resource_separator, {jid, <<"/">>}},
        exmpp_jid:parse(<<"/">>)
      ),
      ?_assertThrow(
        {jid, parse, unexpected_resource_separator, {jid, <<"/Resource">>}},
        exmpp_jid:parse(<<"/Resource">>)
      )
    ],
    {setup, ?SETUP, ?CLEANUP, Tests ++ TestsBinaryParsing}.

jid_creation_with_bad_chars_test_() ->
    Tests = [
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse(?FJ1_S_BAD1)
      ),
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse(?FJ1_S_BAD2)
      ),
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse(?FJ1_S_BAD3)
      ),
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse(?FJ2_S_BAD1)
      ),
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse(?FJ2_S_BAD2)
      ),
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse(?BJ1_S_BAD1)
      ),
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse(?BJ1_S_BAD2)
      ),
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse(?BJ2_S_BAD1)
      )
    ],
    TestsBinaryParsing = [
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse(list_to_binary(?FJ1_S_BAD1))
      ),
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse(list_to_binary(?FJ1_S_BAD2))
      ),
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse(list_to_binary(?FJ1_S_BAD3))
      ),
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse(list_to_binary(?FJ2_S_BAD1))
      ),
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse(list_to_binary(?FJ2_S_BAD2))
      ),
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse(list_to_binary(?BJ1_S_BAD1))
      ),
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse(list_to_binary(?BJ1_S_BAD2))
      ),
      ?_assertThrow(
        {jid, make, invalid, _},
        exmpp_jid:parse(list_to_binary(?BJ2_S_BAD1))
      )
    ],
    {setup, ?SETUP, ?CLEANUP, Tests ++ TestsBinaryParsing}.

good_jid_creation_test_() ->
    Tests = [
      ?_assertMatch(?FJ1, exmpp_jid:parse(?FJ1_S)),
      ?_assertMatch(?FJ2, exmpp_jid:parse(?FJ2_S)),
      ?_assertMatch(?BJ1, exmpp_jid:parse(?BJ1_S)),
      ?_assertMatch(?BJ2, exmpp_jid:parse(?BJ2_S))
    ],
    {setup, ?SETUP, ?CLEANUP, Tests}.

jid_stringification_test_() ->
    [
      ?_assertMatch(?FJ1_S, exmpp_jid:to_list(?FJ1)),
      ?_assertMatch(?FJ2_S, exmpp_jid:to_list(?FJ2)),
      ?_assertMatch(?BJ1_S, exmpp_jid:to_list(?BJ1)),
      ?_assertMatch(?BJ2_S, exmpp_jid:to_list(?BJ2)),
      ?_assertMatch(?FJ1_B, exmpp_jid:to_binary(?FJ1)),
      ?_assertMatch(?FJ2_B, exmpp_jid:to_binary(?FJ2)),
      ?_assertMatch(?BJ1_B, exmpp_jid:to_binary(?BJ1)),
      ?_assertMatch(?BJ2_B, exmpp_jid:to_binary(?BJ2))
    ].

jid_arg_stringification_test_() ->
    [
      ?_assertMatch("d", exmpp_jid:to_list(undefined, "d")),
      ?_assertMatch("n@d", exmpp_jid:to_list("n", "d")),
      ?_assertMatch("n@d/r", exmpp_jid:to_list("n", "d", "r")),
      ?_assertMatch(<<"d">>, exmpp_jid:to_binary(undefined, "d")),
      ?_assertMatch(<<"n@d">>, exmpp_jid:to_binary("n", "d")),
      ?_assertMatch(<<"n@d/r">>, exmpp_jid:to_binary("n", "d", "r")),
      ?_assertMatch(<<"d">>, exmpp_jid:to_binary(undefined, <<"d">>)),
      ?_assertMatch(<<"n@d">>, exmpp_jid:to_binary(<<"n">>, <<"d">>)),
      ?_assertMatch(<<"n@d/r">>, exmpp_jid:to_binary(<<"n">>, <<"d">>,
          <<"r">>))
    ].

bare_jid_stringification_test_() ->
    [
      ?_assertMatch(?BJ1_S, exmpp_jid:bare_to_list(?FJ1)),
      ?_assertMatch(?BJ2_S, exmpp_jid:bare_to_list(?FJ2)),
      ?_assertMatch(?BJ1_S, exmpp_jid:bare_to_list(?BJ1)),
      ?_assertMatch(?BJ2_S, exmpp_jid:bare_to_list(?BJ2)),
      ?_assertMatch(?BJ1_B, exmpp_jid:bare_to_binary(?FJ1)),
      ?_assertMatch(?BJ2_B, exmpp_jid:bare_to_binary(?FJ2)),
      ?_assertMatch(?BJ1_B, exmpp_jid:bare_to_binary(?BJ1)),
      ?_assertMatch(?BJ2_B, exmpp_jid:bare_to_binary(?BJ2))
    ].

jid_conversion_test_() ->
    [
      ?_assertMatch(?BJ1, exmpp_jid:bare(?FJ1)),
      ?_assertMatch(?BJ1, exmpp_jid:bare(?BJ1)),
      ?_assertMatch(?BJ2, exmpp_jid:bare(?FJ2)),
      ?_assertMatch(?BJ2, exmpp_jid:bare(?BJ2))
    ].

bare_jid_conversion_test_() ->
    Tests = [
      ?_assertMatch(?FJ1, exmpp_jid:full(?FJ1, ?RES)),
      ?_assertMatch(?FJ1, exmpp_jid:full(?BJ1, ?RES)),
      ?_assertMatch(?FJ2, exmpp_jid:full(?FJ2, ?RES)),
      ?_assertMatch(?FJ2, exmpp_jid:full(?BJ2, ?RES)),
      ?_assertMatch(?FJ1, exmpp_jid:full(?FJ1, undefined)),
      ?_assertMatch(?BJ1, exmpp_jid:full(?BJ1, undefined))
    ],
    {setup, ?SETUP, ?CLEANUP, Tests}.

bare_jid_conversion_with_bad_resource_test_() ->
    Resource_TL = string:chars($r, 1024),
    Tests = [
      ?_assertThrow(
        {jid, convert, invalid, _},
        exmpp_jid:full(?FJ1, ?RES_BAD)
      ),
      ?_assertThrow(
        {jid, convert, too_long, _},
        exmpp_jid:full(?FJ1, Resource_TL)
      ),
      ?_assertThrow(
        {jid, convert, invalid, _},
        exmpp_jid:full(?BJ1, ?RES_BAD)
      ),
      ?_assertThrow(
        {jid, convert, too_long, _},
        exmpp_jid:full(?BJ1, Resource_TL)
      ),
      ?_assertThrow(
        {jid, convert, invalid, _},
        exmpp_jid:full(?FJ2, ?RES_BAD)
      ),
      ?_assertThrow(
        {jid, convert, too_long, _},
        exmpp_jid:full(?FJ2, Resource_TL)
      ),
      ?_assertThrow(
        {jid, convert, invalid, _},
        exmpp_jid:full(?BJ2, ?RES_BAD)
      ),
      ?_assertThrow(
        {jid, convert, too_long, _},
        exmpp_jid:full(?BJ2, Resource_TL)
      )
    ],
    {setup, ?SETUP, ?CLEANUP, Tests}.

accessors_test_() ->
    Tests = [
      ?_assertMatch(?BJ2_S, exmpp_jid:domain_as_list(?FJ2)),
      ?_assertMatch(?BJ2_S, exmpp_jid:prep_domain_as_list(?FJ2)),
      ?_assertMatch(undefined, exmpp_jid:prep_node_as_list(?FJ2)),
      ?_assertMatch(undefined, exmpp_jid:resource_as_list(?BJ1))
    ],
    {setup, ?SETUP, ?CLEANUP, Tests}.

jid_comparison_test_() ->
    [
      ?_assert(exmpp_jid:compare(?FJ1, ?FJ1)),
      ?_assertNot(exmpp_jid:compare(?FJ1, ?BJ1)),
      ?_assert(exmpp_jid:compare(?FJ2, ?FJ2)),
      ?_assertNot(exmpp_jid:compare(?FJ2, ?BJ2)),
      ?_assertNot(exmpp_jid:compare(?FJ1, ?FJ2)),
      ?_assertNot(exmpp_jid:compare(?BJ1, ?BJ2))
    ].

bare_jid_comparison_test_() ->
    [
      ?_assert(exmpp_jid:bare_compare(?FJ1, ?FJ1)),
      ?_assert(exmpp_jid:bare_compare(?FJ1, ?BJ1)),
      ?_assert(exmpp_jid:bare_compare(?FJ2, ?FJ2)),
      ?_assert(exmpp_jid:bare_compare(?FJ2, ?BJ2)),
      ?_assertNot(exmpp_jid:bare_compare(?FJ1, ?FJ2)),
      ?_assertNot(exmpp_jid:bare_compare(?BJ1, ?BJ2))
    ].

domain_comparison_test_() ->
    [
      ?_assert(exmpp_jid:compare_domains(?FJ1, ?FJ1)),
      ?_assert(exmpp_jid:compare_domains(?FJ1, ?BJ1)),
      ?_assert(exmpp_jid:compare_domains(?FJ2, ?FJ2)),
      ?_assert(exmpp_jid:compare_domains(?FJ2, ?BJ2)),
      ?_assertNot(exmpp_jid:compare_domains(?FJ1, ?FJ2)),
      ?_assertNot(exmpp_jid:compare_domains(?FJ1, ?BJ2))
    ].
