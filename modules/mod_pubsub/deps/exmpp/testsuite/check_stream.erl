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

-module(check_stream).

-include("exmpp.hrl").

-export([check/0, do_check/0]).

check() ->
    testsuite:run(fun do_check/0).

do_check() ->
    test_stream_opening(),
    test_stream_opening_reply(),
    test_stream_closing(),
    ok.

% --------------------------------------------------------------------
% Stream handling testsuite.
% --------------------------------------------------------------------

-define(OPENING1,
  {xmlel, ?NS_XMPP, [{?NS_XMPP, "stream"}, {?NS_JABBER_CLIENT, none}],
    'stream', [
      {xmlattr, undefined, undefined, 'to', "dest"}
    ], undefined}
).

-define(OPENING2,
  {xmlel, ?NS_XMPP, [{?NS_XMPP, "stream"}, {?NS_JABBER_CLIENT, none}],
    'stream', [
      {xmlattr, undefined, undefined, 'to', "dest"},
      {xmlattr, undefined, undefined, 'version', "1.0"}
    ], undefined}
).

-define(OPENING3,
  {xmlel, ?NS_XMPP, [{?NS_XMPP, "stream"}, {?NS_JABBER_CLIENT, none}],
    'stream', [
      {xmlattr, undefined, undefined, 'to', "dest"},
      {xmlattr, undefined, undefined, 'version', "1.0"},
      {xmlattr, ?NS_XML, undefined, 'lang', "fr"}
    ], undefined}
).

-define(OPENING_REPLY1,
  {xmlel, ?NS_XMPP, [{?NS_XMPP, "stream"}, {?NS_JABBER_CLIENT, none}],
    'stream', [
      {xmlattr, undefined, undefined, 'from', "dest"},
      {xmlattr, undefined, undefined, 'id', "foobar"}
    ], undefined}
).

-define(OPENING_REPLY2,
  {xmlel, ?NS_XMPP, [{?NS_XMPP, "stream"}, {?NS_JABBER_CLIENT, none}],
    'stream', [
      {xmlattr, undefined, undefined, 'from', "dest"},
      {xmlattr, undefined, undefined, 'version', "1.0"},
      {xmlattr, undefined, undefined, 'id', "foobar"}
    ], undefined}
).

-define(OPENING_REPLY3,
  {xmlel, ?NS_XMPP, [{?NS_XMPP, "stream"}, {?NS_JABBER_CLIENT, none}],
    'stream', [
      {xmlattr, undefined, undefined, 'from', "dest"},
      {xmlattr, undefined, undefined, 'version', "1.0"},
      {xmlattr, undefined, undefined, 'id', "foobar"},
      {xmlattr, ?NS_XML, undefined, 'lang', "fr"}
    ], undefined}
).

-define(OPENING_REPLY1a,
  {xmlel, ?NS_XMPP, [{?NS_XMPP, "stream"}, {?NS_JABBER_CLIENT, none}],
    'stream', [
      {xmlattr, undefined, undefined, 'from', "dest"},
      {xmlattr, undefined, undefined, 'id', "foobar"}
    ], undefined}
).

-define(OPENING_REPLY2a,
  {xmlel, ?NS_XMPP, [{?NS_XMPP, "stream"}, {?NS_JABBER_CLIENT, none}],
    'stream', [
      {xmlattr, undefined, undefined, 'version', "1.0"},
      {xmlattr, undefined, undefined, 'from', "dest"},
      {xmlattr, undefined, undefined, 'id', "foobar"}
    ], undefined}
).

-define(OPENING_REPLY3a,
  {xmlel, ?NS_XMPP, [{?NS_XMPP, "stream"}, {?NS_JABBER_CLIENT, none}],
    'stream', [
      {xmlattr, undefined, undefined, 'version', "1.0"},
      {xmlattr, ?NS_XML, undefined, 'lang', "fr"},
      {xmlattr, undefined, undefined, 'from', "dest"},
      {xmlattr, undefined, undefined, 'id', "foobar"}
    ], undefined}
).

-define(CLOSING1,
  {xmlendtag, ?NS_XMPP, "stream", 'stream'}
).

test_stream_opening() ->
    testsuite:is(exmpp_stream:opening("dest", ?NS_JABBER_CLIENT, undefined),
      ?OPENING1),
    testsuite:is(exmpp_stream:opening("dest", ?NS_JABBER_CLIENT, ""),
      ?OPENING1),
    testsuite:is(exmpp_stream:opening("dest", ?NS_JABBER_CLIENT, {0, 0}),
      ?OPENING1),
    testsuite:is(exmpp_stream:opening("dest", ?NS_JABBER_CLIENT, "1.0"),
      ?OPENING2),
    testsuite:is(exmpp_stream:opening("dest", ?NS_JABBER_CLIENT, {1, 0}),
      ?OPENING2),
    testsuite:is(exmpp_stream:opening("dest", ?NS_JABBER_CLIENT, "1.0", "fr"),
      ?OPENING3),
    ok.

test_stream_opening_reply() ->
    testsuite:is(exmpp_stream:opening_reply(
        "dest", ?NS_JABBER_CLIENT, undefined, "foobar"),
      ?OPENING_REPLY1),
    testsuite:is(exmpp_stream:opening_reply(
        "dest", ?NS_JABBER_CLIENT, "", "foobar"),
      ?OPENING_REPLY1),
    testsuite:is(exmpp_stream:opening_reply(
        "dest", ?NS_JABBER_CLIENT, {0, 0}, "foobar"),
      ?OPENING_REPLY1),
    testsuite:is(exmpp_stream:opening_reply(
        "dest", ?NS_JABBER_CLIENT, "1.0", "foobar"),
      ?OPENING_REPLY2),
    testsuite:is(exmpp_stream:opening_reply(
        "dest", ?NS_JABBER_CLIENT, {1, 0}, "foobar"),
      ?OPENING_REPLY2),
    testsuite:is(exmpp_stream:opening_reply(
        "dest", ?NS_JABBER_CLIENT, "1.0", "foobar", "fr"),
      ?OPENING_REPLY3),
    testsuite:is(exmpp_stream:opening_reply(?OPENING1, "foobar"),
      ?OPENING_REPLY1a),
    testsuite:is(exmpp_stream:opening_reply(?OPENING2, "foobar"),
      ?OPENING_REPLY2a),
    testsuite:is(exmpp_stream:opening_reply(?OPENING3, "foobar"),
      ?OPENING_REPLY3a),
    ok.

test_stream_closing() ->
    testsuite:is(exmpp_stream:closing(),
      ?CLOSING1),
    testsuite:is(exmpp_stream:closing(?OPENING1),
      ?CLOSING1),
    ok.
