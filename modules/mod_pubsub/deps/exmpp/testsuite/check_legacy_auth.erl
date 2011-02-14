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

-module(check_legacy_auth).

-include("exmpp.hrl").

-export([check/0, do_check/0]).

check() ->
    testsuite:run(fun do_check/0).

do_check() ->
    test_legacy_auth_request(),
    test_legacy_auth_fields(),
    test_legacy_auth_get_fields(),
    test_legacy_auth_password(),
    test_legacy_auth_success(),
    test_legacy_auth_failure(),
    ok.

% --------------------------------------------------------------------
% Legacy authentication testsuite.
% --------------------------------------------------------------------

-define(REQUEST1,
  {xmlel, ?NS_JABBER_CLIENT, [], 'iq', [
      {xmlattr, undefined, undefined, 'type', "get"},
      {xmlattr, undefined, undefined, 'id', "foobar"},
      {xmlattr, undefined, undefined, 'to', "dest"}
    ], [
      {xmlel, ?NS_LEGACY_AUTH, [], 'query',
        [], []}
    ]}
).

-define(FIELDS1,
  {xmlel, ?NS_JABBER_CLIENT, [], 'iq', [
      {xmlattr, undefined, undefined, 'type', "result"},
      {xmlattr, undefined, undefined, 'id', "foobar"}
    ], [
      {xmlel, ?NS_LEGACY_AUTH, [], 'query',
        [], [
          {xmlel, ?NS_LEGACY_AUTH, [], 'username', [], []},
          {xmlel, ?NS_LEGACY_AUTH, [], 'password', [], []},
          {xmlel, ?NS_LEGACY_AUTH, [], 'resource', [], []}
        ]}
    ]}
).

-define(FIELDS2,
  {xmlel, ?NS_JABBER_CLIENT, [], 'iq', [
      {xmlattr, undefined, undefined, 'type', "result"},
      {xmlattr, undefined, undefined, 'id', "foobar"}
    ], [
      {xmlel, ?NS_LEGACY_AUTH, [], 'query',
        [], [
          {xmlel, ?NS_LEGACY_AUTH, [], 'username', [], []},
          {xmlel, ?NS_LEGACY_AUTH, [], 'digest', [], []},
          {xmlel, ?NS_LEGACY_AUTH, [], 'resource', [], []}
        ]}
    ]}
).

-define(FIELDS3,
  {xmlel, ?NS_JABBER_CLIENT, [], 'iq', [
      {xmlattr, undefined, undefined, 'type', "result"},
      {xmlattr, undefined, undefined, 'id', "foobar"}
    ], [
      {xmlel, ?NS_LEGACY_AUTH, [], 'query',
        [], [
          {xmlel, ?NS_LEGACY_AUTH, [], 'username', [], []},
          {xmlel, ?NS_LEGACY_AUTH, [], 'password', [], []},
          {xmlel, ?NS_LEGACY_AUTH, [], 'digest', [], []},
          {xmlel, ?NS_LEGACY_AUTH, [], 'resource', [], []}
        ]}
    ]}
).

-define(PASSWORD1,
  {xmlel, ?NS_JABBER_CLIENT, [], 'iq', [
      {xmlattr, undefined, undefined, 'type', "set"},
      {xmlattr, undefined, undefined, 'id', "foobar"}
    ], [
      {xmlel, ?NS_LEGACY_AUTH, [], 'query',
        [], [
          {xmlel, ?NS_LEGACY_AUTH, [],
            'username', [], [{xmlcdata, <<"User">>}]},
          {xmlel, ?NS_LEGACY_AUTH, [],
            'password', [], [{xmlcdata, <<"Password">>}]},
          {xmlel, ?NS_LEGACY_AUTH, [],
            'resource', [], [{xmlcdata, <<"Resource">>}]}
        ]}
    ]}
).

-define(PASSWORD2,
  {xmlel, ?NS_JABBER_CLIENT, [], 'iq', [
      {xmlattr, undefined, undefined, 'type', "set"},
      {xmlattr, undefined, undefined, 'id', "foobar"}
    ], [
      {xmlel, ?NS_LEGACY_AUTH, [], 'query', [], [
          {xmlel, ?NS_LEGACY_AUTH, [],
            'username', [], [{xmlcdata, <<"User">>}]},
          {xmlel, ?NS_LEGACY_AUTH, [],
            'digest', [],
            [{xmlcdata, <<"ab8bb63d7fb73e5b06b325ec1c147945cfac5a77">>}]},
          {xmlel, ?NS_LEGACY_AUTH, [],
            'resource', [], [{xmlcdata, <<"Resource">>}]}
        ]}
    ]}
).

-define(SUCCESS1,
  {xmlel, ?NS_JABBER_CLIENT, [], 'iq', [
      {xmlattr, undefined, undefined, 'type', "result"},
      {xmlattr, undefined, undefined, 'id', "foobar"}
    ], []}
).

-define(FAILURE1,
  {xmlel, ?NS_JABBER_CLIENT, [], 'iq', [
      {xmlattr, undefined, undefined, 'id', "foobar"},
      {xmlattr, undefined, undefined, 'type', "error"}
    ], [
      {xmlel, ?NS_JABBER_CLIENT, [], 'error',
        [
          {xmlattr, undefined, undefined, 'type', "auth"},
          {xmlattr, undefined, undefined, 'code', "401"}
        ], [
          {xmlel, ?NS_STANZA_ERRORS, [], 'not-authorized', [], []}
        ]}
    ]}
).

-define(FAILURE2,
  {xmlel, ?NS_JABBER_CLIENT, [], 'iq', [
      {xmlattr, undefined, undefined, 'id', "foobar"},
      {xmlattr, undefined, undefined, 'type', "error"}
    ], [
      {xmlel, ?NS_JABBER_CLIENT, [], 'error',
        [
          {xmlattr, undefined, undefined, 'type', "cancel"},
          {xmlattr, undefined, undefined, 'code', "409"}
        ], [
          {xmlel, ?NS_STANZA_ERRORS, [], 'conflict', [], []}
        ]}
    ]}
).

-define(FAILURE3,
  {xmlel, ?NS_JABBER_CLIENT, [], 'iq', [
      {xmlattr, undefined, undefined, 'id', "foobar"},
      {xmlattr, undefined, undefined, 'type', "error"}
    ], [
      {xmlel, ?NS_JABBER_CLIENT, [], 'error',
        [
          {xmlattr, undefined, undefined, 'type', "modify"},
          {xmlattr, undefined, undefined, 'code', "406"}
        ], [
          {xmlel, ?NS_STANZA_ERRORS, [], 'not-acceptable', [], []}
        ]}
    ]}
).

test_legacy_auth_request() ->
    testsuite:is(exmpp_client_legacy_auth:request("dest", "foobar"),
      ?REQUEST1),
    ok.

test_legacy_auth_fields() ->
    testsuite:is(exmpp_server_legacy_auth:fields(?REQUEST1, plain),
      ?FIELDS1),
    testsuite:is(exmpp_server_legacy_auth:fields(?REQUEST1, digest),
      ?FIELDS2),
    testsuite:is(exmpp_server_legacy_auth:fields(?REQUEST1, both),
      ?FIELDS3),
    testsuite:is(exmpp_server_legacy_auth:fields(?REQUEST1),
      ?FIELDS3),
    ok.

test_legacy_auth_get_fields() ->
    testsuite:is(exmpp_client_legacy_auth:get_fields(?FIELDS1),
      [username, password, resource]),
    testsuite:is(exmpp_client_legacy_auth:get_fields(?FIELDS2),
      [username, digest, resource]),
    testsuite:is(exmpp_client_legacy_auth:get_fields(?FIELDS3),
      [username, password, digest, resource]),
    ok.

test_legacy_auth_password() ->
    testsuite:is(exmpp_client_legacy_auth:password_plain(
        "User", "Password", "Resource", "foobar"),
      ?PASSWORD1),
    testsuite:is(exmpp_client_legacy_auth:password_digest(
        "User", "Password", "Resource", "foobar"),
      ?PASSWORD2),
    testsuite:is(exmpp_client_legacy_auth:password(
        ?FIELDS1, "User", "Password", "Resource", "foobar"),
      ?PASSWORD1),
    testsuite:is(exmpp_client_legacy_auth:password(
        ?FIELDS2, "User", "Password", "Resource", "foobar"),
      ?PASSWORD2),
    testsuite:is(exmpp_client_legacy_auth:password(
        ?FIELDS3, "User", "Password", "Resource", "foobar"),
      ?PASSWORD2),
    testsuite:is(exmpp_server_legacy_auth:get_credentials(?PASSWORD1),
      {"User", {plain, "Password"}, "Resource"}),
    Digest = exmpp_client_legacy_auth:digest("foobar", "Password"),
    testsuite:is(exmpp_server_legacy_auth:get_credentials(?PASSWORD2),
      {"User", {digest, Digest}, "Resource"}),
    ok.

test_legacy_auth_success() ->
    testsuite:is(exmpp_server_legacy_auth:success(?PASSWORD1),
      ?SUCCESS1),
    testsuite:is(exmpp_server_legacy_auth:success(?PASSWORD2),
      ?SUCCESS1),
    testsuite:is(exmpp_client_legacy_auth:is_success(?SUCCESS1),
      true),
    ok.

test_legacy_auth_failure() ->
    testsuite:is(exmpp_server_legacy_auth:failure(?PASSWORD1, 'not-authorized'),
      ?FAILURE1),
    testsuite:is(exmpp_server_legacy_auth:failure(?PASSWORD1, 'conflict'),
      ?FAILURE2),
    testsuite:is(exmpp_server_legacy_auth:failure(?PASSWORD1, 'not-acceptable'),
      ?FAILURE3),
    testsuite:is(exmpp_client_legacy_auth:is_success(?FAILURE1),
      false),
    ok.
