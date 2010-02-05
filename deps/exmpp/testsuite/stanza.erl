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

-module(stanza).

-include_lib("eunit/include/eunit.hrl").

-include("exmpp.hrl").

% --------------------------------------------------------------------
% Data.
% --------------------------------------------------------------------

-define(MESSAGE_1, #xmlel{
    ns = ?NS_JABBER_CLIENT,
    name = 'message'
  }).
-define(MESSAGE_2, #xmlel{
    ns = ?NS_JABBER_CLIENT,
    name = 'message',
    attrs = [
      #xmlattr{name = 'from', value = <<"sender">>},
      #xmlattr{name = 'to', value = <<"recipient">>},
      #xmlattr{name = 'type', value = <<"chat">>},
      #xmlattr{name = 'id', value = <<"message-2">>},
      #xmlattr{ns = ?NS_XML, name = 'lang', value = <<"en">>}
    ]
  }).

% --------------------------------------------------------------------
% Testsuite.
% --------------------------------------------------------------------

sender_test_() ->
    [
      ?_assertMatch(
        undefined,
        exmpp_stanza:get_sender(?MESSAGE_1)),
      ?_assertMatch(
        <<"sender">>,
        exmpp_stanza:get_sender(?MESSAGE_2)),
      ?_assertMatch(
        <<"new_sender">>,
        exmpp_stanza:get_sender(
          exmpp_stanza:set_sender(?MESSAGE_1, "new_sender"))),
      ?_assertMatch(
        <<"new_sender">>,
        exmpp_stanza:get_sender(
          exmpp_stanza:set_sender(?MESSAGE_2, "new_sender"))),
      ?_assertMatch(
        undefined,
        exmpp_stanza:get_sender(
          exmpp_stanza:remove_sender(?MESSAGE_1))),
      ?_assertMatch(
        undefined,
        exmpp_stanza:get_sender(
          exmpp_stanza:remove_sender(?MESSAGE_2)))
    ].

recipient_test_() ->
    [
      ?_assertMatch(
        undefined,
        exmpp_stanza:get_recipient(?MESSAGE_1)),
      ?_assertMatch(
        <<"recipient">>,
        exmpp_stanza:get_recipient(?MESSAGE_2)),
      ?_assertMatch(
        <<"new_recipient">>,
        exmpp_stanza:get_recipient(
          exmpp_stanza:set_recipient(?MESSAGE_1, "new_recipient"))),
      ?_assertMatch(
        <<"new_recipient">>,
        exmpp_stanza:get_recipient(
          exmpp_stanza:set_recipient(?MESSAGE_2, "new_recipient"))),
      ?_assertMatch(
        undefined,
        exmpp_stanza:get_recipient(
          exmpp_stanza:remove_recipient(?MESSAGE_1))),
      ?_assertMatch(
        undefined,
        exmpp_stanza:get_recipient(
          exmpp_stanza:remove_recipient(?MESSAGE_2)))
    ].

jids_test_() ->
    [
      ?_assertMatch(
        <<"new_sender">>,
        exmpp_stanza:get_sender(exmpp_stanza:set_jids(?MESSAGE_1,
            "new_sender", "new_recipient"))),
      ?_assertMatch(
        <<"new_recipient">>,
        exmpp_stanza:get_recipient(exmpp_stanza:set_jids(?MESSAGE_1,
            "new_sender", "new_recipient")))
    ].

id_test_() ->
    [
      ?_assertMatch(
        undefined,
        exmpp_stanza:get_id(?MESSAGE_1)),
      ?_assertMatch(
        <<"message-2">>,
        exmpp_stanza:get_id(?MESSAGE_2)),
      ?_assertMatch(
        <<"new_message-1">>,
        exmpp_stanza:get_id(exmpp_stanza:set_id(?MESSAGE_1, "new_message-1"))),
      ?_assertMatch(
        <<"new_message-2">>,
        exmpp_stanza:get_id(exmpp_stanza:set_id(?MESSAGE_2, "new_message-2")))
    ].

type_test_() ->
    [
      ?_assertMatch(
        undefined,
        exmpp_stanza:get_type(?MESSAGE_1)),
      ?_assertMatch(
        <<"chat">>,
        exmpp_stanza:get_type(?MESSAGE_2)),
      ?_assertMatch(
        <<"headline">>,
        exmpp_stanza:get_type(exmpp_stanza:set_type(?MESSAGE_1, "headline"))),
      ?_assertMatch(
        <<"headline">>,
        exmpp_stanza:get_type(exmpp_stanza:set_type(?MESSAGE_2, "headline")))
    ].

lang_test_() ->
    [
      ?_assertMatch(
        undefined,
        exmpp_stanza:get_lang(?MESSAGE_1)),
      ?_assertMatch(
        <<"en">>,
        exmpp_stanza:get_lang(?MESSAGE_2)),
      ?_assertMatch(
        <<"fr">>,
        exmpp_stanza:get_lang(exmpp_stanza:set_lang(?MESSAGE_1, "fr"))),
      ?_assertMatch(
        <<"fr">>,
        exmpp_stanza:get_lang(exmpp_stanza:set_lang(?MESSAGE_2, "fr")))
    ].
