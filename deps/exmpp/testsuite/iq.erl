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

-module(iq).

-include_lib("eunit/include/eunit.hrl").

-include("exmpp.hrl").

% --------------------------------------------------------------------
% Data.
% --------------------------------------------------------------------

-define(PAYLOAD, #xmlel{ns = ?NS_ROSTER, name = 'query'}).
-define(ERROR, #xmlel{ns = ?NS_JABBER_CLIENT, name = 'error', children = [
      #xmlel{ns = ?NS_STANZA_ERRORS, name = 'bad-request'}
    ]}).

-define(IQ_1, #xmlel{
    ns = ?NS_JABBER_CLIENT,
    name = 'iq'
  }).
-define(IQ_2, #xmlel{
    ns = ?NS_JABBER_CLIENT,
    name = 'iq',
    attrs = [
      #xmlattr{name = 'from', value = <<"sender">>},
      #xmlattr{name = 'to', value = <<"recipient">>},
      #xmlattr{name = 'type', value = <<"get">>},
      #xmlattr{name = 'id', value = <<"iq-2">>},
      #xmlattr{ns = ?NS_XML, name = 'lang', value = <<"en">>}
    ],
    children = [
      #xmlcdata{cdata = <<"\n  ">>},
      ?PAYLOAD,
      #xmlcdata{cdata = <<"\n">>}
    ]
  }).
-define(IQ_3, #xmlel{
    ns = ?NS_JABBER_CLIENT,
    name = 'iq',
    attrs = [
      #xmlattr{name = 'from', value = <<"recipient">>},
      #xmlattr{name = 'to', value = <<"sender">>},
      #xmlattr{name = 'type', value = <<"result">>},
      #xmlattr{name = 'id', value = <<"iq-2">>},
      #xmlattr{ns = ?NS_XML, name = 'lang', value = <<"en">>}
    ]
  }).
-define(IQ_4, #xmlel{
    ns = ?NS_JABBER_CLIENT,
    name = 'iq',
    attrs = [
      #xmlattr{name = 'from', value = <<"recipient">>},
      #xmlattr{name = 'to', value = <<"sender">>},
      #xmlattr{name = 'type', value = <<"result">>},
      #xmlattr{name = 'id', value = <<"iq-2">>},
      #xmlattr{ns = ?NS_XML, name = 'lang', value = <<"en">>}
    ],
    children = [?PAYLOAD]
  }).
-define(IQ_5, #xmlel{
    ns = ?NS_JABBER_CLIENT,
    name = 'iq',
    attrs = [
      #xmlattr{name = 'from', value = <<"recipient">>},
      #xmlattr{name = 'to', value = <<"sender">>},
      #xmlattr{name = 'type', value = <<"error">>},
      #xmlattr{name = 'id', value = <<"iq-2">>},
      #xmlattr{ns = ?NS_XML, name = 'lang', value = <<"en">>}
    ],
    children = [
      ?ERROR
    ]
  }).
-define(IQ_6, #xmlel{
    ns = ?NS_JABBER_CLIENT,
    name = 'iq',
    attrs = [
      #xmlattr{name = 'from', value = <<"recipient">>},
      #xmlattr{name = 'to', value = <<"sender">>},
      #xmlattr{name = 'type', value = <<"error">>},
      #xmlattr{name = 'id', value = <<"iq-2">>},
      #xmlattr{ns = ?NS_XML, name = 'lang', value = <<"en">>}
    ],
    children = [
      #xmlcdata{cdata = <<"\n  ">>},
      ?PAYLOAD,
      #xmlcdata{cdata = <<"\n">>},
      ?ERROR
    ]
  }).

-define(IQ_REC_2, #iq{
    kind = request,
    type = 'get',
    id = <<"iq-2">>,
    ns = ?NS_ROSTER,
    payload = ?PAYLOAD,
    error = undefined,
    lang = <<"en">>,
    iq_ns = ?NS_JABBER_CLIENT
  }).
-define(IQ_REC_3, #iq{
    kind = response,
    type = 'result',
    id = <<"iq-2">>,
    lang = <<"en">>,
    iq_ns = ?NS_JABBER_CLIENT
  }).
-define(IQ_REC_4, #iq{
    kind = response,
    type = 'result',
    id = <<"iq-2">>,
    lang = <<"en">>,
    iq_ns = ?NS_JABBER_CLIENT,
    ns = ?NS_ROSTER,
    payload = ?PAYLOAD
  }).
-define(IQ_REC_5, #iq{
    kind = response,
    type = 'error',
    id = <<"iq-2">>,
    lang = <<"en">>,
    iq_ns = ?NS_JABBER_CLIENT,
    error = ?ERROR
  }).
-define(IQ_REC_6, #iq{
    kind = response,
    type = 'error',
    id = <<"iq-2">>,
    lang = <<"en">>,
    iq_ns = ?NS_JABBER_CLIENT,
    ns = ?NS_ROSTER,
    payload = ?PAYLOAD,
    error = ?ERROR
  }).

% --------------------------------------------------------------------
% Testsuite.
% --------------------------------------------------------------------

is_iq_test_() ->
    [
      ?_assert(exmpp_iq:is_iq(?IQ_1)),
      ?_assert(exmpp_iq:is_iq(?IQ_2)),
      ?_assertNot(exmpp_iq:is_iq(?IQ_REC_2)),
      ?_assertNot(exmpp_iq:is_iq(?PAYLOAD))
    ].

is_iq_record_test_() ->
    [
      ?_assertNot(exmpp_iq:is_iq_record(?IQ_1)),
      ?_assertNot(exmpp_iq:is_iq_record(?IQ_2)),
      ?_assert(exmpp_iq:is_iq_record(?IQ_REC_2)),
      ?_assertNot(exmpp_iq:is_iq_record(?PAYLOAD))
    ].

iq_conversion_test_() ->
    IQ_2 = exmpp_xml:remove_whitespaces(?IQ_2),
    IQ_3 = exmpp_xml:remove_whitespaces(?IQ_3),
    IQ_4 = exmpp_xml:remove_whitespaces(?IQ_4),
    IQ_5 = exmpp_xml:remove_whitespaces(?IQ_5),
    IQ_6 = exmpp_xml:remove_whitespaces(?IQ_6),
    [
      ?_assertMatch(
        ?IQ_REC_2,
        exmpp_iq:xmlel_to_iq(?IQ_2)),
      ?_assertMatch(
        IQ_2,
        exmpp_iq:iq_to_xmlel(?IQ_REC_2, "sender", "recipient")),

      ?_assertMatch(
        ?IQ_REC_3,
        exmpp_iq:xmlel_to_iq(?IQ_3)),
      ?_assertMatch(
        IQ_3,
        exmpp_iq:iq_to_xmlel(?IQ_REC_3, "recipient", "sender")),
      ?_assertMatch(
        ?IQ_REC_4,
        exmpp_iq:xmlel_to_iq(?IQ_4)),
      ?_assertMatch(
        IQ_4,
        exmpp_iq:iq_to_xmlel(?IQ_REC_4, "recipient", "sender")),

      ?_assertMatch(
        ?IQ_REC_5,
        exmpp_iq:xmlel_to_iq(?IQ_5)),
      ?_assertMatch(
        IQ_5,
        exmpp_iq:iq_to_xmlel(?IQ_REC_5, "recipient", "sender")),
      ?_assertMatch(
        ?IQ_REC_6,
        exmpp_iq:xmlel_to_iq(?IQ_6)),
      ?_assertMatch(
        IQ_6,
        exmpp_iq:iq_to_xmlel(?IQ_REC_6, "recipient", "sender"))
    ].

type_test_() ->
    [
      ?_assertMatch(
        'get',
        exmpp_iq:get_type(?IQ_2)),
      ?_assertMatch(
        'get',
        exmpp_iq:get_type(?IQ_REC_2)),
      ?_assertMatch(
        'set',
        exmpp_iq:get_type(exmpp_stanza:set_type(?IQ_2, 'set'))),
      ?_assertMatch(
        'set',
        exmpp_iq:get_type(exmpp_stanza:set_type(?IQ_REC_2, 'set')))
    ].

kind_test_() ->
    [
      ?_assertMatch(
        request,
        exmpp_iq:get_kind(?IQ_2)),
      ?_assertMatch(
        request,
        exmpp_iq:get_kind(?IQ_REC_2)),
      ?_assertMatch(
        response,
        exmpp_iq:get_kind(exmpp_stanza:set_type(?IQ_2, 'result'))),
      ?_assertMatch(
        response,
        exmpp_iq:get_kind(exmpp_stanza:set_type(?IQ_REC_2, 'result')))
    ].

request_test_() ->
    [
      ?_assertThrow(
        {iq, get_request, invalid_iq, ?IQ_1},
        exmpp_iq:get_request(?IQ_1)),

      ?_assertMatch(
        ?PAYLOAD,
        exmpp_iq:get_request(?IQ_2)),
      ?_assertMatch(
        ?PAYLOAD,
        exmpp_iq:get_request(?IQ_REC_2)),

      ?_assertThrow(
        {iq, get_request, unexpected_iq, ?IQ_3},
        exmpp_iq:get_request(?IQ_3)),
      ?_assertThrow(
        {iq, get_request, unexpected_iq, ?IQ_REC_3},
        exmpp_iq:get_request(?IQ_REC_3)),
      ?_assertThrow(
        {iq, get_request, unexpected_iq, ?IQ_4},
        exmpp_iq:get_request(?IQ_4)),
      ?_assertThrow(
        {iq, get_request, unexpected_iq, ?IQ_REC_4},
        exmpp_iq:get_request(?IQ_REC_4)),

      ?_assertMatch(
        undefined,
        exmpp_iq:get_request(?IQ_5)),
      ?_assertMatch(
        undefined,
        exmpp_iq:get_request(?IQ_REC_5)),
      ?_assertMatch(
        ?PAYLOAD,
        exmpp_iq:get_request(?IQ_6)),
      ?_assertMatch(
        ?PAYLOAD,
        exmpp_iq:get_request(?IQ_REC_6))
    ].

result_test_() ->
    [
      ?_assertThrow(
        {iq, get_result, invalid_iq, ?IQ_1},
        exmpp_iq:get_result(?IQ_1)),

      ?_assertThrow(
        {iq, get_result, unexpected_iq, ?IQ_2},
        exmpp_iq:get_result(?IQ_2)),
      ?_assertThrow(
        {iq, get_result, unexpected_iq, ?IQ_REC_2},
        exmpp_iq:get_result(?IQ_REC_2)),

      ?_assertMatch(
        undefined,
        exmpp_iq:get_result(?IQ_3)),
      ?_assertMatch(
        undefined,
        exmpp_iq:get_result(?IQ_REC_3)),
      ?_assertMatch(
        ?PAYLOAD,
        exmpp_iq:get_result(?IQ_4)),
      ?_assertMatch(
        ?PAYLOAD,
        exmpp_iq:get_result(?IQ_REC_4)),

      ?_assertThrow(
        {iq, get_result, unexpected_iq, ?IQ_5},
        exmpp_iq:get_result(?IQ_5)),
      ?_assertThrow(
        {iq, get_result, unexpected_iq, ?IQ_REC_5},
        exmpp_iq:get_result(?IQ_REC_5)),
      ?_assertThrow(
        {iq, get_result, unexpected_iq, ?IQ_6},
        exmpp_iq:get_result(?IQ_6)),
      ?_assertThrow(
        {iq, get_result, unexpected_iq, ?IQ_REC_6},
        exmpp_iq:get_result(?IQ_REC_6))
    ].

payload_test_() ->
    [
      ?_assertThrow(
        {iq, get_payload, invalid_iq, ?IQ_1},
        exmpp_iq:get_payload(?IQ_1)),

      ?_assertMatch(
        ?PAYLOAD,
        exmpp_iq:get_payload(?IQ_2)),
      ?_assertMatch(
        ?PAYLOAD,
        exmpp_iq:get_payload(?IQ_REC_2)),

      ?_assertMatch(
        undefined,
        exmpp_iq:get_payload(?IQ_3)),
      ?_assertMatch(
        undefined,
        exmpp_iq:get_payload(?IQ_REC_3)),
      ?_assertMatch(
        ?PAYLOAD,
        exmpp_iq:get_payload(?IQ_4)),
      ?_assertMatch(
        ?PAYLOAD,
        exmpp_iq:get_payload(?IQ_REC_4)),

      ?_assertMatch(
        ?ERROR,
        exmpp_iq:get_payload(?IQ_5)),
      ?_assertMatch(
        ?ERROR,
        exmpp_iq:get_payload(?IQ_REC_5)),
      ?_assertMatch(
        ?ERROR,
        exmpp_iq:get_payload(?IQ_6)),
      ?_assertMatch(
        ?ERROR,
        exmpp_iq:get_payload(?IQ_REC_6))
    ].

response_test_() ->
    [
      ?_assertMatch(
        ?IQ_3,
        exmpp_iq:result(?IQ_2)),
      ?_assertMatch(
        ?IQ_REC_3,
        exmpp_iq:result(?IQ_REC_2)),
      ?_assertMatch(
        ?IQ_4,
        exmpp_iq:result(?IQ_2, ?PAYLOAD)),
      ?_assertMatch(
        ?IQ_REC_4,
        exmpp_iq:result(?IQ_REC_2, ?PAYLOAD)),

      ?_assertMatch(
        ?IQ_5,
        exmpp_iq:error_without_original(?IQ_2, ?ERROR)),
      ?_assertMatch(
        ?IQ_REC_5,
        exmpp_iq:error_without_original(?IQ_REC_2, ?ERROR)),
      ?_assertMatch(
        ?IQ_6,
        exmpp_iq:error(?IQ_2, ?ERROR)),
      ?_assertMatch(
        ?IQ_REC_6,
        exmpp_iq:error(?IQ_REC_2, ?ERROR))
    ].
