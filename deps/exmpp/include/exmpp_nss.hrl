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

% --------------------------------------------------------------------
% Namespace and prefix macros.
% --------------------------------------------------------------------

% Defined by XML.
-define(NS_XML,                      'http://www.w3.org/XML/1998/namespace').
-define(NS_XML_s,                    "http://www.w3.org/XML/1998/namespace").
-define(NS_XML_pfx,                  "xml").

% Defined by XMPP Core (RFC 3920).
-define(NS_XMPP,                     'http://etherx.jabber.org/streams').
-define(NS_XMPP_s,                   "http://etherx.jabber.org/streams").
-define(NS_XMPP_pfx,                 "stream").
-define(NS_STREAM_ERRORS,            'urn:ietf:params:xml:ns:xmpp-streams').
-define(NS_TLS,                      'urn:ietf:params:xml:ns:xmpp-tls').
-define(NS_SASL,                     'urn:ietf:params:xml:ns:xmpp-sasl').
-define(NS_BIND,                     'urn:ietf:params:xml:ns:xmpp-bind').
-define(NS_STANZA_ERRORS,            'urn:ietf:params:xml:ns:xmpp-stanzas').

% Defined by XMPP-IM (RFC 3921).
-define(NS_JABBER_CLIENT,            'jabber:client').
-define(NS_JABBER_CLIENT_s,          "jabber:client").
-define(NS_JABBER_SERVER,            'jabber:server').
-define(NS_SESSION,                  'urn:ietf:params:xml:ns:xmpp-session').
-define(NS_ROSTER,                   'jabber:iq:roster').

% Defined by End-to-End Signing and Object Encryption for XMPP (RFC 3923).
-define(NS_E2E,                      'urn:ietf:params:xml:ns:xmpp-e2e').

% Defined by XEP-0003: Proxy Accept Socket Service (PASS).
-define(NS_PASS,                     'jabber:iq:pass').
-define(NS_PASS_s,                   "jabber:iq:pass").

% Defined by XEP-0004: Data Forms.
-define(NS_DATA_FORMS,               'jabber:x:data').
-define(NS_DATA_FORMS_s,             "jabber:x:data").

% Defined by XEP-0009: Jabber-RPC.
-define(NS_RPC,                      'jabber:iq:rpc').
-define(NS_RPC_s,                    "jabber:iq:rpc").

% Defined by XEP-0011: Jabber Browsing.
-define(NS_BROWSE,                   'jabber:iq:browse').
-define(NS_BROWSE_s,                 "jabber:iq:browse").

% Defined by XEP-0012: Last Activity.
-define(NS_LAST_ACTIVITY,            'jabber:iq:last').
-define(NS_LAST_ACTIVITY_s,          "jabber:iq:last").

% Defined by XEP-0013: Flexible Offline Message Retrieval.
-define(NS_OFFLINE,                  'http://jabber.org/protocol/offline').
-define(NS_OFFLINE_s,                "http://jabber.org/protocol/offline").

% Defined by XEP-0016: Privacy Lists.
-define(NS_PRIVACY,                  'jabber:iq:privacy').
-define(NS_PRIVACY_s,                "jabber:iq:privacy").

% Defined by XEP-0020: Feature Negotiation.
-define(NS_FEATURE_NEG,              'http://jabber.org/protocol/feature-neg').
-define(NS_FEATURE_NEG_s,            "http://jabber.org/protocol/feature-neg").

% Defined by XEP-0022: Message Events.
-define(NS_MESSAGE_EVENT,            'jabber:x:event').
-define(NS_MESSAGE_EVENT_s,          "jabber:x:event").

% Defined by XEP-0023: Message Expiration.
-define(NS_MESSAGE_EXPIRE,           'jabber:x:expire').
-define(NS_MESSAGE_EXPIRE_s,         "jabber:x:expire").

% Defined by XEP-0027: Current Jabber OpenPGP Usage.
-define(NS_PGP_ENCRYPTED,            'jabber:x:encrypted').
-define(NS_PGP_SIGNED,               'jabber:x:signed').
-define(NS_PGP_ENCRYPTED_s,          "jabber:x:encrypted").
-define(NS_PGP_SIGNED_s,             "jabber:x:signed").

% Defined by XEP-0030: Service Discovery.
-define(NS_DISCO_INFO,               'http://jabber.org/protocol/disco#info').
-define(NS_DISCO_ITEMS,              'http://jabber.org/protocol/disco#items').
-define(NS_DISCO_INFO_s,             "http://jabber.org/protocol/disco#info").
-define(NS_DISCO_ITEMS_s,            "http://jabber.org/protocol/disco#items").

% Defined by XEP-0033: Extended Stanza Addressing.
-define(NS_ADDRESS,                  'http://jabber.org/protocol/address').
-define(NS_ADDRESS_s,                "http://jabber.org/protocol/address").

% Defined by XEP-0039: Statistics Gathering.
-define(NS_STATS,                    'http://jabber.org/protocol/stats').
-define(NS_STATS_s,                  "http://jabber.org/protocol/stats").

% Defined by XEP-0045: Multi-User Chat.
-define(NS_MUC,                      'http://jabber.org/protocol/muc').
-define(NS_MUC_ADMIN,                'http://jabber.org/protocol/muc#admin').
-define(NS_MUC_OWNER,                'http://jabber.org/protocol/muc#owner').
-define(NS_MUC_UNIQUE,               'http://jabber.org/protocol/muc#unique').
-define(NS_MUC_USER,                 'http://jabber.org/protocol/muc#user').
-define(NS_MUC_s,                    "http://jabber.org/protocol/muc").
-define(NS_MUC_ADMIN_s,              "http://jabber.org/protocol/muc#admin").
-define(NS_MUC_OWNER_s,              "http://jabber.org/protocol/muc#owner").
-define(NS_MUC_UNIQUE_s,             "http://jabber.org/protocol/muc#unique").
-define(NS_MUC_USER_s,               "http://jabber.org/protocol/muc#user").

% Defined by XEP-0047: In-Band Bytestreams.
-define(NS_IBB,                      'http://jabber.org/protocol/ibb').

% Defined by XEP-0048: Bookmarks.
-define(NS_BOOKMARKS,                'storage:bookmarks').
-define(NS_BOOKMARKS_s,              "storage:bookmarks").

% Defined by XEP-0049: Private XML Storage.
-define(NS_PRIVATE,                  'jabber:iq:private').
-define(NS_PRIVATE_s,                "jabber:iq:private").

% Defined by XEP-0050: Ad-Hoc Commands.
-define(NS_ADHOC,                    'http://jabber.org/protocol/commands').
-define(NS_ADHOC_s,                  "http://jabber.org/protocol/commands").
-define(NS_ADHOC_b,                  <<"http://jabber.org/protocol/commands">>).

% Defined by XEP-0054: vcard-temp.
-define(NS_VCARD,                    'vcard-temp').
-define(NS_VCARD_s,                  "vcard-temp").

% Defined by XEP-0055: Jabber Search.
-define(NS_SEARCH,                   'jabber:iq:search').
-define(NS_SEARCH_s,                 "jabber:iq:search").

% Defined by XEP-0059: Result Set Management.
-define(NS_RSM,                      'http://jabber.org/protocol/rsm').
-define(NS_RSM_s,                    "http://jabber.org/protocol/rsm").

% Defined by XEP-0060: Publish-Subscribe.
-define(NS_PUBSUB,
  'http://jabber.org/protocol/pubsub').
-define(NS_PUBSUB_ERRORS,
  'http://jabber.org/protocol/pubsub#errors').
-define(NS_PUBSUB_EVENT,
  'http://jabber.org/protocol/pubsub#event').
-define(NS_PUBSUB_OWNER,
  'http://jabber.org/protocol/pubsub#owner').
-define(NS_PUBSUB_SUBSCRIBE_AUTH,
  'http://jabber.org/protocol/pubsub#subscribe_authorization').
-define(NS_PUBSUB_SUBSCRIBE_OPTIONS,
  'http://jabber.org/protocol/pubsub#subscribe_options').
-define(NS_PUBSUB_NODE_CONFIG,
  'http://jabber.org/protocol/pubsub#node_config').
-define(NS_PUBSUB_META_DATA,
  'http://jabber.org/protocol/pubsub#meta-data').
-define(NS_PUBSUB_PUBLISH_OPTIONS,
  'http://jabber.org/protocol/pubsub#publish-options').
-define(NS_PUBSUB_s,
  "http://jabber.org/protocol/pubsub").
-define(NS_PUBSUB_ERRORS_s,
  "http://jabber.org/protocol/pubsub#errors").
-define(NS_PUBSUB_EVENT_s,
  "http://jabber.org/protocol/pubsub#event").
-define(NS_PUBSUB_OWNER_s,
  "http://jabber.org/protocol/pubsub#owner").
-define(NS_PUBSUB_SUBSCRIBE_AUTH_s,
  "http://jabber.org/protocol/pubsub#subscribe_authorization").
-define(NS_PUBSUB_SUBSCRIBE_OPTIONS_s,
  "http://jabber.org/protocol/pubsub#subscribe_options").
-define(NS_PUBSUB_NODE_CONFIG_s,
  "http://jabber.org/protocol/pubsub#node_config").
-define(NS_PUBSUB_META_DATA_s,
  "http://jabber.org/protocol/pubsub#meta-data").
-define(NS_PUBSUB_PUBLISH_OPTIONS_s,
  "http://jabber.org/protocol/pubsub#publish-options").

% Defined by XEP-0065: SOCKS5 Bytestreams.
-define(NS_BYTESTREAMS,              'http://jabber.org/protocol/bytestreams'.
-define(NS_BYTESTREAMS_s,            "http://jabber.org/protocol/bytestreams".

% Defined by XEP-0066: Out of Band Data.
-define(NS_OOBD_IQ,                  'jabber:iq:oob').
-define(NS_OOBD_X,                   'jabber:x:oob').
-define(NS_OOBD_IQ_s,                "jabber:iq:oob").
-define(NS_OOBD_X_s,                 "jabber:x:oob").

% Defined by XEP-0070: Verifying HTTP Requests via XMPP.
-define(NS_HTTP_AUTH,                'http://jabber.org/protocol/http-auth').
-define(NS_HTTP_AUTH_s,              "http://jabber.org/protocol/http-auth").

% Defined by XEP-0071: XHTML-IM.
-define(NS_XHTML_IM,                 'http://jabber.org/protocol/xhtml-im').
-define(NS_XHTML_IM_s,               "http://jabber.org/protocol/xhtml-im").

% Defined by XEP-0072: SOAP Over XMPP.
-define(NS_SOAP_FAULT,               'http://jabber.org/protocol/soap#fault').
-define(NS_SOAP_FAULT_s,             "http://jabber.org/protocol/soap#fault").

% Defined by XEP-0077: In-Band Registration.
-define(NS_INBAND_REGISTER,          'jabber:iq:register').
-define(NS_INBAND_REGISTER_FEAT,     'http://jabber.org/features/iq-register').
-define(NS_INBAND_REGISTER_s,        "jabber:iq:register").
-define(NS_INBAND_REGISTER_FEAT_s,   "http://jabber.org/features/iq-register").

% Defined by XEP-0078: Non-SASL Authentication.
-define(NS_LEGACY_AUTH,              'jabber:iq:auth').
-define(NS_LEGACY_AUTH_FEAT,         'http://jabber.org/features/iq-aut').
-define(NS_LEGACY_AUTH_s,            "jabber:iq:auth").
-define(NS_LEGACY_AUTH_FEAT_s,       "http://jabber.org/features/iq-aut").

% Defined by XEP-0079: Advanced Message Processing.
-define(NS_AMP,                      'http://jabber.org/protocol/amp').
-define(NS_AMP_ERRORS,               'http://jabber.org/protocol/amp#error').
-define(NS_AMP_FEAT,                 'http://jabber.org/features/amp').
-define(NS_AMP_s,                    "http://jabber.org/protocol/amp").
-define(NS_AMP_ERRORS_s,             "http://jabber.org/protocol/amp#error").
-define(NS_AMP_FEAT_s,               "http://jabber.org/features/amp").

% Defined by XEP-0080: User Location.
-define(NS_GEOLOC,                   'http://jabber.org/protocol/geoloc').
-define(NS_GEOLOC_s,                 "http://jabber.org/protocol/geoloc").

% Defined by XEP-0083: Nested Roster Groups.
-define(NS_ROSTER_DELIMITER,         'roster:delimiter').
-define(NS_ROSTER_DELIMITER_s,       "roster:delimiter").

% Defined by XEP-0084: User Avatar.
-define(NS_USER_AVATAR,              'urn:xmpp:avatar:metadata').
-define(NS_USER_AVATAR_s,            "urn:xmpp:avatar:metadata").

% Defined by XEP-0085: Chat State Notifications
-define(NS_CHATSTATES,               'http://jabber.org/protocol/chatstates').
-define(NS_CHATSTATES_s,             "http://jabber.org/protocol/chatstates").

% Defined by XEP-0090: Entity Time.
-define(NS_TIME_OLD,                 'jabber:iq:time').
-define(NS_TIME_OLD_s,               "jabber:iq:time").

% Defined by XEP-0091: Delayed Delivery.
-define(NS_DELAY_OLD,                'jabber:x:delay').
-define(NS_DELAY_OLD_s,              "jabber:x:delay").

% Defined by XEP-0092: Software Version.
-define(NS_SOFT_VERSION,             'jabber:iq:version').
-define(NS_SOFT_VERSION_s,           "jabber:iq:version").

% Defined by XEP-0093: Roster Item Exchange.
-define(NS_ROSTER_EXCHANGE_OLD,      'jabber:x:roster').
-define(NS_ROSTER_EXCHANGE_OLD_s,    "jabber:x:roster").

% Defined by XEP-0095: Stream Initiation.
-define(NS_SI,                       'http://jabber.org/protocol/si').
-define(NS_SI_s,                     "http://jabber.org/protocol/si").

% Defined by XEP-0096: File Transfer.
-define(NS_FILE_TRANSFERT,
  'http://jabber.org/protocol/si/profile/file-transfer').
-define(NS_FILE_TRANSFERT_s,
  "http://jabber.org/protocol/si/profile/file-transfer").

% Defined by XEP-0100: Gateway Interaction.
-define(NS_GATEWAY,                  'jabber:iq:gateway').
-define(NS_GATEWAY_s,                "jabber:iq:gateway").

% Defined by XEP-0107: User Mood.
-define(NS_USER_MOOD,                'http://jabber.org/protocol/mood').
-define(NS_USER_MOOD_s,              "http://jabber.org/protocol/mood").

% Defined by XEP-0108: User Activity.
-define(NS_USER_ACTIVITY,            'http://jabber.org/protocol/activity').
-define(NS_USER_ACTIVITY_s,          "http://jabber.org/protocol/activity").

% Defined by XEP-0114: Jabber Component Protocol.
-define(NS_COMPONENT_ACCEPT,         'jabber:component:accept').
-define(NS_COMPONENT_CONNECT,        'jabber:component:connect').
-define(NS_COMPONENT_ACCEPT_s,       "jabber:component:accept").
-define(NS_COMPONENT_CONNECT_s,      "jabber:component:connect").

% Defined by XEP-0115: Entity Capabilities.
-define(NS_CAPS,                     'http://jabber.org/protocol/caps').
-define(NS_CAPS_s,                   "http://jabber.org/protocol/caps").

% Defined by XEP-0118: User Tune.
-define(NS_USER_TUNE,                'http://jabber.org/protocol/tune').
-define(NS_USER_TUNE_s,              "http://jabber.org/protocol/tune").

% Defined by XEP-0122: Data Forms Validation.
-define(NS_DATA_FORMS_VALIDATE,
  'http://jabber.org/protocol/xdata-validate').
-define(NS_DATA_FORMS_VALIDATE_s,
  "http://jabber.org/protocol/xdata-validate").

% Defined by XEP-0124: Bidirectional-streams Over Synchronous HTTP.
-define(NS_BOSH,                     'urn:xmpp:xbosh').
-define(NS_BOSH_s,                   "urn:xmpp:xbosh").

-define(NS_HTTP_BIND,                'http://jabber.org/protocol/httpbind').
-define(NS_HTTP_BIND_s,              "http://jabber.org/protocol/httpbind").

% Defined by XEP-0130: Waiting Lists.
-define(NS_WAITING_LIST,             'http://jabber.org/protocol/waitinglist').
-define(NS_WAITING_LIST_s,           "http://jabber.org/protocol/waitinglist").

% Defined by XEP-0131: Stanza Headers and Internet Metadata (SHIM).
-define(NS_SHIM,                     'http://jabber.org/protocol/shim').
-define(NS_SHIM_s,                   "http://jabber.org/protocol/shim").

% Defined by XEP-0136: Message Archiving.
-define(NS_ARCHIVING,                'urn:xmpp:archive').
-define(NS_ARCHIVING_s,              "urn:xmpp:archive").

% Defined by XEP-0137: Publishing Stream Initiation Requests.
-define(NS_SI_PUB,                   'http://jabber.org/protocol/sipub').
-define(NS_SI_PUB_s,                 "http://jabber.org/protocol/sipub").

% Defined by XEP-0138: Stream Compression.
-define(NS_COMPRESS,                 'http://jabber.org/protocol/compress').
-define(NS_COMPRESS_FEAT,            'http://jabber.org/features/compress').
-define(NS_COMPRESS_s,               "http://jabber.org/protocol/compress").
-define(NS_COMPRESS_FEAT_s,          "http://jabber.org/features/compress").

% Defined by XEP-0141: Data Forms Layout.
-define(NS_DATA_FORMS_LAYOUT,
  'http://jabber.org/protocol/xdata-layout').
-define(NS_DATA_FORMS_LAYOUT_s,
  "http://jabber.org/protocol/xdata-layout").

% Defined by XEP-0144: Roster Item Exchange.
-define(NS_ROSTER_EXCHANGE,          'http://jabber.org/protocol/rosterx').
-define(NS_ROSTER_EXCHANGE_s,        "http://jabber.org/protocol/rosterx").

% Defined by XEP-0145: Annotations.
-define(NS_ROSTER_NOTES,             'storage:rosternotes').
-define(NS_ROSTER_NOTES_s,           "storage:rosternotes").

% Defined by XEP-0153: vCard-Based Avatars.
-define(NS_VCARD_UPDATE,             'vcard-temp:x:update').
-define(NS_VCARD_UPDATE_s,           "vcard-temp:x:update").

% Defined by XEP-0154: User Profile.
-define(NS_USER_PROFILE,             'urn:xmpp:tmp:profile').
-define(NS_USER_PROFILE_s,           "urn:xmpp:tmp:profile").

% Defined by XEP-0157: Contact Addresses for XMPP Services.
-define(NS_SERVERINFO,               'http://jabber.org/network/serverinfo').
-define(NS_SERVERINFO_s,             "http://jabber.org/network/serverinfo").

% Defined by XEP-0158: Robot Challenges.
-define(NS_ROBOT_CHALLENGE,          'urn:xmpp:tmp:challenge').
-define(NS_ROBOT_CHALLENGE_s,        "urn:xmpp:tmp:challenge").

% Defined by XEP-0160: Best Practices for Handling Offline Messages.
-define(NS_MSGOFFLINE,               'msgoffline').
-define(NS_MSGOFFLINE_s,             "msgoffline").

% Defined by XEP-0161: Abuse Reporting.
-define(NS_ABUSE_REPORTING,          'urn:xmpp:tmp:abuse').
-define(NS_ABUSE_REPORTING_s,        "urn:xmpp:tmp:abuse").

% Defined by XEP-0166: Jingle.
-define(NS_JINGLE,                   'urn:xmpp:tmp:jingle').
-define(NS_JINGLE_ERRORS,            'urn:xmpp:tmp:jingle:errors').
-define(NS_JINGLE_s,                 "urn:xmpp:tmp:jingle").
-define(NS_JINGLE_ERRORS_s,          "urn:xmpp:tmp:jingle:errors").

% Defined by XEP-0167: Jingle RTP Sessions.
-define(NS_JINGLE_RPT,               'urn:xmpp:tmp:jingle:apps:rtp').
-define(NS_JINGLE_RPT_INFO,          'urn:xmpp:tmp:jingle:apps:rtp:info').
-define(NS_JINGLE_RPT_s,             "urn:xmpp:tmp:jingle:apps:rtp").
-define(NS_JINGLE_RPT_INFO_s,        "urn:xmpp:tmp:jingle:apps:rtp:info").

% Defined by XEP-0168: Resource Application Priority.
-define(NS_RAP,
  'http://www.xmpp.org/extensions/xep-0168.html#ns').
-define(NS_RAP_ROUTE,
  'http://www.xmpp.org/extensions/xep-0168.html#ns-route').
-define(NS_RAP_s,
  "http://www.xmpp.org/extensions/xep-0168.html#ns").
-define(NS_RAP_ROUTE_s,
  "http://www.xmpp.org/extensions/xep-0168.html#ns-route").

% Defined by XEP-0171: Language Translation.
-define(NS_LANG_TRANS,               'urn:xmpp:langtrans').
-define(NS_LANG_TRANS_ITEMS,         'urn:xmpp:langtrans#items').
-define(NS_LANG_TRANS_s,             "urn:xmpp:langtrans").
-define(NS_LANG_TRANS_ITEMS_s,       "urn:xmpp:langtrans#items").

% Defined by XEP-0172: User Nickname.
-define(NS_USER_NICKNAME,            'http://jabber.org/protocol/nick').
-define(NS_USER_NICKNAME_s,          "http://jabber.org/protocol/nick").

% Defined by XEP-0176: Jingle ICE-UDP Transport Method.
-define(NS_JINGLE_ICE_UDP,           'urn:xmpp:tmp:jingle:transports:ice-udp').
-define(NS_JINGLE_ICE_UDP_s,         "urn:xmpp:tmp:jingle:transports:ice-udp").

% Defined by XEP-0177: Jingle Raw UDP Transport Method.
-define(NS_JINGLE_RAW_UDP,
  'urn:xmpp:tmp:jingle:transports:raw-udp').
-define(NS_JINGLE_RAW_UDP_INFO,
  'urn:xmpp:tmp:jingle:transports:raw-udp:info').
-define(NS_JINGLE_RAW_UDP_s,
  "urn:xmpp:tmp:jingle:transports:raw-udp").
-define(NS_JINGLE_RAW_UDP_INFO_s,
  "urn:xmpp:tmp:jingle:transports:raw-udp:info").

% Defined by XEP-0181: Jingle DTMF.
-define(NS_JINGLE_DTMF,              'urn:xmpp:tmp:jingle:dtmf').
-define(NS_JINGLE_DTMF_s,            "urn:xmpp:tmp:jingle:dtmf").

% Defined by XEP-0184: Message Receipts.
-define(NS_RECEIPTS,                 'urn:xmpp:receipts').
-define(NS_RECEIPTS_s,               "urn:xmpp:receipts").

% Defined by XEP-0186: Invisible Command.
-define(NS_INVISIBLE_COMMAND,        'urn:xmpp:tmp:invisible').
-define(NS_INVISIBLE_COMMAND_s,      "urn:xmpp:tmp:invisible").

% Defined by XEP-0189: Public Key Publishing.
-define(NS_PUBKEY,                   'urn:xmpp:tmp:pubkey').
-define(NS_PUBKEY_s,                 "urn:xmpp:tmp:pubkey").

% Defined by XEP-0191: Simple Communications Blocking.
-define(NS_BLOCKING,                 'urn:xmpp:blocking').
-define(NS_BLOCKING_ERRORS,          'urn:xmpp:blocking:errors').
-define(NS_BLOCKING_s,               "urn:xmpp:blocking").
-define(NS_BLOCKING_ERRORS_s,        "urn:xmpp:blocking:errors").

% Defined by XEP-0194: User Chatting.
-define(NS_USER_CHATTING,
  'http://www.xmpp.org/extensions/xep-0194.html#ns').
-define(NS_USER_CHATTING_s,
  "http://www.xmpp.org/extensions/xep-0194.html#ns").

% Defined by XEP-0195: User Browsing.
-define(NS_USER_BROWSING,
  'http://www.xmpp.org/extensions/xep-0195.html#ns').
-define(NS_USER_BROWSING_s,
  "http://www.xmpp.org/extensions/xep-0195.html#ns").

% Defined by XEP-0196: User Gaming.
-define(NS_USER_GAMING,
  'http://www.xmpp.org/extensions/xep-0196.html#ns').
-define(NS_USER_GAMING_s,
  "http://www.xmpp.org/extensions/xep-0196.html#ns").

% Defined by XEP-0197: User Viewing.
-define(NS_USER_VIEWING,
  'http://www.xmpp.org/extensions/xep-0197.html#ns').
-define(NS_USER_VIEWING_s,
  "http://www.xmpp.org/extensions/xep-0197.html#ns").

% Defined by XEP-0198: Stanza Acknowledgements.
-define(NS_STANZA_ACK,
  'http://www.xmpp.org/extensions/xep-0198.html#ns').
-define(NS_STANZA_ACK_s,
  "http://www.xmpp.org/extensions/xep-0198.html#ns").

% Defined by XEP-0199: XMPP Ping.
-define(NS_PING,                     'urn:xmpp:ping').
-define(NS_PING_s,                   "urn:xmpp:ping").

% Defined by XEP-0202: Entity Time.
-define(NS_TIME,                     'urn:xmpp:time').
-define(NS_TIME_s,                   "urn:xmpp:time").

% Defined by XEP-0203: Delayed Delivery.
-define(NS_DELAY,                    'urn:xmpp:delay').
-define(NS_DELAY_s,                  "urn:xmpp:delay").

% Defined by XEP-0206: XMPP Over BOSH.
-define(NS_XBOSH,                    'urn:xmpp:xbosh').
-define(NS_XBOSH_s,                  "urn:xmpp:xbosh").
-define(NS_XBOSH_pfx,                "xmpp").

% Defined by XEP-0208: Bootstrapping Implementation of Jingle.
-define(NS_JINGLE_BOOTSTRAPING,
  'http://www.xmpp.org/extensions/xep-0208.html#ns').
-define(NS_JINGLE_BOOTSTRAPING_s,
  "http://www.xmpp.org/extensions/xep-0208.html#ns").

% Defined by XEP-0209: Metacontacts.
-define(NS_METACONTACTS,             'storage:metacontacts').
-define(NS_METACONTACTS_s,           "storage:metacontacts").

% Defined by XEP-0215: External Service Discovery.
-define(NS_EXTERNAL_DISCO,
  'http://www.xmpp.org/extensions/xep-0215.html#ns').
-define(NS_EXTERNAL_DISCO_s,
  "http://www.xmpp.org/extensions/xep-0215.html#ns").

% Defined by XEP-0220: Server Dialback.
-define(NS_DIALBACK,                 'jabber:server:dialback').
-define(NS_DIALBACK_FEAT,            'urn:xmpp:features:dialback').
-define(NS_DIALBACK_pfx,             "db").
-define(NS_DIALBACK_s,               "jabber:server:dialback").
-define(NS_DIALBACK_FEAT_s,          "urn:xmpp:features:dialback").

% Defined by XEP-0221: Data Forms Media Element.
-define(NS_DATA_FORMS_MEDIA,         'urn:xmpp:tmp:media-element').
-define(NS_DATA_FORMS_MEDIA_s,       "urn:xmpp:tmp:media-element").

% Defined by XEP-0224: Attention.
-define(NS_ATTENTION,
  'http://www.xmpp.org/extensions/xep-0224.html#ns').
-define(NS_ATTENTION_s,
  "http://www.xmpp.org/extensions/xep-0224.html#ns").

% Defined by XEP-0225: Component Connections.
-define(NS_COMPONENT_CONNECTION,     'urn:xmpp:tmp:component').
-define(NS_COMPONENT_CONNECTION_s,   "urn:xmpp:tmp:component").

% Defined by XEP-0227: Portable Import/Export Format for XMPP-IM Servers.
-define(NS_SERVER_IMPORT_EXPORT,
  'http://www.xmpp.org/extensions/xep-0227.html#ns').
-define(NS_SERVER_IMPORT_EXPORT_s,
  "http://www.xmpp.org/extensions/xep-0227.html#ns").

% Defined by XEP-0231: Data Element.
-define(NS_DATA,                     'urn:xmpp:tmp:data-element').
-define(NS_DATA_s,                   "urn:xmpp:tmp:data-element").

% Defined by XEP-0233: Use of Domain-Based Service Names in XMPP SASL
% Negotiation.
-define(NS_DOMAIN_BASED_NAME,        'urn:xmpp:tmp:domain-based-name').
-define(NS_DOMAIN_BASED_NAME_s,      "urn:xmpp:tmp:domain-based-name").

% Defined by XEP-0234: Jingle File Transfer.
-define(NS_JINGLE_FILE_TRANSFERT,    'urn:xmpp:tmp:jingle:apps:file-transfer').
-define(NS_JINGLE_FILE_TRANSFERT_s,  "urn:xmpp:tmp:jingle:apps:file-transfer").

% Defined by XEP-0235: Authorization Tokens.
-define(NS_AUTH_TOKEN,               'urn:xmpp:tmp:auth-token').
-define(NS_AUTH_TOKEN_s,             "urn:xmpp:tmp:auth-token").

% Defined by XEP-0237: Roster Sequencing.
-define(NS_ROSTER_SEQ,               'urn:xmpp:tmp:roster-sequencing').
-define(NS_ROSTER_SEQ_s,             "urn:xmpp:tmp:roster-sequencing").

% Defined by XEP-0244: IO Data.
-define(NS_IO_DATA,                  'urn:xmpp:tmp:io-data').
-define(NS_IO_DATA_s,                "urn:xmpp:tmp:io-data").

% Defined by XEP-0247: Jingle XML Streams.
-define(NS_JINGLE_XML_STREAM,        'urn:xmpp:tmp:jingle:apps:xmlstream').
-define(NS_JINGLE_XML_STREAM_s,      "urn:xmpp:tmp:jingle:apps:xmlstream").

% Defined by XHTML 1.0.
-define(NS_XHTML,                    'http://www.w3.org/1999/xhtml').
-define(NS_XHTML_s,                  "http://www.w3.org/1999/xhtml").
