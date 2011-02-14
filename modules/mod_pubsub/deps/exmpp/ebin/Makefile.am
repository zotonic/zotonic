beamdir = $(ERLANG_INSTALL_LIB_DIR_exmpp)/ebin

BEAM_FILES = exmpp.app							\
	     exmpp.appup						\
	     exmpp.beam							\
	     exmpp_compress.beam					\
	     exmpp_dialback.beam					\
	     exmpp_internals.beam					\
	     exmpp_utils.beam						\
	     exmpp_iq.beam						\
	     exmpp_jid.beam						\
	     exmpp_message.beam						\
	     exmpp_presence.beam					\
	     exmpp_stanza.beam						\
	     exmpp_stream.beam						\
	     exmpp_stringprep.beam					\
	     exmpp_sup.beam						\
	     exmpp_tls.beam						\
	     exmpp_xml.beam						\
	     exmpp_xmlstream.beam					\
	     exmpp_known_nss.beam					\
	     exmpp_known_elems.beam					\
	     exmpp_known_attrs.beam					\
	     exmpp_client_binding.beam					\
	     exmpp_client_compression.beam				\
	     exmpp_client_disco.beam					\
	     exmpp_client_legacy_auth.beam				\
	     exmpp_client_privacy.beam					\
	     exmpp_client_pubsub.beam					\
	     exmpp_client_register.beam					\
	     exmpp_client_roster.beam					\
	     exmpp_client_sasl.beam					\
	     exmpp_client_session.beam					\
	     exmpp_client_tls.beam					\
	     exmpp_client_muc.beam					\
	     exmpp_server_binding.beam					\
	     exmpp_server_compression.beam				\
	     exmpp_server_legacy_auth.beam				\
	     exmpp_server_privacy.beam					\
	     exmpp_server_sasl.beam					\
	     exmpp_server_session.beam					\
	     exmpp_server_tls.beam					\
	     exmpp_session.beam						\
	     exmpp_ssl.beam						\
	     exmpp_tcp.beam						\
	     exmpp_bosh.beam						\
	     exmpp_component.beam

if BUILD_COMPAT
BEAM_FILES += ejabberd_socket.beam					\
	      ejabberd_zlib.beam					\
	      jlib.beam							\
	      stringprep.beam						\
	      tls.beam							\
	      xml.beam							\
	      xml_stream.beam
endif

beam_DATA = $(BEAM_FILES)

CLEANFILES = $(beam_DATA)

if ENABLE_EXAMPLES
examplesdir = $(ERLANG_INSTALL_LIB_DIR_exmpp)/examples/ebin

examples_DATA = echo_client.beam

CLEANFILES += $(examples_DATA)
endif
