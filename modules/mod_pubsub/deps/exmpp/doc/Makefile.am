EXTRA_DIST = overview.edoc stylesheet.css

stylesheet.css:
	(test "$(srcdir)" != "." &&\
	 (test ! -e "$@" || test "$(srcdir)/$@" -nt "$@") &&\
	 cp "$(srcdir)/$@" "$@") || :

erlang.png:

ERL_SRCS = $(top_srcdir)/src/core/*.erl					\
	   $(top_srcdir)/src/server/*.erl				\
	   $(top_srcdir)/src/client/*.erl				\
	   $(top_srcdir)/src/compat/*.erl

HTML_FILES = index.html							\
	     modules-frame.html						\
	     overview-summary.html					\
	     packages-frame.html					\
	     exmpp.html							\
	     exmpp_client_binding.html					\
	     exmpp_client_compression.html				\
	     exmpp_client_disco.html				\
	     exmpp_client_legacy_auth.html				\
	     exmpp_client_privacy.html					\
	     exmpp_client_pubsub.html					\
	     exmpp_client_register.html					\
	     exmpp_client_roster.html					\
	     exmpp_client_sasl.html					\
	     exmpp_client_session.html					\
	     exmpp_client_tls.html					\
	     exmpp_compress.html					\
	     exmpp_component.html					\
	     exmpp_dialback.html					\
	     exmpp_internals.html					\
	     exmpp_iq.html						\
	     exmpp_jid.html						\
	     exmpp_message.html						\
	     exmpp_presence.html					\
	     exmpp_server_binding.html					\
	     exmpp_server_compression.html				\
	     exmpp_server_legacy_auth.html				\
	     exmpp_server_privacy.html					\
	     exmpp_server_sasl.html					\
	     exmpp_server_session.html					\
	     exmpp_server_tls.html					\
	     exmpp_session.html						\
	     exmpp_ssl.html						\
	     exmpp_stanza.html						\
	     exmpp_stream.html						\
	     exmpp_stringprep.html					\
	     exmpp_sup.html						\
	     exmpp_tcp.html						\
	     exmpp_bosh.html						\
	     exmpp_tls.html						\
	     exmpp_utils.html						\
	     exmpp_xml.html						\
	     exmpp_xmlstream.html					\
	     ejabberd_socket.html					\
	     ejabberd_zlib.html						\
	     jlib.html							\
	     stringprep.html						\
	     tls.html							\
	     xml.html							\
	     xml_stream.html

$(HTML_FILES): overview.edoc $(ERL_SRCS) stylesheet.css
	$(ERL) +B -noshell\
		-run edoc_run application "'$(PACKAGE_NAME)'" '"$(top_srcdir)"'\
		'[{def, {vsn, "$(PACKAGE_VERSION)"}}, {dir, "."}, {stylesheet, "stylesheet.css"}, {preprocess, true}, {includes, ["$(top_builddir)/include"]}, no_packages]'\
		-s erlang halt
	@for f in $(HTML_FILES); do					\
		$(SED) -i.bak						\
		-e 's|</title>|</title><meta http-equiv="Content-Type" content="text/html; charset=UTF-8">|'						\
		-e 's|\xa9|\&copy;|g' $$f &&				\
		rm -f $$f.bak;						\
	done

all-local: $(HTML_FILES)

clean-local:
	(test "$(srcdir)" != "." && rm -f stylesheet.css) || :

htmldocdir = $(ERLANG_INSTALL_LIB_DIR_exmpp)/doc/html

htmldoc_DATA = $(HTML_FILES) stylesheet.css erlang.png

CLEANFILES = $(HTML_FILES) edoc-info erlang.png
