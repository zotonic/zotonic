SUBDIRS = internal

headersdir = $(ERLANG_INSTALL_LIB_DIR_exmpp)/include

dist_headers_DATA = exmpp.hrl						\
		    exmpp_nss.hrl					\
		    exmpp_xml.hrl					\
		    exmpp_xmpp.hrl					\
		    exmpp_client.hrl

if BUILD_COMPAT
dist_headers_DATA += jlib.hrl
endif
