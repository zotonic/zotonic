MAKE_SPECS_LIST = $(srcdir)/make-specs-list
EXTRACT_KNOWN = $(srcdir)/extract-known-from-specs

KNOWN_NSS_SOURCE = $(srcdir)/exmpp_known_nss.in
KNOWN_ELEMS_SOURCE = $(srcdir)/exmpp_known_elems.in
KNOWN_ATTRS_SOURCE = $(srcdir)/exmpp_known_attrs.in

EXTRA_DIST = $(MAKE_SPECS_LIST) $(EXTRACT_KNOWN)			\
	     $(KNOWN_NSS_SOURCE)					\
	     $(KNOWN_ELEMS_SOURCE)					\
	     $(KNOWN_ATTRS_SOURCE)					\
	     exmpp_known_nss.hrl.awk					\
	     exmpp_known_nss.hrl.js					\
	     exmpp_known_elems.hrl.awk					\
	     exmpp_known_elems.hrl.js					\
	     exmpp_known_attrs.hrl.awk					\
	     exmpp_known_attrs.hrl.js

BUILT_SOURCES = exmpp_known_nss.hrl					\
		exmpp_known_elems.hrl					\
		exmpp_known_attrs.hrl

internalheadersdir = $(ERLANG_INSTALL_LIB_DIR_exmpp)/include/internal

internalheaders_HEADERS = $(BUILT_SOURCES) exmpp_xmpp.hrl

CLEANFILES = $(BUILT_SOURCES)

# --------------------------------------------------------------------
# Known namespaces/names/attributes headers generation.
# --------------------------------------------------------------------

exmpp_known_nss.hrl: $(srcdir)/exmpp_known_nss.hrl.awk $(KNOWN_NSS_SOURCE)
	$(AWK) -f $(srcdir)/exmpp_known_nss.hrl.awk $(KNOWN_NSS_SOURCE) > $@

exmpp_known_elems.hrl: $(srcdir)/exmpp_known_elems.hrl.awk $(KNOWN_ELEMS_SOURCE)
	$(AWK) -f $(srcdir)/exmpp_known_elems.hrl.awk $(KNOWN_ELEMS_SOURCE) > $@

exmpp_known_attrs.hrl: $(srcdir)/exmpp_known_attrs.hrl.awk $(KNOWN_ATTRS_SOURCE)
	$(AWK) -f $(srcdir)/exmpp_known_attrs.hrl.awk $(KNOWN_ATTRS_SOURCE) > $@

# --------------------------------------------------------------------
# Known namespaces/names/attributes updates.
# --------------------------------------------------------------------

updateknown: $(MAKE_SPECS_LIST) $(EXTRACT_KNOWN)
	@$(MAKE_SPECS_LIST) | $(EXTRACT_KNOWN)				\
		$(KNOWN_NSS_SOURCE)					\
		$(KNOWN_NAMES_SOURCE)					\
		$(KNOWN_ATTRS_SOURCE)

.PHONY: updateknown
