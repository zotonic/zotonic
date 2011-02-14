ETEST = $(builddir)/etest
EUNIT_REPORTER = eunit_tty

ERL_FILES = $(EUNIT_REPORTER).erl					\
	    xml_parser.erl						\
	    xml_attributes.erl						\
	    jid.erl							\
	    stanza.erl							\
	    iq.erl

HRL_FILES = $(top_srcdir)/include/exmpp_xml.hrl

JUNIT_REPORTS = exmpp-junit-*.xml

REAL_TESTS =

if WITH_EXPAT
endif

REAL_TESTS += xml_parser						\
	      xml_attributes						\
	      jid							\
	      stanza							\
	      iq

TESTS = $(REAL_TESTS)

EXTRA_DIST = $(ERL_FILES)

CLEANFILES = $(EUNIT_REPORTER).beam $(TESTS) $(JUNIT_REPORTS) erl_crash.dump

$(EUNIT_REPORTER).beam: $(srcdir)/$(EUNIT_REPORTER).erl $(HRL_FILES)
	@(echo "Recompile: $(srcdir)/$(EUNIT_REPORTER)" &&		\
	  $(ERLC) -Wall +debug_info +report +warn_export_vars		\
	  +warn_shadow_vars +warn_unused_import				\
	  -I $(top_srcdir)/include $(srcdir)/$(EUNIT_REPORTER).erl) || :

$(TESTS): $(EUNIT_REPORTER).beam $(ETEST)
	@(test ! -e $@ -o $(ETEST) -nt $@ && echo "Create: $@" &&	\
	  cp $(ETEST) $@ && chmod 755 $@) || :

check-junit: $(EUNIT_REPORTER).beam
	-rm -f $(JUNIT_REPORTS)
	$(ETEST) -junit $(TESTS)

.PHONY: check-junit
