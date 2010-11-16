ERL          ?= erl
ERLC          = erlc
EBIN_DIRS    := $(wildcard deps/*/ebin)
APP          := zotonic
PARSER        =src/erlydtl/erlydtl_parser

all: z_logger mochiweb webmachine erlang-oauth exmpp $(PARSER).erl erl ebin/$(APP).app 

erl:
	@$(ERL) -pa $(EBIN_DIRS) -pa ebin -noinput +B \
	  -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

$(PARSER).erl: $(PARSER).yrl
	$(ERLC) -o src/erlydtl src/erlydtl/erlydtl_parser.yrl

z_logger:
	cd deps/z_logger && $(MAKE)

mochiweb:
	cd deps/mochiweb && $(MAKE)

webmachine:
	cd deps/webmachine && $(MAKE)

erlang-oauth:
	cd deps/erlang-oauth && $(MAKE)

exmpp:
	cd deps/exmpp && (test -f Makefile || ./configure --disable-documentation) && $(MAKE)

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

clean: 
	@echo "removing:"
	(cd deps/z_logger; $(MAKE) clean)
	(cd deps/mochiweb; $(MAKE) clean)
	(cd deps/webmachine; $(MAKE) clean)
	(cd deps/erlang-oauth; $(MAKE) clean)
	(cd deps/exmpp; $(MAKE) clean)
	rm -f ebin/*.beam ebin/*.app
	rm -f erl_crash.dump $(PARSER).erl
	rm -f priv/log/*

ebin/$(APP).app:
	cp src/$(APP).app $@

