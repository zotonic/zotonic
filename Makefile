ERL          ?= erl
ERLC          = erlc
EBIN_DIRS    := $(wildcard deps/*/ebin)
APP          := zotonic
PARSER        =src/erlydtl/erlydtl_parser

all: mochiweb webmachine erlang-oauth exmpp $(PARSER).erl erl ebin/$(APP).app 

erl:
	@$(ERL) -pa $(EBIN_DIRS) -pa ebin -noinput +B \
	  -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

$(PARSER).erl: $(PARSER).yrl
	$(ERLC) -o src/erlydtl src/erlydtl/erlydtl_parser.yrl

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
	(cd deps/mochiweb; $(MAKE) clean)
	(cd deps/webmachine; $(MAKE) clean)
	(cd deps/erlang-oauth; $(MAKE) clean)
	@rm -fv ebin/*.beam ebin/*.app
	@rm -fv erl_crash.dump $(PARSER).erl
	@rm -fv priv/log/*

ebin/$(APP).app:
	cp src/$(APP).app $@

