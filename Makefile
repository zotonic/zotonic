ERL       ?= erl
ERLC      ?= $(ERL)c
EBIN_DIRS := $(wildcard deps/*/ebin)
APP       := zotonic
PARSER     =src/erlydtl/erlydtl_parser

MAKEFILES := $(shell find -L modules/ priv/sites/ priv/modules/ priv/sites/*/modules/ -maxdepth 2 -name Makefile)

all: gen_smtp iconv z_logger mochiweb webzmachine module-deps $(PARSER).erl erl ebin/$(APP).app 

erl:
	@$(ERL) -pa $(EBIN_DIRS) -pa ebin -noinput +B \
	  -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

$(PARSER).erl: $(PARSER).yrl
	$(ERLC) -o src/erlydtl $(PARSER).yrl

gen_smtp:
	cd deps/gen_smtp && $(MAKE)

iconv:
	cd deps/iconv && ./rebar compile

z_logger:
	cd deps/z_logger && $(MAKE)

mochiweb:
	cd deps/mochiweb && $(MAKE)

webzmachine:
	cd deps/webzmachine && $(MAKE)

module-deps:
	@if [ "${MAKEFILES}" != "" ]; then for f in ${MAKEFILES}; do echo $$f; $(MAKE) -C `dirname $$f`; done; fi


docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

clean: 
	@echo "removing:"
	(cd deps/gen_smtp; $(MAKE) clean)
	(cd deps/z_logger; $(MAKE) clean)
	(cd deps/mochiweb; $(MAKE) clean)
	(cd deps/webzmachine; $(MAKE) clean)
	(cd deps/iconv; ./rebar clean)
	@if [ "${MAKEFILES}" != "" ]; then for f in ${MAKEFILES}; do echo $$f; $(MAKE) -C `dirname $$f` clean; done; fi
	rm -f ebin/*.beam ebin/*.app
	rm -f erl_crash.dump $(PARSER).erl
	rm -f priv/log/*

ebin/$(APP).app:
	cp src/$(APP).app $@

