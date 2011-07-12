ERL       ?= erl
ERLC      ?= $(ERL)c
EBIN_DIRS := $(wildcard deps/*/ebin)
APP       := zotonic
PARSER     =src/erlydtl/erlydtl_parser

all: gen_smtp iconv z_logger mochiweb webmachine module-deps $(PARSER).erl erl ebin/$(APP).app 

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

webmachine:
	cd deps/webmachine && $(MAKE)

module-deps:
	@if [ "`find modules/ -name Makefile`" != "" ]; then for f in modules/*/Makefile; do echo $$f; $(MAKE) -C `dirname $$f`; done; fi
	@if [ "`find priv/modules/ -name Makefile`" != "" ]; then for f in priv/modules/*/Makefile; do echo $$f; $(MAKE) -C `dirname $$f`; done; fi
	@if [ "`find priv/sites/*/modules/ -name Makefile`" != "" ]; then for f in priv/sites/*/modules/*/Makefile; do echo $$f; $(MAKE) -C `dirname $$f`; done; fi

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

clean: 
	@echo "removing:"
	(cd deps/gen_smtp; $(MAKE) clean)
	(cd deps/z_logger; $(MAKE) clean)
	(cd deps/mochiweb; $(MAKE) clean)
	(cd deps/webmachine; $(MAKE) clean)
	(cd deps/iconv; ./rebar clean)
	@if [ "`find modules/ -name Makefile`" != "" ]; then for f in modules/*/Makefile; do echo $$f; $(MAKE) -C `dirname $$f` clean; done; fi
	@if [ "`find priv/modules/ -name Makefile`" != "" ]; then for f in priv/modules/*/Makefile; do echo $$f; $(MAKE) -C `dirname $$f` clean; done; fi
	@if [ "`find priv/sites/*/modules/ -name Makefile`" != "" ]; then for f in priv/sites/*/modules/*/Makefile; do echo $$f; $(MAKE) -C `dirname $$f` clean; done; fi
	rm -f ebin/*.beam ebin/*.app
	rm -f erl_crash.dump $(PARSER).erl
	rm -f priv/log/*

ebin/$(APP).app:
	cp src/$(APP).app $@

