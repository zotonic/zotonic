ERL       ?= erl
ERLC      ?= $(ERL)c
APP       := zotonic
PARSER     =src/erlydtl/erlydtl_parser

GIT_CHECK := $(shell test -d .git && git submodule update --init)
MAKEFILES := $(shell find -L deps modules priv/sites priv/modules priv/extensions priv/sites/*/modules -maxdepth 2 -name Makefile)

.PHONY: all
all: iconv mimetypes makefile-deps $(PARSER).erl erl ebin/$(APP).app 

.PHONY: erl
erl:
	@$(ERL) -pa $(wildcard deps/*/ebin) -pa ebin -noinput +B \
	  -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

$(PARSER).erl: $(PARSER).yrl
	$(ERLC) -o src/erlydtl $(PARSER).yrl

iconv:
	cd deps/iconv && ./rebar compile

mimetypes:
	cd deps/mimetypes && ./rebar compile

makefile-deps:
	@if [ "${MAKEFILES}" != "" ]; then for f in ${MAKEFILES}; do echo $$f; $(MAKE) -C `dirname $$f`; done; fi

.PHONY: docs
docs:
	bin/zotonic generate-edoc

.PHONY: clean_logs
clean_logs:
	@echo "deleting logs:"
	rm -f erl_crash.dump $(PARSER).erl
	rm -f priv/log/*

.PHONY: clean
clean: clean_logs
	@echo "removing:"
	(cd deps/iconv; ./rebar clean)
	(cd deps/mimetypes; ./rebar clean)
	@if [ "${MAKEFILES}" != "" ]; then for f in ${MAKEFILES}; do echo $$f; $(MAKE) -C `dirname $$f` clean; done; fi
	rm -f ebin/*.beam ebin/*.app

ebin/$(APP).app:
	cp src/$(APP).app $@
