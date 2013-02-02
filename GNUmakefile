ERL       ?= erl
ERLC      ?= $(ERL)c
APP       := zotonic
PARSER    =src/erlydtl/erlydtl_parser

# List existing dependencies eventually putting lager first (if exists)
_DEPS_DIRS  =$(filter-out deps/lager/, $(filter %/, $(wildcard deps/*/)))
_DEPS       =$(_DEPS_DIRS:%/=%)
_WITH_LAGER =$(wildcard deps/lager) $(_DEPS)
DEPS_CMD    =$(foreach dep, $(_WITH_LAGER), compile-rule/$(dep))

# Erlang Rebar downloading, see: https://groups.google.com/forum/?fromgroups=#!topic/erlang-programming/U0JJ3SeUv5Y
REBAR=$(shell which rebar || echo ./rebar)
REBAR_DEPS=$(shell which rebar || echo ../../rebar)
REBAR_URL=http://cloud.github.com/downloads/basho/rebar/rebar

./rebar:
	$(ERL) -noshell -s inets -s ssl \
	  -eval 'httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar"}])' \
	  -s init stop
	chmod +x ./rebar

# Helper targets
.PHONY: erl
erl:
	@$(ERL) -pa $(wildcard deps/*/ebin) -pa ebin -noinput +B \
	  -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

$(PARSER).erl: $(PARSER).yrl
	$(ERLC) -o src/erlydtl $(PARSER).yrl

ebin/$(APP).app: src/$(APP).app.src
	cp src/$(APP).app.src $@

# Phony rule to compile a particular dependency with Rebar
compile-rule/%: $(REBAR)
	cd $* && $(REBAR_DEPS) compile

# Use Rebar to get, update and compile dependencies
.PHONY: get-deps update-deps compile-deps compile-zotonic compile

get-deps: $(REBAR)
	$(REBAR) get-deps

update-deps: $(REBAR)
	$(REBAR) update-deps

compile-deps: $(REBAR) $(DEPS_CMD)

compile-zotonic: $(PARSER).erl erl ebin/$(APP).app

compile: all

# Default target - call all compile rules in succession
.PHONY: all
all: compile-deps compile-zotonic

# Generate documentation
.PHONY: docs edocs
docs:
	@echo Building HTML documentation...
	cd doc && make stubs && make html
	@echo HTML documentation is now available in doc/_build/html/

edocs:
	@echo Building reference edoc documentation...
	bin/zotonic generate-edoc

# Cleaning
.PHONY: clean_logs
clean_logs:
	@echo "deleting logs:"
	rm -f erl_crash.dump $(PARSER).erl
	rm -f priv/log/*

.PHONY: clean
clean: clean_logs $(REBAR)
	@echo "removing:"
	rm -f ebin/*.beam ebin/*.app
	@echo "cleaning dependencies:"
	$(REBAR) clean

.PHONY: dist-clean
dist-clean: clean
	rm -f ./rebar
