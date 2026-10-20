ERL       ?= erl
ERLC      ?= $(ERL)c
APP       := zotonic
PARSER    := src/erlydtl_parser

REBAR ?= ./rebar3
REBAR_URL := https://github.com/erlang/rebar3/releases/download/3.14.4/rebar3
REBAR_ENV = EXOMETER_PACKAGES="-afunix -netlink -exo +setup"

# Default target - compile dependencies and zotonic
.PHONY: all
all: compile

./rebar3:
	$(ERL) -noshell -s inets -s ssl \
	  -eval '{ok, saved_to_file} = httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar3"}])' \
	  -s init stop
	chmod +x ./rebar3

# Use Rebar3 to manage and compile dependencies
.PHONY: compile-zotonic compile upgrade-deps

upgrade-deps: $(REBAR)
	$(REBAR_ENV) $(REBAR) upgrade

compile-zotonic:
	$(REBAR_ENV) $(REBAR) compile
	mkdir -p _build/default/lib/zotonic && ln -sfn ../../../../modules _build/default/lib/zotonic/modules
	bin/zotonic compile

compile: $(REBAR)
	$(REBAR_ENV) $(REBAR) compile
	mkdir -p _build/default/lib/zotonic && ln -sfn ../../../../modules _build/default/lib/zotonic/modules
	bin/zotonic compile

# Generate documentation
.PHONY: docs edocs
docs:
	@echo Building HTML documentation...
	cd doc && $(MAKE) stubs && $(MAKE) html
	@echo HTML documentation is now available in doc/_build/html/

edocs:
	@echo Building reference edoc documentation...
	bin/zotonic generate-edoc

# Cleaning
.PHONY: clean_logs
clean_logs:
	@echo "deleting logs:"
	rm -f erl_crash.dump
	rm -rf priv/log/*

.PHONY: clean
clean: clean_logs $(REBAR)
	@echo "removing:"
	rm -f $(PARSER).erl src/erlydtl/erlydtl_parser.erl
	@echo "cleaning build:"
	$(REBAR_ENV) $(REBAR) clean

.PHONY: dist-clean
dist-clean: clean
	rm -f ./rebar3
	rm -rf _build
