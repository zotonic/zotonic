ERL       ?= erl
ERLC      ?= $(ERL)c
APP       := zotonic
PARSER    := src/erlydtl_parser

# Erlang Rebar downloading
# see: https://groups.google.com/forum/?fromgroups=#!topic/erlang-programming/U0JJ3SeUv5Y
REBAR := ./rebar3
REBAR_URL := https://s3.amazonaws.com/rebar3/rebar3
REBAR_OPTS ?=
# Default target - update sources and call all compile rules in succession
.PHONY: all
all: compile

$(REBAR):
	$(ERL) -noshell -s inets -s ssl \
	  -eval '{ok, saved_to_file} = httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "$(REBAR)"}])' \
	  -s init stop
	chmod +x $(REBAR)

# Use Rebar to get, update and compile dependencies
.PHONY: upgrade-deps compile-zotonic compile test

compile-zotonic:
	bin/zotonic compile

upgrade-deps: $(REBAR)
	$(REBAR) $(REBAR_OPTS) upgrade

compile: $(REBAR)
	$(REBAR) $(REBAR_OPTS) compile
	bin/zotonic compile

test: compile
	bin/zotonic runtests erlydtl && bin/zotonic runtests

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
	@echo "cleaning ebin:"
	$(REBAR) $(REBAR_OPTS) clean

.PHONY: dist-clean
dist-clean: clean
	$(REBAR) $(REBAR_OPTS) clean -a
	rm -f ./rebar3
