ERL       ?= erl
ERLC      ?= $(ERL)c
APP       := zotonic
PARSER    := src/erlydtl_parser

# Erlang Rebar downloading
# see: https://groups.google.com/forum/?fromgroups=#!topic/erlang-programming/U0JJ3SeUv5Y
REBAR := ./rebar
REBAR_URL := https://github.com/rebar/rebar/wiki/rebar
REBAR_ENV = EXOMETER_PACKAGES="-afunix -netlink -exo +setup"

# The release branch should have a file named USE_REBAR_LOCKED
# See: https://github.com/lukyanov/rebar-lock-deps
use_locked_config = $(wildcard USE_REBAR_LOCKED)
ifeq ($(use_locked_config),USE_REBAR_LOCKED)
  rebar_config = rebar.config.lock
else
  rebar_config = rebar.config
endif
REBAR_OPTS = -C $(rebar_config)


# Default target - update sources and call all compile rules in succession
.PHONY: all
all: get-deps compile

./rebar:
	$(ERL) -noshell -s inets -s ssl \
	  -eval '{ok, saved_to_file} = httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar"}])' \
	  -s init stop
	chmod +x ./rebar

# Use Rebar to get, update and compile dependencies
.PHONY: delete-deps get-deps update-deps compile-deps compile-zotonic compile test

delete-deps: $(REBAR)
	$(REBAR_ENV) $(REBAR) $(REBAR_OPTS) delete-deps

get-deps: $(REBAR)
	$(REBAR_ENV) $(REBAR) $(REBAR_OPTS) get-deps

update-deps: $(REBAR)
	$(REBAR_ENV) $(REBAR) $(REBAR_OPTS) update-deps

refresh-deps: $(REBAR)
	$(REBAR_ENV) $(REBAR) $(REBAR_OPTS) refresh-deps

list-deps: $(REBAR)
	$(REBAR_ENV) $(REBAR) $(REBAR_OPTS) list-deps

compile-zotonic:
	$(REBAR_ENV) $(REBAR) $(REBAR_OPTS) skip_deps=true compile
	bin/zotonic compile

compile: get-deps
	$(REBAR_ENV) $(REBAR) $(REBAR_OPTS) compile
	bin/zotonic compile

lock-deps: $(REBAR)
	$(REBAR_ENV) $(REBAR) $(REBAR_OPTS) lock-deps

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
	$(REBAR_ENV) $(REBAR) $(REBAR_OPTS) clean

.PHONY: dist-clean
dist-clean: clean
	rm -f ./rebar
	find deps -type d -mindepth 1 -maxdepth 1 -exec rm -rf {} \;
