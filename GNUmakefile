ERL       ?= erl
ERLC      ?= $(ERL)c
APP       := zotonic
PARSER    := src/erlydtl/erlydtl_parser

# Erlang Rebar downloading
# see: https://groups.google.com/forum/?fromgroups=#!topic/erlang-programming/U0JJ3SeUv5Y
REBAR := ./rebar
REBAR_URL := https://github.com/rebar/rebar/wiki/rebar
REBAR_ENV = EXOMETER_PACKAGES="-afunix -netlink -exo"

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

# Helper targets
.PHONY: erl

# First compile zotonic_compile, so that we can call "zotonic compile" to compile ourselves, then just call 'zotonic compile'.
erl: ebin/$(APP).app
	@$(ERL) -pa $(wildcard deps/*/ebin) -noinput -eval 'case make:files(["src/zotonic_compile.erl"], [{outdir, "ebin"}]) of up_to_date -> halt(0); error -> halt(1) end.'
	bin/zotonic compile

$(PARSER).erl: $(PARSER).yrl
	$(ERLC) -o src/erlydtl $(PARSER).yrl

ebin/$(APP).app: src/$(APP).app.src
	cp src/$(APP).app.src $@

# Use Rebar to get, update and compile dependencies
.PHONY: get-deps update-deps compile-deps compile-zotonic compile

get-deps: $(REBAR)
	$(REBAR_ENV) $(REBAR) $(REBAR_OPTS) get-deps

update-deps: $(REBAR)
	$(REBAR) $(REBAR_OPTS) update-deps

compile-deps: $(REBAR)
	$(REBAR_ENV) $(REBAR) $(REBAR_OPTS) compile

compile-zotonic: $(PARSER).erl erl

compile: compile-deps compile-zotonic

lock-deps: $(REBAR)
	$(REBAR_ENV) $(REBAR) $(REBAR_OPTS) lock-deps

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
	rm -f erl_crash.dump $(PARSER).erl
	rm -rf priv/log/*

.PHONY: clean
clean: clean_logs $(REBAR)
	@echo "removing:"
	rm -f ebin/*.beam ebin/*.app
	@echo "cleaning dependencies:"
	$(REBAR) $(REBAR_OPTS) clean

.PHONY: dist-clean
dist-clean: clean
	rm -f ./rebar
