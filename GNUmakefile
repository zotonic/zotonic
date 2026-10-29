ERL       ?= erl
ERLC      ?= $(ERL)c
APP       := zotonic

# Prefer the checkout's rebar3. An explicit download also selects it for this run.
REBAR := ./rebar3
ifeq ($(wildcard ./rebar3),)
ifeq ($(filter rebar3 ./rebar3,$(MAKECMDGOALS)),)
REBAR := $(or $(shell command -v rebar3),./rebar3)
endif
endif

REBAR_URL := https://github.com/erlang/rebar3/releases/download/3.27.3/rebar3
REBAR_SHA256 := f1d87d78decfc56b87d971c75ee6355698ec0c49fadd57f2c056043b7ca291b5
REBAR_STAMP := ./.rebar3-$(REBAR_SHA256)
REBAR_OPTS ?=

ifdef https_proxy
PROXY_HOSTNAME := $(shell echo ${https_proxy} | awk -F ":" '{print $$2}' | cut -c 3-)
PROXY_PORT := $(shell echo ${https_proxy} | awk -F ":" '{print $$3}')
PROXY_ARGS := --proxy "$(PROXY_HOSTNAME)" "$(PROXY_PORT)"
endif

.PHONY: all upgrade-deps compile-core compile dev test update-cotonic update

# Default target - update sources and call all compile rules in succession
all: compile-core compile
	@echo "Zotonic" `bin/zotonic -v` "was successfully compiled"

$(REBAR_STAMP):
	rm -f ./.rebar3-*
	touch $@
	rm -f ./rebar3

# Run `make rebar3` to download the pinned version, even when PATH has rebar3.
./rebar3: $(REBAR_STAMP)
	./tools/download.escript "$(REBAR_URL)" "./rebar3" --sha256 "$(REBAR_SHA256)" --executable $(PROXY_ARGS)

# Use Rebar to get, update and compile dependencies
.PHONY: upgrade-deps compile-zotonic compile test

upgrade-deps: $(REBAR)
	$(REBAR) $(REBAR_OPTS) upgrade --all

compile-core: $(REBAR) _build/default/lib/zotonic_core/include/zotonic.hrl

_build/default/lib/zotonic_core/include/zotonic.hrl: $(REBAR)
	@echo ============================================================================
	@echo ======================= Compiling Zotonic core apps =========================
	@echo ============================================================================
	ZOTONIC_APPS=none $(REBAR) $(REBAR_OPTS) compile

compile: $(REBAR)
	@echo ============================================================================
	@echo =========================== Compiling user apps ============================
	@echo ============================================================================
	ZOTONIC_UMBRELLA=1 $(REBAR) $(REBAR_OPTS) compile

dev:
	docker-compose run --rm --service-ports zotonic sh

test: compile
	bin/zotonic runtests

update:
	@echo "Updating Zotonic from branch: " `git rev-parse --abbrev-ref HEAD`
	@git pull
	@(cd apps/zotonic_mod_base/; ./cotonic-update.sh)

update-cotonic:
	@(cd apps/zotonic_mod_base/; ./cotonic-update.sh)

.PHONY: xref dialyzer

xref: $(REBAR)
	$(REBAR) xref

dialyzer: $(REBAR)
	$(REBAR) dialyzer

# Generate documentation
.PHONY: edocs

edocs: $(REBAR)
	@echo Building reference edoc documentation...
	@ZOTONIC=`pwd` bin/generate_edoc.escript

# Cleaning
.PHONY: clean_logs
clean_logs:
	@echo "deleting logs:"
	rm -f erl_crash.dump
	rm -rf priv/log/*

.PHONY: clean
clean: clean_logs $(REBAR)
	@echo "cleaning ebin:"
	# There could be an ebin directory left by a previous 0.x branch compilation
	rm -rf ebin
	$(REBAR) $(REBAR_OPTS) clean

.PHONY: dist-clean
dist-clean: clean
	$(REBAR) $(REBAR_OPTS) clean -a
	rm -f ./.rebar3-*
	rm -f ./rebar3
