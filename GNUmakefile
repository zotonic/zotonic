ERL       ?= erl
ERLC      ?= $(ERL)c
APP       := zotonic

REBAR := ./rebar3
REBAR_URL := https://github.com/erlang/rebar3/releases/download/3.27.3/rebar3
REBAR_SHA256 := f1d87d78decfc56b87d971c75ee6355698ec0c49fadd57f2c056043b7ca291b5
REBAR_STAMP := ./.rebar3-$(REBAR_SHA256)
REBAR_OPTS ?=

ifneq (${https_proxy},)
PROXY_HOSTNAME=$(shell echo ${https_proxy} | awk -F ":" '{print $$2}' | cut -c 3-)
PROXY_PORT=$(shell echo ${https_proxy} | awk -F ":" '{print $$3}')
SET_HTTPS_PROXY=ok = httpc:set_options([{https_proxy, {{"${PROXY_HOSTNAME}", ${PROXY_PORT}}, []}}]),
endif

.PHONY: all upgrade-deps compile-core compile dev test update-cotonic update

# Default target - update sources and call all compile rules in succession
all: compile-core compile
	@echo "Zotonic" `bin/zotonic -v` "was successfully compiled"

$(REBAR_STAMP):
	escript tools/download.escript  "$(REBAR_URL)" "$(REBAR)" --sha256 "$(REBAR_SHA256)"  --executable
	touch $@

# Use Rebar to get, update and compile dependencies
.PHONY: upgrade-deps compile-zotonic compile test

upgrade-deps: $(REBAR_STAMP)
	$(REBAR) $(REBAR_OPTS) upgrade --all

compile-core: $(REBAR_STAMP) _build/default/lib/zotonic_core/include/zotonic.hrl

_build/default/lib/zotonic_core/include/zotonic.hrl: $(REBAR_STAMP)
	@echo ============================================================================
	@echo ======================= Compiling Zotonic core apps =========================
	@echo ============================================================================
	ZOTONIC_APPS=none $(REBAR) $(REBAR_OPTS) compile

compile: $(REBAR_STAMP)
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

xref: $(REBAR_STAMP)
	./rebar3 xref

dialyzer: $(REBAR_STAMP)
	./rebar3 dialyzer

# Generate documentation
.PHONY: edocs

edocs: $(REBAR_STAMP)
	@echo Building reference edoc documentation...
	@ZOTONIC=`pwd` bin/generate_edoc.escript

# Cleaning
.PHONY: clean_logs
clean_logs:
	@echo "deleting logs:"
	rm -f erl_crash.dump
	rm -rf priv/log/*

.PHONY: clean
clean: clean_logs $(REBAR_STAMP)
	@echo "cleaning ebin:"
	# There could be an ebin directory left by a previous 0.x branch compilation
	rm -rf ebin
	$(REBAR) $(REBAR_OPTS) clean

.PHONY: dist-clean
dist-clean: clean
	$(REBAR) $(REBAR_OPTS) clean -a
	rm -f ./.rebar-*
	rm -f ./rebar3
