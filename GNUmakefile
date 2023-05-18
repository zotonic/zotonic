ERL       ?= erl
ERLC      ?= $(ERL)c
APP       := zotonic

# Use the local or installed rebar3 if available, if not found then download to ./rebar3
REBAR := $(shell which rebar3)
ifneq ("$(wildcard ./rebar3)","")
	REBAR := ./rebar3
else ifeq ($(REBAR),"")
	REBAR := ./rebar3
endif

REBAR_URL := https://s3.amazonaws.com/rebar3/rebar3
REBAR_ETAG := rebar_etag
REBAR_OPTS ?=

ifneq (${https_proxy},)
	PROXY_HOSTNAME=$(shell echo ${https_proxy} | awk -F ":" '{print $$2}' | cut -c 3-)
	PROXY_PORT=$(shell echo ${https_proxy} | awk -F ":" '{print $$3}')
	SET_HTTPS_PROXY=ok = httpc:set_options([{https_proxy, {{"${PROXY_HOSTNAME}", ${PROXY_PORT}}, []}}]),
endif

.PHONY: all

# Default target - update sources and call all compile rules in succession
all: compile
	@echo "Zotonic" `bin/zotonic -v` "was successfully compiled"

./rebar3: $(REBAR_ETAG)
	$(ERL) -noshell -s inets -s ssl \
	  -eval 'file:delete("./rebar3"), $(SET_HTTPS_PROXY) {ok, saved_to_file} = httpc:request(get, {"$(REBAR_URL)", []}, [{ssl, [ {verify, verify_none} ]}], [{stream, "./rebar3"}])' \
	  -s init stop
	chmod +x ./rebar3

# Use Rebar to get, update and compile dependencies
.PHONY: upgrade-deps compile dev test update update-cotonic

upgrade-deps: $(REBAR)
	$(REBAR) $(REBAR_OPTS) upgrade --all

compile: $(REBAR)
	$(REBAR) $(REBAR_OPTS) compile

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
.PHONY: docs edocs

docs:
	@echo Building HTML documentation...
	cd doc && $(MAKE) stubs && $(MAKE) html
	@echo HTML documentation is now available in doc/_build/html/

edocs: $(REBAR)
	@echo Building reference edoc documentation...
	@ZOTONIC=`pwd` bin/generate_edoc.escript

# Get ETAG from rebar3 amazon cloud and update etag file if different.

# First use if it does not exist at all
# used by "make compile" dependencies
$(REBAR_ETAG):
	$(ERL) -noshell -s inets -s ssl \
		-eval '$(SET_HTTPS_PROXY) case httpc:request(head, {"$(REBAR_URL)", []}, [{ssl, [ {verify, verify_none} ]},{timeout, 2000}], []) of {ok, {_, Headers,_}} -> Etag = proplists:get_value("etag", Headers), Bin = list_to_binary(Etag), case file:read_file("$(REBAR_ETAG)") of {ok, Bin} -> io:fwrite("ETag update not needed~n"); {ok, _OldEtag} -> file:write_file("$(REBAR_ETAG)", Bin); {error, enoent} -> file:write_file("$(REBAR_ETAG)", Bin); _ -> ok end,   io:fwrite("Etag: ~s~n",[Etag]); Error -> io:fwrite("Failed to get rebar3 etag: ~p~n",[Error]) end' \
		-s init stop

.PHONY: pull
pull:
	$(ERL) -noshell -s inets -s ssl \
		-eval '$(SET_HTTPS_PROXY) case httpc:request(head, {"$(REBAR_URL)", []}, [{ssl, [ {verify, verify_none} ]},{timeout, 2000}], []) of {ok, {_, Headers,_}} -> Etag = proplists:get_value("etag", Headers), Bin = list_to_binary(Etag), case file:read_file("$(REBAR_ETAG)") of {ok, Bin} -> io:fwrite("ETag update not needed~n"); {ok, _OldEtag} -> file:write_file("$(REBAR_ETAG)", Bin); {error, enoent} -> file:write_file("$(REBAR_ETAG)", Bin); _ -> ok end,   io:fwrite("Etag: ~s~n",[Etag]); Error -> io:fwrite("Failed to get rebar3 etag: ~p~n",[Error]) end' \
		-s init stop

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
	rm -f ./rebar3
