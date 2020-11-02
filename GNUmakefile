ERL       ?= erl
ERLC      ?= $(ERL)c
APP       := zotonic

# Erlang Rebar downloading
# see: https://groups.google.com/forum/?fromgroups=#!topic/erlang-programming/U0JJ3SeUv5Y
REBAR := ./rebar3
REBAR_URL := https://s3.amazonaws.com/rebar3/rebar3
REBAR_ETAG := rebar_etag
REBAR_OPTS ?=

ifneq (${https_proxy},)
PROXY_HOSTNAME=$(shell echo ${https_proxy} | awk -F ":" '{print $$2}' | cut -c 3-)
PROXY_PORT=$(shell echo ${https_proxy} | awk -F ":" '{print $$3}')
SET_HTTPS_PROXY=ok = httpc:set_options([{https_proxy, {{"${PROXY_HOSTNAME}", ${PROXY_PORT}}, []}}]),
endif

.PHONY: all upgrade-deps compile dev test

# Default target - update sources and call all compile rules in succession
all: compile

$(REBAR): $(REBAR_ETAG)
	$(ERL) -noshell -s inets -s ssl \
	  -eval 'file:delete("$(REBAR)"), $(SET_HTTPS_PROXY) {ok, saved_to_file} = httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "$(REBAR)"}])' \
	  -s init stop
	chmod +x $(REBAR)

# Use Rebar to get, update and compile dependencies
.PHONY: upgrade-deps compile-zotonic compile test

upgrade-deps: $(REBAR)
	$(REBAR) $(REBAR_OPTS) upgrade

compile: $(REBAR)
	$(REBAR) $(REBAR_OPTS) compile

dev:
	docker-compose run --rm --service-ports zotonic sh

test: compile
	bin/zotonic runtests


.PHONY: xref dialyzer

xref: $(REBAR)
	./rebar3 xref

dialyzer: $(REBAR)
	./rebar3 dialyzer

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
		-eval '$(SET_HTTPS_PROXY) case httpc:request(head, {"$(REBAR_URL)", []}, [{timeout, 2000}], []) of {ok, {_, Headers,_}} -> Etag = proplists:get_value("etag", Headers), Bin = list_to_binary(Etag), case file:read_file("$(REBAR_ETAG)") of {ok, Bin} -> io:fwrite("ETag update not needed~n"); {ok, _OldEtag} -> file:write_file("$(REBAR_ETAG)", Bin); {error, enoent} -> file:write_file("$(REBAR_ETAG)", Bin); _ -> ok end,   io:fwrite("Etag: ~s~n",[Etag]); Error -> io:fwrite("Failed to get rebar3 etag: ~p~n",[Error]) end' \
		-s init stop

.PHONY: pull
pull:
	$(ERL) -noshell -s inets -s ssl \
		-eval '$(SET_HTTPS_PROXY) case httpc:request(head, {"$(REBAR_URL)", []}, [{timeout, 2000}], []) of {ok, {_, Headers,_}} -> Etag = proplists:get_value("etag", Headers), Bin = list_to_binary(Etag), case file:read_file("$(REBAR_ETAG)") of {ok, Bin} -> io:fwrite("ETag update not needed~n"); {ok, _OldEtag} -> file:write_file("$(REBAR_ETAG)", Bin); {error, enoent} -> file:write_file("$(REBAR_ETAG)", Bin); _ -> ok end,   io:fwrite("Etag: ~s~n",[Etag]); Error -> io:fwrite("Failed to get rebar3 etag: ~p~n",[Error]) end' \
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
