ERL       ?= erl
ERLC      ?= $(ERL)c
APP       := zotonic
PARSER    := src/erlydtl/erlydtl_parser

# Erlang Rebar downloading
# see: https://groups.google.com/forum/?fromgroups=#!topic/erlang-programming/U0JJ3SeUv5Y
REBAR := $(shell (type rebar 2>/dev/null || echo ./rebar) | tail -1 | awk '{ print $$NF }')
REBAR_DEPS := $(shell which rebar || echo ../../rebar)
REBAR_URL := http://cloud.github.com/downloads/basho/rebar/rebar

# Default target - update sources and call all compile rules in succession
.PHONY: all
all: get-deps update-deps compile


./rebar:
	$(ERL) -noshell -s inets -s ssl \
	  -eval '{ok, saved_to_file} = httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar"}])' \
	  -s init stop
	chmod +x ./rebar

DEPS = $(shell find deps -type d | egrep '^deps/[^/]*$$' | grep -v 'deps/lager')
LAGER = deps/lager
Compile = (cd $(1) && $(REBAR_DEPS) deps_dir=.. compile)

# Helper targets
.PHONY: erl
erl:
	@$(ERL) -pa $(wildcard deps/*/ebin) -pa ebin -noinput +B \
	  -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

$(PARSER).erl: $(PARSER).yrl
	$(ERLC) -o src/erlydtl $(PARSER).yrl

ebin/$(APP).app: src/$(APP).app.src
	cp src/$(APP).app.src $@

# Use Rebar to get, update and compile dependencies
.PHONY: get-deps update-deps compile-deps compile-zotonic compile

get-deps: $(REBAR)
	$(REBAR) get-deps

update-deps: $(REBAR)
	$(REBAR) update-deps

compile-deps: $(REBAR)
	if [ -d $(LAGER) ]; then $(call Compile, $(LAGER)); fi
	for i in $(DEPS); do $(call Compile, $$i); done

compile-zotonic: $(PARSER).erl erl ebin/$(APP).app

compile: compile-deps compile-zotonic


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
	rm -rf priv/log/*

.PHONY: clean
clean: clean_logs $(REBAR)
	@echo "removing:"
	rm -f ebin/*.beam ebin/*.app
	@echo "cleaning dependencies:"
	$(REBAR) clean

.PHONY: dist-clean
dist-clean: clean
	rm -f ./rebar
