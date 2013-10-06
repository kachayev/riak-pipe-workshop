REBAR := $(shell which rebar || echo ./rebar)
APP := github
NODE := github
COOKIE := github-users-analyzer

SKIP_DEPS ?= false

.PHONY: all deps

all: 
	$(REBAR) deps compile

console:
	erl \
		-name $(NODE)@127.0.0.1 \
		-setcookie $(COOKIE) \
		-riak_core handoff_port 4090 \
		-riak_core web_port 4091 \
		-sasl errlog_type error \
		-pa deps/*/ebin ebin \
		-github auth $(GITHUB_AUTH) \
		-s $(APP)

compile:
	$(REBAR) compile skip_deps=$(SKIP_DEPS)

deps:
	$(REBAR) get-deps

update-deps:
	$(REBAR) update-deps
