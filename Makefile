ERL= $(shell which erl)

ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/*/ebin

REBAR= $(shell which rebar)

ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif

.PHONY: clean distclean

all: compile

compile: 
	$(REBAR) get-deps compile

test: 
	$(REBAR) eunit

clean: 
	$(REBAR) clean 

distclean: clean
	   rm -rf erl_crash.dump *~ *#
	   rm -rvf $(CURDIR)/deps/*
	   rm -rvf $(CURDIR)/doc/*