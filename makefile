REBAR ?= rebar3
REBAR_CMD = $(REBAR) $(profile:%=as %)

all: compile xref eunit

compile:
	@$(REBAR_CMD) compile

xref:
	@$(REBAR_CMD) xref

clean:
	@$(REBAR_CMD) clean

test:
	@$(REBAR_CMD) do eunit,cover

edoc:
	@$(REBAR_CMD) edoc

shell:
	-@$(REBAR_CMD) shell

dialyzer:
	@$(REBAR_CMD) dialyzer

