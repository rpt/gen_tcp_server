.PHONY: all compile test clean doc

all: compile

compile: rebar
	./rebar compile

clean: rebar
	./rebar clean

test: compile
	./rebar skip_deps=true eunit

rebar:
	wget -q http://cloud.github.com/downloads/basho/rebar/rebar
	chmod u+x rebar

doc:
	./rebar skip_deps=true doc
