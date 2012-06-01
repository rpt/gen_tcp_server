.PHONY: all compile clean doc

all: compile

compile: rebar
	./rebar compile

clean: rebar
	./rebar clean

rebar:
	wget -q http://cloud.github.com/downloads/basho/rebar/rebar
	chmod u+x rebar

doc:
	./rebar skip_deps=true doc
