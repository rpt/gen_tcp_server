.PHONY: all compile test dialyzer clean doc

APPS = kernel stdlib

compile: rebar examples/*.erl
	@./rebar compile
	@(cd examples && erlc -pa ../ebin *.erl)

test: rebar
	@./rebar eunit

dialyzer: build.plt compile
	dialyzer --plt $< ebin

build.plt:
	dialyzer -q --build_plt --apps $(APPS) --output_plt $@

clean: rebar
	@./rebar clean
	@rm -f examples/*.beam

doc: rebar
	@./rebar doc

rebar:
	@wget -q http://cloud.github.com/downloads/basho/rebar/rebar
	@chmod u+x rebar
