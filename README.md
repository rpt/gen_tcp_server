# Generic TCP Server [![Build Status][travis_ci_image]][travis_ci]

**Generic TCP Server** (`gen_tcp_server`) is an Erlang behaviour providing quick and
easy way to add TCP server functionality to you application. It's implemented as
a supervisor managing TCP connections as it's children.

## How to use it?

 * Run `make` to build.
 * Run `make test` to run tests.
 * Add as a dependency to your `rebar.config`:

``` erlang
{gen_tcp_server, "", {git, "git://github.com/rpt/gen_tcp_server.git", {tag, "1.0.1"}}}
```

## Callbacks

The `gen_tcp_server` behaviour specifies three callbacks:

 * `handle_accept/1` - called on accepting a new connection

``` erlang
handle_accept(Socket :: socket()) -> {ok, State :: term()} | {stop, Reason :: term()}.
```

 * `handle_tcp/3` - for handling incoming TCP data

``` erlang
handle_tcp(Socket :: socket(), Data :: binary(), State :: term()) -> {ok, State :: term()} |
                                                                     {stop, Reason :: term()}.
```

 * `handle_close/3` - called when socket is closed

``` erlang
-type reason :: normal | {tcp_error, term()} |
                {handle_accept_error, term()} | {handle_tcp_error, term()}.

handle_close(Socket :: socket(), Reason :: reason(), State :: term()) -> ok.
```

## Pool of acceptors

To use a pool of acceptors use `gen_tcp_server:start_link/3` and specify a `pool` option. For example:

``` erlang
gen_tcp_server:start_link(handler_module, 1234, [{pool, 10}]).
```

## Examples

Simple `echo_server` example showing how to use `gen_tcp_server` can be found
[here][echo_server.erl].

[echo_server.erl]:
https://github.com/rpt/gen_tcp_server/blob/master/examples/echo_server.erl
[travis_ci]:
http://travis-ci.org/rpt/gen_tcp_server
[travis_ci_image]:
https://secure.travis-ci.org/rpt/gen_tcp_server.png
