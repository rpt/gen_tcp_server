Generic TCP Server
==================

[![Build Status][travis_ci_image]][travis_ci]

**Generic TCP Server** (`gen_tcp_server`) is an Erlang behaviour providing quick and
easy way to add TCP server functionality to you application. It's implemented as
a supervisor managing TCP connections as it's children.

How to use it?
-----------

 * Run `make` to build.
 * Run `make test` to run tests.
 * Add as a rebar dependency in your application:

        {deps,  [{gen_tcp_server, ".*", {git, "git://github.com/rpt/gen_tcp_server.git"}}]}.

Callbacks
--------

The `gen_tcp_server` behaviour specifies three callbacks:

 * `handle_accept/1` - called on accepting a new connection

        handle_accept(Socket :: socket()) -> {ok, State :: term()} |
                                             {stop, Reason :: term()}.

 * `handle_tcp/3` - for handling incoming TCP data

        handle_tcp(Socket :: socket(), Data :: binary(), State :: term()) -> {ok, State :: term()} |
                                                                             {stop, Reason :: term()}.

 * `handle_close/3` - called when socket is closed

        -type reason :: normal |
                        {tcp_error, term()} |
                        {handle_accept_error, term()} |
                        {handle_tcp_error, term()}.

        handle_close(Socket :: socket(), Reason :: reason(), State :: term()) -> ok.

Example
-------

Simple `echo_server` example showing how to use `gen_tcp_server` can be found
[here][echo_server.erl].

[echo_server.erl]:
https://github.com/rpt/gen_tcp_server/blob/master/examples/echo_server.erl
[travis_ci]:
http://travis-ci.org/rpt/gen_tcp_server
[travis_ci_image]:
https://secure.travis-ci.org/rpt/gen_tcp_server.png
