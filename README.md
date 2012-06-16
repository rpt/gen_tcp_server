Generic TCP Server
==================

Generic TCP Server (`gen_tcp_server`) is an Erlang behaviour providing quick and
easy way to add TCP server functionality to you application. It's implemented as
a supervisor managing TCP connections as it's children.

Callbacks
--------

`handle_accept(Socket :: socket()) -> {ok, State :: term()} |
                                      {stop, Reason :: term()}.`
`handle_tcp(Socket :: socket(), Data :: binary(), State :: term()) ->
     {ok, State :: term()} | {stop, Reason :: term()}.`

Example
-------

Simple `echo_server` example showing how to use `gen_tcp_server` can be found
[here][echo_server.erl].

[echo_server.erl]:
https://github.com/rpt/gen_tcp_server/blob/master/examples/echo_server.erl