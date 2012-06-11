%%%-----------------------------------------------------------------------------
%%% @copyright 2012 Krzysztof Rutka
%%% @author Krzysztof Rutka <krzysztof.rutka@gmail.com>
%%% @doc Main header file for the generic TCP server.
%%% @end
%%%-----------------------------------------------------------------------------

-define(GEN_TCP_SERVER_OPTS, [binary,
                              {packet, raw},
                              {active, once}]).
