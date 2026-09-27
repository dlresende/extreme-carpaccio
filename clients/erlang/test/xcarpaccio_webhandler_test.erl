-module(xcarpaccio_webhandler_test).

-include_lib("eunit/include/eunit.hrl").

ping_test() ->
    ?assertEqual({200, <<"text/plain; charset=utf-8">>, <<"pong">>},
                 xcarpaccio_webhandler:reply(<<"POST">>, <<"/ping">>)).
