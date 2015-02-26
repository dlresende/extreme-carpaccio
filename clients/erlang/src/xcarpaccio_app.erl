-module(xcarpaccio_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(C_ACCEPTORS,  100).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Routes    = routes(),
    Dispatch  = cowboy_router:compile(Routes),
    Port      = port(),
    TransOpts = [{port, Port}],
    ProtoOpts = [{env, [{dispatch, Dispatch}]}],
    HttpStart = cowboy:start_http(http, ?C_ACCEPTORS, TransOpts, ProtoOpts),
    io:format("cowboy:start_http (on port ~p): ~p~n", [Port, HttpStart]),
    {ok, _}   = HttpStart.

stop(_State) ->
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

%%
%% Cowboy routes mapping -
%% route does not support HTTP Method constraint
%%   https://groups.google.com/forum/#!topic/erlang-programming/-v2sBGxhDMY
%%
%% To prevent path duplication, dispatching is done in the handler itself
%%
routes() ->
    [
     {'_', [
            {"/[...]", xcarpaccio_webhandler, []}
           ]}
    ].

%%
%% Retrieve the PORT either from an os environment variable
%% e.g. in Heroku environment, or from the application conf.
%%
port() ->
    case os:getenv("PORT") of
        false ->
            {ok, Port} = application:get_env(http_port),
            Port;
        Other ->
            list_to_integer(Other)
    end.
