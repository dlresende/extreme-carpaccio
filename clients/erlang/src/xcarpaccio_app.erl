-module(xcarpaccio_app).

-behaviour(application).

-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Port = port(),
    Dispatch = cowboy_router:compile(routes()),
    {ok, _} = cowboy:start_clear(xcarpaccio_http,
                                 [{port, Port}],
                                 #{env => #{dispatch => Dispatch}}),
    io:format("Listening on http://0.0.0.0:~p~n", [Port]).

stop(_State) ->
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

routes() ->
    [{'_', [{"/ping", xcarpaccio_webhandler, #{method => <<"POST">>}, []}]}].

%%
%% Retrieve the PORT from the environment, falling back to the app env.
%%
port() ->
    case os:getenv("PORT") of
        false ->
            {ok, Port} = application:get_env(xcarpaccio, http_port),
            Port;
        Other ->
            list_to_integer(Other)
    end.
