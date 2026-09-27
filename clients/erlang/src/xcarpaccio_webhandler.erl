-module(xcarpaccio_webhandler).

-export([init/3, handle/2]).
-export([reply/2]).

%% ===================================================================
%% Cowboy callbacks
%% ===================================================================

init(Req, Opts) ->
    {ok, Req, Opts}.

handle(Req0, State) ->
    {Method, Req1} = cowboy_req:method(Req0),
    {Path, Req2} = cowboy_req:path(Req1),
    {Status, ContentType, Body} = reply(Method, Path),
    Req = cowboy_req:reply(Status, #{<<"content-type">> => ContentType}, Body, Req2),
    {ok, Req, State}.

%% ===================================================================
%% Request dispatch
%% ===================================================================

%%
%% @doc Pure decision function, so the routing behaviour can be unit tested
%% without starting cowboy.
%%
reply(<<"POST">>, <<"/ping">>) ->
    {200, <<"text/plain; charset=utf-8">>, <<"pong">>};
reply(_Method, _Path) ->
    {404, <<"text/plain; charset=utf-8">>, <<"Not Found">>}.
