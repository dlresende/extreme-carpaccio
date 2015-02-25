-module(xcarpaccio_webhandler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

%% ===================================================================
%% Handler callbacks
%% ===================================================================

%%
%% @doc init/3 handler callback
%%
init(_Transport, Req, Opts) ->
    % Opts is defined "as is" for the State value
    % in the handle/2 method
    {ok, Req, Opts}.

%%
%% @doc handle/2 handler callback
%%
handle(Req, Params) ->
  {Method, Req1} = cowboy_req:method(Req),
  {Path,   Req2} = cowboy_req:path(Req1),
  io:format("xcarpaccio:handle: ~p // ~p: ~p ~n", [Method, Path, Params]),
  {ok, Req0} = handle0(Method, Path, Req2),
  io:format("xcarpaccio:handle: ~p ~n", [Req0]),
  {ok, Req0, undefined}.


%%
%% @doc terminate/3 handler callback
%%
terminate(_Reason, _Req, _State) ->
    ok.

%% ===================================================================
%% Handle functions
%% ===================================================================

%%
%% @doc
%% @private
%%
handle0(<<"POST">>, <<"/">>, Req2) ->
  Result = #{total=>0.0},
  Body = json_utils:encode(Result),
  cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>},
                         {<<"content-encoding">>, <<"utf-8">>}], Body, Req2);


%%
%% @doc
%% @private
%%
handle0(<<"GET">>, <<"/ping">>, Req2) ->
  Body = <<"pong">>,
  cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>},
                         {<<"content-encoding">>, <<"utf-8">>}], Body, Req2);

%%
%% @doc All Other GET cases returns a 404 (not found) HTTP response
%% @private
%%
handle0(<<"GET">>, _, Req2) ->
  cowboy_req:reply(404, Req2);

%%
%% @doc Any other Method cases returns a 405 (not allowed) HTTP response
%% @private
%%
handle0(Method, Path, Req2) ->
  io:format("xcarpaccio:handle0: not allowed [~p][~p] ~n", [Method, Path]),
  %% Method not allowed.
  cowboy_req:reply(405, Req2).
