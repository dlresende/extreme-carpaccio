-module(json_utils).

%% API
-export([decode/1,
         encode/1,
         get_value/2]).

%% ===================================================================
%% API functions
%% ===================================================================

%%
%% @doc decode/1 decode a json text into Erlang datastructure
%%
decode(Source) ->
  jsx:decode(Source).

%%
%% @doc encode/1 encode an Erlang datastructure into a json text
%%
encode(Data) ->
  jsx:encode(Data).

%%
%% @doc get_value/2 lookup a named tuple within the datastructure and
%%    either return the associated value `{ok, ValueFound}` or `not_found`
%%

get_value([], _Searched) ->
    not_found;

get_value([{Key, Value}|_T], Searched) when Key == Searched ->
    {ok, Value};

get_value([_H|T], Searched) ->
    get_value(T, Searched).
