-module(json_utils).


-export([decode/1,
         encode/1,
         get_value/2]).

decode(Source) ->
  jsx:decode(Source).

encode(Data) ->
  jsx:encode(Data).

get_value([], _Searched) ->
    not_found;

get_value([{Key, Value}|_T], Searched) when Key == Searched ->
    {ok, Value};

get_value([_H|T], Searched) ->
    get_value(T, Searched).
