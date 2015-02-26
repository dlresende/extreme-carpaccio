-module(jsx_usage_test).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Tests
%% ===================================================================

parsing_test() ->
  Vals = jsx:decode(<<"{\"prices\":[75.87],\"quantities\":[5],\"country\":\"FR\"}">>),
  Key = <<"prices">>,
  ?assertEqual({Key, [75.87]}, proplists:lookup(Key, Vals)).

encoding_test() ->
  Data = #{total => 0.0},
  Json = jsx:encode(Data),
  ?assertEqual(<<"{\"total\":0.0}">>, Json).
