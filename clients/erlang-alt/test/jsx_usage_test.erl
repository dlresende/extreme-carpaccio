-module(jsx_usage_test).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Tests
%% ===================================================================

parsing_test() ->
  Vals = json_utils:decode(<<"{\"prices\":[75.87],\"quantities\":[5],\"country\":\"FR\"}">>),
  ?assertEqual({ok, [75.87]}, json_utils:get_value(Vals, <<"prices">>)).

encoding_test() ->
  Data = #{total => 0.0},
  Json = json_utils:encode(Data),
  ?assertEqual(<<"{\"total\":0.0}">>, Json).
