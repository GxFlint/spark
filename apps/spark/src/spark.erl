-module(spark).

-define(APPLICATION, ?MODULE).
%% API
-export([start/0, stop/0, test/0]).

start()  ->
  application:ensure_all_started(?APPLICATION).

stop() ->
  application:stop(?APPLICATION).

test() ->
  neuron_layer:test(),
  ok.




