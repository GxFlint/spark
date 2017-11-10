-module(spark).
-define(APPLICATION, ?MODULE).

%% API
-export([start/1, stop/0, test/0]).
-export([calc/1, train/2, export/0]).

start(NetworkTopology)  ->
  application:ensure_all_started(?APPLICATION),
  _ = NetworkTopology.

stop() ->
  application:stop(?APPLICATION).

calc(InputVector) ->
  _ = InputVector,
  ok.

train(_, _) ->
  ok.

export() ->
  NetworkTopology = layers_and_weights,
  NetworkTopology.

test() ->
  pmml:test(),
  neuron_layer:test(),
  ok.




