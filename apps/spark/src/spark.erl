-module(spark).
-include("spark.hrl").
-define(APPLICATION, ?MODULE).

%% API
-export([start/0, build/1, compile_network/1, stop/0, test/0]).
-export([cast/2, call/2]).

start() ->
  application:ensure_all_started(?APPLICATION).

build(NNState) ->
  Kernel = compile_kernel(NNState),
  Kernel#nn_kernel{}.

compile_kernel(NNState) ->
  #nn_kernel{}.

compile_network(FileName) ->
  compile_network(FileName, self).

compile_network(FileName, ReturnID) ->
  case pmml:open(FileName) of
    {ok, Topology} -> {ok, 'NNId'} = build_tree(Topology, ReturnID);
    {error, Reason} -> {error, Reason}
  end.

build_tree(Topology, ReturnID) ->
  %% Start a new supervision tree
  ok.

stop() ->
  application:stop(?APPLICATION).

cast(NNId, InputVector) when is_pid(NNId) ->
  neuron_layer:push_to(NNId, InputVector).

call(NNId, InputVector) when is_pid(NNId) ->
  %% TODO: Weight for cast sequence to return
  neuron_layer:push_to(NNId, InputVector).

test() ->
  pmml:test(),
  neuron_layer:test(),
  ok.
