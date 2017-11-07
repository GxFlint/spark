-module(vector).

%% API
-export([collapse/1, sum/2, multiply/2, apply/2]).

collapse(WeightedInputVector) ->
  CollapsedWeightedInputVector = lists:foldl(fun(X, Sum) -> X + Sum end, 0, WeightedInputVector),
  CollapsedWeightedInputVector.

sum(WeightedInputVector, BiasVector) ->
  BiasedWeightedInputVector = lists:zipwith(fun (X, Y) -> X + Y end, WeightedInputVector, BiasVector),
  BiasedWeightedInputVector.

multiply(InputVector, WeightVector) ->
  WeightedInputVector = lists:zipwith(fun (X, Y) -> X * Y end, InputVector, WeightVector),
  WeightedInputVector.

apply(ActivationFunction, BiasedWeightedInputVector) -> %% γⁿ = φ(wbⁿ)
  NeuronOutputVector = lists:map(ActivationFunction, BiasedWeightedInputVector),
  NeuronOutputVector.

