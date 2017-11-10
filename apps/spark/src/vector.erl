-module(vector).

%% API
-export([collapse/1, sum/2, multiply/2, apply/2]).

collapse(WeightedInputVector) ->
  lists:foldl(fun(X, Sum) -> X + Sum end, 0, WeightedInputVector).

sum(WeightedInputVector, BiasVector) ->
  lists:zipwith(fun (X, Y) -> X + Y end, WeightedInputVector, BiasVector).

multiply(InputVector, WeightVector) ->
  lists:zipwith(fun (X, Y) -> X * Y end, InputVector, WeightVector).

apply(ActivationFunction, BiasedWeightedInputVector) -> %% γⁿ = φ(wbⁿ)
  lists:map(ActivationFunction, BiasedWeightedInputVector).

