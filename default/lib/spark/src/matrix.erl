-module(matrix).

%% API
-export([multiply/2, collapse/1]).

multiply(InputVector, WeightMatrix) ->
  [vector:multiply(InputVector, WeightVector) || WeightVector <- WeightMatrix].

collapse(WeightedInputMatrix) ->
  [vector:collapse(WeightedInputVector) || WeightedInputVector <- WeightedInputMatrix].

