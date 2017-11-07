-module(matrix).

%% API
-export([multiply/2, collapse/1]).

multiply(InputVector, WeightMatrix) ->
  WeightedInputMatrix = [vector:multiply(InputVector, WeightVector) || WeightVector <- WeightMatrix],
  WeightedInputMatrix.

collapse(WeightedInputMatrix) ->
  WeightedInputVector = [vector:collapse(WeightedInputVector) || WeightedInputVector <- WeightedInputMatrix],
  WeightedInputVector.

