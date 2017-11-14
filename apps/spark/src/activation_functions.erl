-module(activation_functions).

%% ====================================================================
%% API functions
%% ====================================================================
-export([linear/1]).
-export([binary_step/1, step/1]).
-export([logistic/1, sigmoid/1]).
-export([rectifier/1]).
-export([tangent/1]).
-export([radial/4]).

% f(x) = x -> (-∞, ∞)
linear(X) -> X.

% f(x) = 1 if x > 0 else 0 -> {0, 1}
binary_step(X) when X > 0 -> 1;
binary_step(_) -> 0.
step(X) -> binary_step(X).

% f(x) = x if x > 0 else 0 -> (0, ∞)
rectifier(X) when X > 0 -> X;
rectifier(_) -> 0.

% f(x) = 1 / (1 + e^-x) -> (0, 1)
logistic(X) ->
  sigmoid(X).
sigmoid(X) ->
  1 / (1 + math:exp(X * -1)).

% f(x) = e^2x - 1 / e^2x + 1 -> (-1, 1)
tangent(X) ->
  (math:exp(2 * X) - 1) / (math:exp(2 * X) + 1).

% f(x, a, b, c) = ? && TODO: Implement Radial Basis Function
radial(X, A, B, C) ->
  X + A + B + C.

