-module(activation_functions).

%% ====================================================================
%% API functions
%% ====================================================================
-export([linear/1, binary_step/1, logistic/1, tangent/1, radial/4]).

% f(x) = x -> (-∞, ∞)
linear(X) -> X.

% f(x) = 0 for x < 0 || 1 for x > 0 -> {0, 1}
binary_step(X) when X < 0 -> 0;
binary_step(X) when X > 0 -> 1.

% f(x) = 1 / (1 + e^-x) -> (0, 1)
logistic(X) ->
  1 / (1 + math:exp(X * -1)).

% f(x) = e^2x - 1 / e^2x + 1 -> (-1, 1)
tangent(X) ->
  (math:exp(2 * X) - 1) / (math:exp(2 * X) + 1).

% f(x, a, b, c) = ? && TODO: Implement Radial Basis Function
radial(X, A, B, C) ->
  X + A + B + C.

