-module(neuron_layer).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                                  terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/4]).
-export([push_to/2]).

create(WeightMatrix, BiasVector, ActivationFunction, NextLayer) ->
	gen_server:start_link(?MODULE, [WeightMatrix, BiasVector, ActivationFunction, NextLayer], []).

push_to(Layer, InputVector) ->
  gen_server:cast(Layer, {push, InputVector}).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {weight_matrix, % [neurons x inputs]
								bias_vector, 	 % [neurons x 1]
				        activation_function,
				        next_layer = undefined}).

init([WeightMatrix, BiasVector, ActivationFunction, NextLayer]) ->

	State = #state{
    weight_matrix = WeightMatrix,
		bias_vector = BiasVector,
    activation_function = ActivationFunction,
    next_layer = NextLayer
  },

  {ok, State}.

handle_call(_Request, _From, State) ->
    Reply = not_implemented,
    {reply, Reply, State}.

handle_cast({push, InputVector}, State) ->
	WeightMatrix			 = State#state.weight_matrix,
	BiasVector				 = State#state.bias_vector,
	ActivationFunction = State#state.activation_function,
	NextLayer					 = State#state.next_layer,

	OutputVector = excite_neurons(InputVector, WeightMatrix, BiasVector, ActivationFunction),

	neuron_layer:push_to(NextLayer, OutputVector),

	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% γ = φ(∑(wⁿ · xⁿ) + b) %% TODO: Implement as C function

excite_neurons(InputVector, WeightMatrix, BiasVector, ActivationFunction) ->
	WeightedInputMatrix = matrix:multiply(InputVector, WeightMatrix),
  WeightedInputVector = matrix:collapse(WeightedInputMatrix),
  BiasedWeightedInputVector = vector:sum(WeightedInputVector, BiasVector),
  vector:apply(ActivationFunction, BiasedWeightedInputVector).

%% ==================================================================================
%%                       Neural Network Matrix Multiplication
%% ==================================================================================
%%
%%                       w¹   w²   w³       b
%%                    |[0.0, 0.0, 0.0]| + |0.0| = I¹     w¹   w²   w³   w⁴    w⁵      b
%%    x¹	 x²   x³    |[0.0, 0.0, 0.0]| + |0.0| = I²   [0.0, 0.0, 0.0, 0.0, 0.0] + |0.0| = Horizontal Degree
%%  [0.0, 0.0, 0.0] * |[0.0, 0.0, 0.0]| + |0.0| = I³ * [0.0, 0.0, 0.0, 0.0, 0.0] + |0.0| = Vertical Degree
%%                    |[0.0, 0.0, 0.0]| + |0.0| = I⁴
%%                    |[0.0, 0.0, 0.0]| + |0.0| = I⁵
%%
%% ----------------------------------------------------------------------------------



%% Intel I5-6300U - Instruction Set Extensions
%% Streaming SIMD Extensions - SSE4.1/4.2
%% Advanced Vector Extensions - AVX2

%% Intel Parallel Studio XE 2017

