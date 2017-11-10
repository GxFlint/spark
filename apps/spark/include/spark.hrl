-record(input_neuron, {field_ref::string()}).
-record(input_layer, {neurons :: list(#input_neuron{})}).

-record(hidden_neuron, {weights :: list(float()), bias :: float()}).
-record(hidden_layer, {neurons :: list(#hidden_neuron{}), activation_function}).

-record(output_neuron, {}).
-record(output_layer, {}).
