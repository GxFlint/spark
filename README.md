# Spark
A neural network application written in Erlang

Proposed usage
```
NNState == Neuron layers, weights and bias values
NNState = pmml:open("a_neural_network_defined_as_a_pmml_file.xml"),
spark:start(NNState),
spark:train(??,??),
OutputVector = spark:calc(InputVector),
NNState = spark:export(),
pmml:save(NNState, "current_network_topology_and_weights_as_a_pmml_file.xml"),
```