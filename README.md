# Spark
A neural network application written in Erlang

Proposed usage
```
NNState == Neuron layers, weights and bias values
NNState = pmml:open("a_neural_network_defined_as_a_pmml_file.xml"),
spark:start(),
NNId = spark:buid(NNState),
OutputVector = spark:run(NNId, InputVector),
```