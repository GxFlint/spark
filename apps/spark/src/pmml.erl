-module(pmml).
-include("spark.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% API
-export([open/1, save/2,test/0]).

-record(topology, {layers}).

open(FileName) ->
  case xmerl_scan:file(FileName) of
    {error, Reason} -> error(Reason);
    {Xml, _} ->
      'PMML' = Xml#xmlElement.name,
      case xmerl_lib:find_attribute(version, Xml#xmlElement.attributes) of
        false -> error({attribute_not_found, version});
        {value, PMMLVersion} ->
          PMMLContent = Xml#xmlElement.content,
          NN = getElement('NeuralNetwork',  PMMLContent),
          
          Tags = PMMLContent#xmlText.value
      end
  end.

getElement(ElementName, Elements) ->
  getElement(ElementName, Elements, undefined).
  
getElement(ElementName, _, E) when E#xmlElement.name == ElementName -> E;
getElement(ElementName, [H|T], _Element) ->
  getElement(ElementName, T, H).

save(NNState, FileName) ->
  ok.

test() ->
  {X, _} = open("C:/Users/Gustavo/Desktop/single_iris_mlp.xml"),
  
  [C] = X#xmlElement.content,
  
  ok.

