-module(example).
-niffy([cube/1]).
-export([cube/1]).


cube(_A) ->
  "int cube(int a)
   {
     return a * a * a;
   }".