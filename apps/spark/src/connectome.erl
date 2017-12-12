-module(connectome).

-export([test/0, bar/1, foo/1]).

-on_load(init/0).

init() ->
  Nif = "C:/Projects/Connectome/Debug/Connectome",
  erlang:load_nif(Nif, 0).


test() ->
  case test_nif() of
    nif_library_not_loaded -> nif_library_not_loaded;
    _ -> ok
  end.

test_nif() ->
  nif_library_not_loaded.

foo(_X) ->
  nif_library_not_loaded.
bar(_Y) ->
  nif_library_not_loaded.