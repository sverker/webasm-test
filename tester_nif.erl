-module(tester_nif).

-export([hello/0, apply/2, print_func/1]).

-nifs([hello/0, apply/2, print_func/1]).

-on_load(init/0).

init() ->
      erlang:load_nif("./tester_nif", 0).

hello() ->
      erlang:nif_error("NIF library not loaded").

apply(_Func, _Args) ->
      erlang:nif_error("NIF library not loaded").

print_func(_Func) ->
      erlang:nif_error("NIF library not loaded").
