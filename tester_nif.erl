-module(tester_nif).

-export([hello/0]).

-nifs([hello/0]).

-on_load(init/0).

init() ->
      erlang:load_nif("./tester_nif", 0).

hello() ->
      erlang:nif_error("NIF library not loaded").

