-module(tester_nif).

-export([print_func/2, new/0, new/1, call/3, call_raw/3]).

-nifs([print_func/2, new_instance/0, call/3, call_raw/3]).

-on_load(init/0).

-define(PROCESS_BOUND, 1).

init() ->
      erlang:load_nif("./tester_nif", 0).

print_func(_Inst, _Func) ->
      erlang:nif_error("NIF library not loaded").

new() ->
    new([]).

new([]) ->
    new_instance(0);
new([process_bound]) ->
    new_instance(?PROCESS_BOUND).

new_instance(_Flags) ->
    erlang:nif_error("NIF library not loaded").

call(_,_,_) ->
    erlang:nif_error("NIF library not loaded").

call_raw(_,_,_) ->
    erlang:nif_error("NIF library not loaded").

