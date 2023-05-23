-module(tester_nif).

-export([hello/0, print_func/1, arg_binary_alloc/1, arg_binary_free/1,
	 ret_binary/2, new_module_inst/0, new_exec_env/0,
	 new_instance/0, call/3,
	 malloc/1]).

-nifs([hello/0, print_func/1, arg_binary_alloc/1, arg_binary_free/1,
       ret_binary/2, new_module_inst/0, new_exec_env/0,
       new_instance/0, call/3,
       malloc/1]).

-on_load(init/0).

init() ->
      erlang:load_nif("./tester_nif", 0).

hello() ->
      erlang:nif_error("NIF library not loaded").

print_func(_Func) ->
      erlang:nif_error("NIF library not loaded").

arg_binary_alloc(_Binary) ->
      erlang:nif_error("NIF library not loaded").

arg_binary_free(_BinRef) ->
      erlang:nif_error("NIF library not loaded").

ret_binary(_BinRef, _BinSize) ->
    erlang:nif_error("NIF library not loaded").

new_module_inst() ->
    erlang:nif_error("NIF library not loaded").

new_exec_env() ->
    erlang:nif_error("NIF library not loaded").

new_instance() ->
    erlang:nif_error("NIF library not loaded").

call(_,_,_) ->
    erlang:nif_error("NIF library not loaded").

malloc(_) ->
    erlang:nif_error("NIF library not loaded").
