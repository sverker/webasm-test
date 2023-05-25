-module(tester_nif).

-export([print_func/2, arg_binary_alloc/2, arg_binary_free/2,
	 ret_binary/3,
	 new_instance/0, call/3, call_raw/3,
	 malloc/1]).

-nifs([print_func/2, arg_binary_alloc/2, arg_binary_free/2,
       ret_binary/3,
       new_instance/0, call/3, call_raw/3,
       malloc/1]).

-on_load(init/0).

init() ->
      erlang:load_nif("./tester_nif", 0).

print_func(_Inst, _Func) ->
      erlang:nif_error("NIF library not loaded").

arg_binary_alloc(_Inst, _Binary) ->
      erlang:nif_error("NIF library not loaded").

arg_binary_free(_Inst, _BinRef) ->
      erlang:nif_error("NIF library not loaded").

ret_binary(_Inst, _BinRef, _BinSize) ->
    erlang:nif_error("NIF library not loaded").

new_instance() ->
    erlang:nif_error("NIF library not loaded").

call(_,_,_) ->
    erlang:nif_error("NIF library not loaded").

call_raw(_,_,_) ->
    erlang:nif_error("NIF library not loaded").

malloc(_) ->
    erlang:nif_error("NIF library not loaded").
