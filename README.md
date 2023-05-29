# wasm_runtime_nif

An Erlang module with a NIF implementation to load and call web-assembly code
to run within the Erlang runtime VM.


## Prerequisite

Get WebAssembly Micro Runtime
```sh
git clone https://github.com/bytecodealliance/wasm-micro-runtime
cd wasm-micro-runtime
export WASM_DIR=`pwd`
```
and build it with thread support
```sh
cd $WASM_DIR/product-mini/platforms/linux
mkdir build-thr-mng
cd build-thr-mng
cmake .. -DWAMR_BUILD_LIB_PTHREAD=1
make
```

An Erlang/OTP installation
```sh
export ERL_DIR=...
```


## Build this repo

First set `WASM_DIR` and `ERL_DIR` like above.
Then just run make in the root of this repo.
```sh
make
```

## Run it

```erlang
$ERL_DIR/bin/erl
1> wasm_runtime_nif:init("./test.wasm").
```
then create an instance of the wasm module
```erlang
2> M = wasm_runtime_nif:new().
```

Function `call_raw/3` is used to call functions that use plain integer and float
types for arguments and return value:

```erlang
2> wasm_runtime_nif:call_raw(M, add_I32, [4,7]).
11
```

Function `call/3` is used to call functions that use the `erl_nif_wasm.h`
interface to read and create Erlang terms.
```erlang
3> wasm_runtime_nif:call(M, add_terms, [4,7]).
11
4> wasm_runtime_nif:call(M, add_list_terms, [[4,6,3,7,5]]).
25
4> wasm_runtime_nif:call(M, binary_reverse, [<<1,2,3,4,5>>]).
<<5,4,3,2,1>>
```


## Multi threading limitations

Only one Erlang process (VM thread) at a time can call functions in a wasm
module instance. This is by default enforced by a mutex lock (like above) or by
binding the module instance to the calling Erlang process:

```erlang
5> Mb = wasm_runtime_nif:new([process_bound]).

6> wasm_runtime_nif:call(Mb, add_terms, [4,7]).
11
7> self().
<0.96.0>
```
now crash the shell to get a new shell process
```erlang
8> 1=2.
** exception error: no match of right hand side value 2
9> self().
<0.99.0>
10> wasm_runtime_nif:call(Mb, add_terms, [4,7]).
** exception error: "called by wrong process"
```

Different wasm module instancess can be called concurrently, but they will not
share any data with each other. The only way to run purely multi-threaded wasm
code is to spawn threads within the wasm code (not tested).

