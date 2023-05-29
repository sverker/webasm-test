all: wasm_runtime_nif.so test.wasm wasm_runtime_nif.beam

WASM_LINK := -L/home/uabseri/src/wasm-micro-runtime/product-mini/platforms/linux/build-thr-mngr -Wl,-rpath=/home/uabseri/src/wasm-micro-runtime/product-mini/platforms/linux/build-thr-mngr -liwasm
WASM_INCLUDE := -I/home/uabseri/src/wasm-micro-runtime/core/iwasm/include
CFLAGS := -g -O0
ERL_ROOT := /home/uabseri/program/otp-24.3.4.1

wasm_runtime_nif.so: wasm_runtime_nif.o
	gcc $^ -fPIC -shared $(WASM_LINK) -o $@

wasm_runtime_nif.o: wasm_runtime_nif.c
	gcc -c -fPIC $(CFLAGS) $(WASM_INCLUDE) -I$(ERL_ROOT)/usr/include wasm_runtime_nif.c -o $@

wasm_runtime_nif.beam: wasm_runtime_nif.erl
	$(ERL_ROOT)/bin/erlc wasm_runtime_nif.erl

test.wasm: test.c erl_nif_wasm.h
	clang --target=wasm32 --no-standard-libraries -Wl,--export-all -Wl,--no-entry -Wl,--allow-undefined test.c -o $@
