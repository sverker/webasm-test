all: tester_nif.so add.wasm tester_nif.beam

WASM_LINK := -L/home/uabseri/src/wasm-micro-runtime/product-mini/platforms/linux/build-thr-mngr -Wl,-rpath=/home/uabseri/src/wasm-micro-runtime/product-mini/platforms/linux/build-thr-mngr -liwasm
WASM_INCLUDE := -I/home/uabseri/src/wasm-micro-runtime/core/iwasm/include
CFLAGS := -g -O0
ERL_ROOT := /home/uabseri/program/otp-24.3.4.1

tester_nif.so: tester_nif.o
	gcc $^ -fPIC -shared $(WASM_LINK) -o $@

tester_nif.o: tester_nif.c
	gcc -c -fPIC $(CFLAGS) $(WASM_INCLUDE) -I$(ERL_ROOT)/usr/include tester_nif.c -o $@

tester_nif.beam: tester_nif.erl
	$(ERL_ROOT)/bin/erlc tester_nif.erl

add.wasm: add.c erl_nif_wasm.h
	clang --target=wasm32 --no-standard-libraries -Wl,--export-all -Wl,--no-entry -Wl,--allow-undefined add.c -o $@
