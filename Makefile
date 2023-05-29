all: wasm_runtime_nif.so wasm_runtime_nif.beam test.wasm

ifeq ($(WASM_DIR),)
    WASM_DIR := /home/uabseri/src/wasm-micro-runtime
endif
ifeq ($(ERL_DIR),)
    ERL_DIR := /home/uabseri/program/otp-24.3.4.1
endif

WASM_LINK := -L$(WASM_DIR)/product-mini/platforms/linux/build-thr-mngr -Wl,-rpath=$(WASM_DIR)/product-mini/platforms/linux/build-thr-mngr -liwasm
WASM_INCLUDE := -I$(WASM_DIR)/core/iwasm/include
CFLAGS := -g -O0

wasm_runtime_nif.so: wasm_runtime_nif.o
	gcc $^ -fPIC -shared $(WASM_LINK) -o $@

wasm_runtime_nif.o: wasm_runtime_nif.c
	gcc -c -fPIC $(CFLAGS) $(WASM_INCLUDE) -I$(ERL_DIR)/usr/include wasm_runtime_nif.c -o $@

wasm_runtime_nif.beam: wasm_runtime_nif.erl
	$(ERL_DIR)/bin/erlc wasm_runtime_nif.erl

test.wasm: test.c erl_nif_wasm.h
	clang --target=wasm32 --no-standard-libraries -Wl,--export-all -Wl,--no-entry -Wl,--allow-undefined test.c -o $@
