all: main tester_nif.so add.wasm tester_nif.beam

WASM_LINK := -L/home/uabseri/src/wasm-micro-runtime/product-mini/platforms/linux/build -Wl,-rpath=/home/uabseri/src/wasm-micro-runtime/product-mini/platforms/linux/build -liwasm
WASM_INCLUDE := -I/home/uabseri/src/wasm-micro-runtime/core/iwasm/include
CFLAGS := -g -O0
ERL_ROOT := /home/uabseri/program/otp-24.3.4.1

main: main.o tester.o
	gcc $^ $(WASM_LINK) -o main

tester_nif.so: tester_nif.o tester.o
	gcc $^ -fPIC -shared $(WASM_LINK) -o $@

tester_nif.o: tester_nif.c tester.h
	gcc -c -fPIC $(CFLAGS) $(WASM_INCLUDE) -I$(ERL_ROOT)/usr/include tester_nif.c -o $@

main.o: main.c tester.h
	gcc -c $(CFLAGS) $(WASM_INCLUDE) main.c -o main.o

tester.o: tester.c tester.h
	gcc -c -fPIC $(CFLAGS) $(WASM_INCLUDE) tester.c -o $@

tester_nif.beam: tester_nif.erl
	$(ERL_ROOT)/bin/erlc tester_nif.erl

add.wasm: add.c
	clang --target=wasm32 --no-standard-libraries -Wl,--export-all -Wl,--no-entry -Wl,--allow-undefined add.c -o $@
