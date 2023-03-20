all: main add.wasm

main: main.c
	gcc -g -O0 -I /home/uabseri/src/wasm-micro-runtime/core/iwasm/include main.c -L/home/uabseri/src/wasm-micro-runtime/product-mini/platforms/linux/build -Wl,-rpath=/home/uabseri/src/wasm-micro-runtime/product-mini/platforms/linux/build -liwasm -o main


add.wasm: add.c
	clang --target=wasm32 --no-standard-libraries -Wl,--export-all -Wl,--no-entry -Wl,--allow-undefined add.c -o add.wasm
