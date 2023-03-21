int tester_init();
int tester_run();
int tester_call_func(const char* name, uint32_t n_args, wasm_val_t* args,
                     wasm_val_t* result, const char** error);
