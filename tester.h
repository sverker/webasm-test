extern wasm_module_inst_t module_inst;

int tester_init();
int tester_run();
int tester_call_func(wasm_function_inst_t,
                     uint32_t n_args, wasm_val_t* args,
                     uint32_t n_ret, wasm_val_t* result,
                     const char** error);
void print_func(const char* fname,
                wasm_function_inst_t func,
                wasm_module_inst_t module_inst);
