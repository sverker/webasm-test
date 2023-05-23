
int32_t enif_printf_I(const char* fmt, int32_t arg1);
int64_t enif_printf_L(const char* fmt, int64_t arg1);
int32_t enif_printf_F(const char* fmt, double arg1);

typedef uint64_t ERL_NIF_TERM;
typedef uintptr_t ErlNifEnv;
int32_t enif_wasm_get_int32(ErlNifEnv, ERL_NIF_TERM, int32_t*);
ERL_NIF_TERM enif_wasm_make_int32(ErlNifEnv, int32_t);
ERL_NIF_TERM enif_wasm_make_badarg(ErlNifEnv);
