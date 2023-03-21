#include <wasm_export.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>


static char* read_wasm_binary_to_buffer(const char* wasm_file,
					uint32_t* size)
{
  FILE* ff;
  int fd;
  struct stat statbuf;
  char* buf;
    
  ff = fopen(wasm_file, "r");
  if (!ff) {
    fprintf(stderr, "File not found: %s\n", wasm_file);
    perror("fopen");
    exit(1);
  }
  fd = fileno(ff);
  if (fstat(fd, &statbuf) != 0) {
    fprintf(stderr, "Could not stat: %s\n", wasm_file);
    perror("fstat");
    exit(1);
  }

  buf = malloc(statbuf.st_size);
  if (fread(buf, 1, statbuf.st_size, ff) != statbuf.st_size) {
    fprintf(stderr, "Could not read: %s\n", wasm_file);
    perror("fread");
    exit(1);
  }
  if (fclose(ff) != 0) {
    fprintf(stderr, "Could not close: %s\n", wasm_file);
    perror("fclose");
    exit(1);
  }
  *size = statbuf.st_size;
  return buf;
}

int enif_printf_I(wasm_exec_env_t exec_env, const char* fmt, int arg1)
{

  //fprintf(stderr, "enif_printf1 called fmt=%p\n", fmt);
  return fprintf(stderr, fmt, arg1);
}

/* the native functions that will be exported to WASM app */
static NativeSymbol native_symbols[] = {
    EXPORT_WASM_API_WITH_SIG(enif_printf_I, "($i)i")
};

int main()
{
  char *buffer, error_buf[128];
  wasm_module_t module;
  wasm_module_inst_t module_inst;
  wasm_function_inst_t func;
  wasm_exec_env_t exec_env;
  uint32_t size, stack_size = 8092, heap_size = 8092;


  /* all the runtime memory allocations are retricted in the global_heap_buf array */
  static char global_heap_buf[512 * 1024];
  RuntimeInitArgs init_args;
  memset(&init_args, 0, sizeof(RuntimeInitArgs));

  fprintf(stderr, "global_heap_buf = %p\n", global_heap_buf);

  /* configure the memory allocator for the runtime */
  init_args.mem_alloc_type = Alloc_With_Pool;
  init_args.mem_alloc_option.pool.heap_buf = global_heap_buf;
  init_args.mem_alloc_option.pool.heap_size = sizeof(global_heap_buf);

  /* configure the native functions being exported to WASM app */
  init_args.native_module_name = "env";
  init_args.n_native_symbols = sizeof(native_symbols) / sizeof(NativeSymbol);
  init_args.native_symbols = native_symbols;

  /* set maximum thread number if needed when multi-thread is enabled,
     the default value is 4 */
  init_args.max_thread_num = 4;

  /* initialize runtime environment with user configurations*/
  if (!wasm_runtime_full_init(&init_args)) {
    return -1;
  }

  /* initialize the wasm runtime by default configurations */
  //wasm_runtime_init();

  /* read WASM file into a memory buffer */
  buffer = read_wasm_binary_to_buffer("./add.wasm", &size);

  /* add line below if we want to export native functions to WASM app */
  //wasm_runtime_register_natives(...);

  /* parse the WASM file from buffer and create a WASM module */
  module = wasm_runtime_load(buffer, size, error_buf, sizeof(error_buf));

  /* create an instance of the WASM module (WASM linear memory is ready) */
  module_inst = wasm_runtime_instantiate(module, stack_size, heap_size,
                                         error_buf, sizeof(error_buf));


  /* lookup a WASM function by its name
     The function signature can NULL here */
  func = wasm_runtime_lookup_function(module_inst, "add", NULL);

  /* creat an execution environment to execute the WASM functions */
  exec_env = wasm_runtime_create_exec_env(module_inst, stack_size);

  {
    uint32_t argv[2];

    /* arguments are always transferred in 32-bit element */
    argv[0] = 8;
    argv[1] = 13;

    /* call the WASM function */
    if (wasm_runtime_call_wasm(exec_env, func, 2, argv) ) {
      /* the return value is stored in argv[0] */
      printf("add function return: %d\n", argv[0]);
    }
    else {
      /* exception is thrown if call fails */
      printf("%s\n", wasm_runtime_get_exception(module_inst));
    }
  }

  return 0;
}
