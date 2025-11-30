// codegen_wasm32.c - WebAssembly code generator for chibicc
//
// This file replaces chibicc's x86-64 codegen.c with a WebAssembly backend.
// It generates WASM binary format directly, suitable for running in browsers.
//
// WebAssembly is a stack-based virtual machine. To compute (a + b):
//   1. Push a onto the stack
//   2. Push b onto the stack
//   3. Execute i32.add (pops two values, pushes result)
//
// This maps naturally to expression trees: recursively emit left subtree,
// then right subtree, then the operator.

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "chibicc.h"

// Many helper functions are defined for the full API but not all are used
// in the examples. This is intentional—they'll be needed for chibicc integration.
#ifdef __GNUC__
#pragma GCC diagnostic ignored "-Wunused-function"
#endif

// ----------------------------------------------------------------------------
// WASM Binary Format Basics
// ----------------------------------------------------------------------------
//
// A WASM module is organized into sections:
//   - Type section:     Function signatures
//   - Import section:   External functions (like printf from libc)
//   - Function section: Maps functions to their signatures
//   - Memory section:   Linear memory declaration
//   - Global section:   Global variables
//   - Export section:   Functions/memory exposed to host
//   - Code section:     Function bodies
//   - Data section:     Initialized data (strings, etc.)
//
// Numbers are encoded as LEB128 (Little Endian Base 128), a variable-length
// encoding where the high bit indicates "more bytes follow".

static FILE *output_file;
static Obj *current_fn;

// ----------------------------------------------------------------------------
// WASM Opcodes
// ----------------------------------------------------------------------------

typedef enum {
    // Control flow
    OP_UNREACHABLE   = 0x00,
    OP_NOP           = 0x01,
    OP_BLOCK         = 0x02,
    OP_LOOP          = 0x03,
    OP_IF            = 0x04,
    OP_ELSE          = 0x05,
    OP_END           = 0x0B,
    OP_BR            = 0x0C,
    OP_BR_IF         = 0x0D,
    OP_BR_TABLE      = 0x0E,
    OP_RETURN        = 0x0F,
    OP_CALL          = 0x10,
    OP_CALL_INDIRECT = 0x11,

    // Parametric
    OP_DROP          = 0x1A,
    OP_SELECT        = 0x1B,

    // Variable access
    OP_LOCAL_GET     = 0x20,
    OP_LOCAL_SET     = 0x21,
    OP_LOCAL_TEE     = 0x22,
    OP_GLOBAL_GET    = 0x23,
    OP_GLOBAL_SET    = 0x24,

    // Memory
    OP_I32_LOAD      = 0x28,
    OP_I64_LOAD      = 0x29,
    OP_F32_LOAD      = 0x2A,
    OP_F64_LOAD      = 0x2B,
    OP_I32_LOAD8_S   = 0x2C,
    OP_I32_LOAD8_U   = 0x2D,
    OP_I32_LOAD16_S  = 0x2E,
    OP_I32_LOAD16_U  = 0x2F,
    OP_I32_STORE     = 0x36,
    OP_I64_STORE     = 0x37,
    OP_F32_STORE     = 0x38,
    OP_F64_STORE     = 0x39,
    OP_I32_STORE8    = 0x3A,
    OP_I32_STORE16   = 0x3B,
    OP_MEMORY_SIZE   = 0x3F,
    OP_MEMORY_GROW   = 0x40,

    // Constants
    OP_I32_CONST     = 0x41,
    OP_I64_CONST     = 0x42,
    OP_F32_CONST     = 0x43,
    OP_F64_CONST     = 0x44,

    // Comparison (i32)
    OP_I32_EQZ       = 0x45,
    OP_I32_EQ        = 0x46,
    OP_I32_NE        = 0x47,
    OP_I32_LT_S      = 0x48,
    OP_I32_LT_U      = 0x49,
    OP_I32_GT_S      = 0x4A,
    OP_I32_GT_U      = 0x4B,
    OP_I32_LE_S      = 0x4C,
    OP_I32_LE_U      = 0x4D,
    OP_I32_GE_S      = 0x4E,
    OP_I32_GE_U      = 0x4F,

    // Comparison (i64)
    OP_I64_EQZ       = 0x50,
    OP_I64_EQ        = 0x51,
    OP_I64_NE        = 0x52,
    OP_I64_LT_S      = 0x53,
    OP_I64_LT_U      = 0x54,
    OP_I64_GT_S      = 0x55,
    OP_I64_GT_U      = 0x56,
    OP_I64_LE_S      = 0x57,
    OP_I64_LE_U      = 0x58,
    OP_I64_GE_S      = 0x59,
    OP_I64_GE_U      = 0x5A,

    // Arithmetic (i32)
    OP_I32_CLZ       = 0x67,
    OP_I32_CTZ       = 0x68,
    OP_I32_POPCNT    = 0x69,
    OP_I32_ADD       = 0x6A,
    OP_I32_SUB       = 0x6B,
    OP_I32_MUL       = 0x6C,
    OP_I32_DIV_S     = 0x6D,
    OP_I32_DIV_U     = 0x6E,
    OP_I32_REM_S     = 0x6F,
    OP_I32_REM_U     = 0x70,
    OP_I32_AND       = 0x71,
    OP_I32_OR        = 0x72,
    OP_I32_XOR       = 0x73,
    OP_I32_SHL       = 0x74,
    OP_I32_SHR_S     = 0x75,
    OP_I32_SHR_U     = 0x76,
    OP_I32_ROTL      = 0x77,
    OP_I32_ROTR      = 0x78,

    // Arithmetic (i64)
    OP_I64_CLZ       = 0x79,
    OP_I64_CTZ       = 0x7A,
    OP_I64_POPCNT    = 0x7B,
    OP_I64_ADD       = 0x7C,
    OP_I64_SUB       = 0x7D,
    OP_I64_MUL       = 0x7E,
    OP_I64_DIV_S     = 0x7F,
    OP_I64_DIV_U     = 0x80,
    OP_I64_REM_S     = 0x81,
    OP_I64_REM_U     = 0x82,
    OP_I64_AND       = 0x83,
    OP_I64_OR        = 0x84,
    OP_I64_XOR       = 0x85,
    OP_I64_SHL       = 0x86,
    OP_I64_SHR_S     = 0x87,
    OP_I64_SHR_U     = 0x88,

    // Conversions
    OP_I32_WRAP_I64     = 0xA7,
    OP_I64_EXTEND_I32_S = 0xAC,
    OP_I64_EXTEND_I32_U = 0xAD,
} WasmOp;

// WASM value types
typedef enum {
    TYPE_I32   = 0x7F,
    TYPE_I64   = 0x7E,
    TYPE_F32   = 0x7D,
    TYPE_F64   = 0x7C,
    TYPE_VOID  = 0x40,  // Used for block types with no result
} WasmType;

// WASM section IDs
typedef enum {
    SECTION_CUSTOM   = 0,
    SECTION_TYPE     = 1,
    SECTION_IMPORT   = 2,
    SECTION_FUNCTION = 3,
    SECTION_TABLE    = 4,
    SECTION_MEMORY   = 5,
    SECTION_GLOBAL   = 6,
    SECTION_EXPORT   = 7,
    SECTION_START    = 8,
    SECTION_ELEMENT  = 9,
    SECTION_CODE     = 10,
    SECTION_DATA     = 11,
} WasmSection;

// ----------------------------------------------------------------------------
// Output Buffer
// ----------------------------------------------------------------------------
// We build the WASM binary in memory, then write it all at once.

typedef struct {
    uint8_t *data;
    size_t len;
    size_t cap;
} Buffer;

static Buffer *buf_new(void) {
    Buffer *b = calloc(1, sizeof(Buffer));
    b->cap = 1024;
    b->data = malloc(b->cap);
    return b;
}

static void buf_push(Buffer *b, uint8_t byte) {
    if (b->len >= b->cap) {
        b->cap *= 2;
        b->data = realloc(b->data, b->cap);
    }
    b->data[b->len++] = byte;
}

static void buf_push_bytes(Buffer *b, const void *data, size_t len) {
    for (size_t i = 0; i < len; i++)
        buf_push(b, ((uint8_t *)data)[i]);
}

// LEB128 encoding for unsigned integers
static void buf_push_uleb128(Buffer *b, uint64_t val) {
    do {
        uint8_t byte = val & 0x7F;
        val >>= 7;
        if (val != 0)
            byte |= 0x80;
        buf_push(b, byte);
    } while (val != 0);
}

// LEB128 encoding for signed integers
static void buf_push_sleb128(Buffer *b, int64_t val) {
    int more = 1;
    while (more) {
        uint8_t byte = val & 0x7F;
        val >>= 7;

        int sign_bit = byte & 0x40;
        if ((val == 0 && !sign_bit) || (val == -1 && sign_bit))
            more = 0;
        else
            byte |= 0x80;

        buf_push(b, byte);
    }
}

// Push a length-prefixed string (used for names in exports, etc.)
static void buf_push_name(Buffer *b, const char *name) {
    size_t len = strlen(name);
    buf_push_uleb128(b, len);
    buf_push_bytes(b, name, len);
}

// ----------------------------------------------------------------------------
// Module Builder
// ----------------------------------------------------------------------------
// Collects all parts of the WASM module during code generation.

typedef struct {
    Buffer *types;      // Function type declarations
    Buffer *imports;    // Imported functions
    Buffer *functions;  // Function index -> type index mapping
    Buffer *memory;     // Memory declarations
    Buffer *globals;    // Global variables
    Buffer *exports;    // Exported functions/memory
    Buffer *code;       // Function bodies
    Buffer *data;       // Initialized data segments

    int num_types;
    int num_imports;
    int num_functions;
    int num_globals;
    int num_exports;
    int num_data_segments;

    int stack_ptr_global;  // Index of __stack_pointer global
    int heap_base;         // Start of heap in linear memory
} WasmModule;

static WasmModule *module_new(void) {
    WasmModule *m = calloc(1, sizeof(WasmModule));
    m->types = buf_new();
    m->imports = buf_new();
    m->functions = buf_new();
    m->memory = buf_new();
    m->globals = buf_new();
    m->exports = buf_new();
    m->code = buf_new();
    m->data = buf_new();
    m->heap_base = 65536;  // Default: 64KB for stack, rest for heap
    return m;
}

// Add a function type: (param types...) -> (result types...)
// Returns the type index
static int module_add_type(WasmModule *m, WasmType *params, int num_params,
                           WasmType *results, int num_results) {
    buf_push(m->types, 0x60);  // Function type marker

    buf_push_uleb128(m->types, num_params);
    for (int i = 0; i < num_params; i++)
        buf_push(m->types, params[i]);

    buf_push_uleb128(m->types, num_results);
    for (int i = 0; i < num_results; i++)
        buf_push(m->types, results[i]);

    return m->num_types++;
}

// Add an imported function
static int module_add_import(WasmModule *m, const char *module_name,
                             const char *field_name, int type_idx) {
    buf_push_name(m->imports, module_name);
    buf_push_name(m->imports, field_name);
    buf_push(m->imports, 0x00);  // Import kind: function
    buf_push_uleb128(m->imports, type_idx);
    return m->num_imports++;
}

// Add a function declaration (body added separately in code section)
static int module_add_function(WasmModule *m, int type_idx) {
    buf_push_uleb128(m->functions, type_idx);
    return m->num_imports + m->num_functions++;
}

// Add an export
static void module_add_export(WasmModule *m, const char *name,
                              int kind, int index) {
    buf_push_name(m->exports, name);
    buf_push(m->exports, kind);  // 0=func, 1=table, 2=memory, 3=global
    buf_push_uleb128(m->exports, index);
    m->num_exports++;
}

// Add a global variable
static int module_add_global(WasmModule *m, WasmType type, int mutable,
                             int64_t init_value) {
    buf_push(m->globals, type);
    buf_push(m->globals, mutable ? 1 : 0);

    // Initial value expression
    if (type == TYPE_I32) {
        buf_push(m->globals, OP_I32_CONST);
        buf_push_sleb128(m->globals, init_value);
    } else {
        buf_push(m->globals, OP_I64_CONST);
        buf_push_sleb128(m->globals, init_value);
    }
    buf_push(m->globals, OP_END);

    return m->num_globals++;
}

// Add a data segment (initialized memory)
static void module_add_data(WasmModule *m, int offset,
                            const void *data, size_t len) {
    buf_push(m->data, 0x00);  // Active segment, memory 0

    // Offset expression
    buf_push(m->data, OP_I32_CONST);
    buf_push_sleb128(m->data, offset);
    buf_push(m->data, OP_END);

    // Data bytes
    buf_push_uleb128(m->data, len);
    buf_push_bytes(m->data, data, len);

    m->num_data_segments++;
}

// ----------------------------------------------------------------------------
// Function Code Builder
// ----------------------------------------------------------------------------
// Generates the bytecode for a single function body.

typedef struct {
    Buffer *code;

    // Local variables: first params, then declared locals
    int num_params;
    int num_locals;

    // For generating unique labels
    int label_counter;

    // Current function's stack frame size
    int frame_size;
} FuncBuilder;

static FuncBuilder *func_new(int num_params) {
    FuncBuilder *f = calloc(1, sizeof(FuncBuilder));
    f->code = buf_new();
    f->num_params = num_params;
    return f;
}

// Declare local variables (grouped by type for compactness)
static void func_declare_locals(FuncBuilder *f, int num_i32, int num_i64) {
    int num_groups = (num_i32 > 0) + (num_i64 > 0);
    buf_push_uleb128(f->code, num_groups);

    if (num_i32 > 0) {
        buf_push_uleb128(f->code, num_i32);
        buf_push(f->code, TYPE_I32);
    }
    if (num_i64 > 0) {
        buf_push_uleb128(f->code, num_i64);
        buf_push(f->code, TYPE_I64);
    }

    f->num_locals = num_i32 + num_i64;
}

// Emit a single opcode
static void emit_op(FuncBuilder *f, WasmOp op) {
    buf_push(f->code, op);
}

// Emit opcode with unsigned LEB128 immediate
static void emit_op_u(FuncBuilder *f, WasmOp op, uint64_t val) {
    buf_push(f->code, op);
    buf_push_uleb128(f->code, val);
}

// Emit opcode with signed LEB128 immediate
static void emit_op_s(FuncBuilder *f, WasmOp op, int64_t val) {
    buf_push(f->code, op);
    buf_push_sleb128(f->code, val);
}

// Emit a memory operation (load/store) with alignment and offset
static void emit_mem_op(FuncBuilder *f, WasmOp op, int align, int offset) {
    buf_push(f->code, op);
    buf_push_uleb128(f->code, align);   // log2 of alignment
    buf_push_uleb128(f->code, offset);
}

// ----------------------------------------------------------------------------
// High-Level Code Generation Helpers
// ----------------------------------------------------------------------------

// Push an i32 constant onto the stack
static void emit_i32_const(FuncBuilder *f, int32_t val) {
    emit_op_s(f, OP_I32_CONST, val);
}

// Push an i64 constant onto the stack
static void emit_i64_const(FuncBuilder *f, int64_t val) {
    emit_op_s(f, OP_I64_CONST, val);
}

// Load from local variable
static void emit_local_get(FuncBuilder *f, int index) {
    emit_op_u(f, OP_LOCAL_GET, index);
}

// Store to local variable
static void emit_local_set(FuncBuilder *f, int index) {
    emit_op_u(f, OP_LOCAL_SET, index);
}

// Store to local variable, keeping value on stack
static void emit_local_tee(FuncBuilder *f, int index) {
    emit_op_u(f, OP_LOCAL_TEE, index);
}

// Load from global variable
static void emit_global_get(FuncBuilder *f, int index) {
    emit_op_u(f, OP_GLOBAL_GET, index);
}

// Store to global variable
static void emit_global_set(FuncBuilder *f, int index) {
    emit_op_u(f, OP_GLOBAL_SET, index);
}

// Call a function by index
static void emit_call(FuncBuilder *f, int func_index) {
    emit_op_u(f, OP_CALL, func_index);
}

// Load i32 from memory: stack has address, replaces with value
static void emit_i32_load(FuncBuilder *f, int offset) {
    emit_mem_op(f, OP_I32_LOAD, 2, offset);  // align=4 (2^2)
}

// Store i32 to memory: stack has [address, value], consumes both
static void emit_i32_store(FuncBuilder *f, int offset) {
    emit_mem_op(f, OP_I32_STORE, 2, offset);
}

// Load i8 from memory (sign-extended to i32)
static void emit_i8_load_s(FuncBuilder *f, int offset) {
    emit_mem_op(f, OP_I32_LOAD8_S, 0, offset);
}

// Load i8 from memory (zero-extended to i32)
static void emit_i8_load_u(FuncBuilder *f, int offset) {
    emit_mem_op(f, OP_I32_LOAD8_U, 0, offset);
}

// Store low byte of i32 to memory
static void emit_i8_store(FuncBuilder *f, int offset) {
    emit_mem_op(f, OP_I32_STORE8, 0, offset);
}

// ----------------------------------------------------------------------------
// Control Flow
// ----------------------------------------------------------------------------

// Start a block (for break targets)
static int emit_block_start(FuncBuilder *f, WasmType result_type) {
    emit_op(f, OP_BLOCK);
    buf_push(f->code, result_type);
    return f->label_counter++;
}

// Start a loop (for continue targets)
static int emit_loop_start(FuncBuilder *f, WasmType result_type) {
    emit_op(f, OP_LOOP);
    buf_push(f->code, result_type);
    return f->label_counter++;
}

// End a block or loop
static void emit_end(FuncBuilder *f) {
    emit_op(f, OP_END);
}

// Branch to enclosing block/loop (0 = innermost)
static void emit_br(FuncBuilder *f, int depth) {
    emit_op_u(f, OP_BR, depth);
}

// Conditional branch
static void emit_br_if(FuncBuilder *f, int depth) {
    emit_op_u(f, OP_BR_IF, depth);
}

// If-then-else
static void emit_if(FuncBuilder *f, WasmType result_type) {
    emit_op(f, OP_IF);
    buf_push(f->code, result_type);
}

static void emit_else(FuncBuilder *f) {
    emit_op(f, OP_ELSE);
}

// Return from function
static void emit_return(FuncBuilder *f) {
    emit_op(f, OP_RETURN);
}

// Drop top of stack
static void emit_drop(FuncBuilder *f) {
    emit_op(f, OP_DROP);
}

// ----------------------------------------------------------------------------
// Binary Output
// ----------------------------------------------------------------------------

// Write a section: section_id, size, contents
static void write_section(Buffer *out, int section_id, Buffer *contents) {
    if (contents->len == 0)
        return;

    buf_push(out, section_id);
    buf_push_uleb128(out, contents->len);
    buf_push_bytes(out, contents->data, contents->len);
}

// Write a section with a count prefix
static void write_section_with_count(Buffer *out, int section_id,
                                     int count, Buffer *contents) {
    if (count == 0)
        return;

    // Calculate size: count (as uleb128) + contents
    Buffer *temp = buf_new();
    buf_push_uleb128(temp, count);
    buf_push_bytes(temp, contents->data, contents->len);

    buf_push(out, section_id);
    buf_push_uleb128(out, temp->len);
    buf_push_bytes(out, temp->data, temp->len);

    free(temp->data);
    free(temp);
}

// Generate the complete WASM binary
static Buffer *module_finish(WasmModule *m) {
    Buffer *out = buf_new();

    // Magic number and version
    buf_push_bytes(out, "\0asm", 4);
    uint8_t version[] = {1, 0, 0, 0};
    buf_push_bytes(out, version, 4);

    // Type section
    write_section_with_count(out, SECTION_TYPE, m->num_types, m->types);

    // Import section
    write_section_with_count(out, SECTION_IMPORT, m->num_imports, m->imports);

    // Function section
    write_section_with_count(out, SECTION_FUNCTION, m->num_functions, m->functions);

    // Memory section (1 memory, min 2 pages = 128KB)
    if (1) {
        Buffer *mem = buf_new();
        buf_push_uleb128(mem, 1);  // 1 memory
        buf_push(mem, 0x00);       // No maximum
        buf_push_uleb128(mem, 2);  // Min 2 pages (128KB)
        write_section(out, SECTION_MEMORY, mem);
        free(mem->data);
        free(mem);
    }

    // Global section
    write_section_with_count(out, SECTION_GLOBAL, m->num_globals, m->globals);

    // Export section
    write_section_with_count(out, SECTION_EXPORT, m->num_exports, m->exports);

    // Code section
    write_section_with_count(out, SECTION_CODE, m->num_functions, m->code);

    // Data section
    write_section_with_count(out, SECTION_DATA, m->num_data_segments, m->data);

    return out;
}

// ---------------------------------------------------------------------------
// Small helpers used by the backend
// ---------------------------------------------------------------------------

static int is_void(Type *ty) {
    return ty->kind == TY_VOID;
}

// For now, treat any integer <= 4 bytes as an i32 scalar.
// You can extend this later for floats or other cases.
static int is_scalar_i32(Type *ty) {
    return is_integer(ty) && ty->size <= 4;
}

// If you want a backend-specific error, you can wrap the generic error()
#define backend_error(...) error(__VA_ARGS__)
// ---------------------------------------------------------------------------
// Attach function bodies to the module's code section
// ---------------------------------------------------------------------------
//
// For each function body, the code section contains:
//   uleb128 body_size
//   [body bytes...]
//
// body bytes = local decls + opcode stream (ending with OP_END)

static void module_add_function_body(WasmModule *m, FuncBuilder *f) {
    Buffer *dst = m->code;
    Buffer *src = f->code;

    buf_push_uleb128(dst, src->len);
    buf_push_bytes(dst, src->data, src->len);
}

// ---------------------------------------------------------------------------
// Map chibicc Obj locals/params to wasm locals
// ---------------------------------------------------------------------------
//
// For now this is a stub that reports how many params/locals a function has.
// You can later extend it to actually assign per-Obj local indices for ND_VAR.

static void assign_local_indices(Obj *fn, int *out_num_params, int *out_num_locals) {
    int param_count = 0;
    for (Obj *p = fn->params; p; p = p->next)
        param_count++;

    int local_count = 0;
    for (Obj *v = fn->locals; v; v = v->next) {
        // For a first pass, treat all scalar locals as i32
        if (is_scalar_i32(v->ty))
            local_count++;
    }

    *out_num_params = param_count;
    *out_num_locals = local_count;
}

// ---------------------------------------------------------------------------
// Top-level codegen entry point
// ---------------------------------------------------------------------------

static FuncBuilder *current_func;

void codegen_wasm32(Obj *prog, FILE *out) {
    output_file = out;

    WasmModule *m = module_new();

    // First pass: create function types and assign wasm function indices.
    for (Obj *fn = prog; fn; fn = fn->next) {
        if (!fn->is_function || !fn->is_definition)
            continue;

        current_fn = fn;

        // Count params
        int param_count = 0;
        for (Obj *p = fn->params; p; p = p->next)
            param_count++;

        WasmType *params = calloc(param_count, sizeof(WasmType));
        for (int i = 0; i < param_count; i++)
            params[i] = TYPE_I32;  // all scalar params as i32 for now

        WasmType results[1];
        int result_count = 0;
        if (!is_void(fn->ty->return_ty)) {
            if (!is_scalar_i32(fn->ty->return_ty))
                backend_error("Function %s: only scalar i32 returns supported", fn->name);
            results[0] = TYPE_I32;
            result_count = 1;
        }

        int type_idx = module_add_type(m, params, param_count, results, result_count);
        free(params);

        int func_index = module_add_function(m, type_idx);
        fn->offset = func_index;  // reuse offset for function index (safe for functions)

        // Export "main" for the host
        if (strcmp(fn->name, "main") == 0)
            module_add_export(m, "main", 0 /* kind: func */, func_index);
    }

    // Second pass: generate bodies
    for (Obj *fn = prog; fn; fn = fn->next) {
        if (!fn->is_function || !fn->is_definition)
            continue;

        current_fn = fn;

        int param_count = 0;
        int local_count = 0;
        assign_local_indices(fn, &param_count, &local_count);

        // Build function body
        current_func = func_new(param_count);

        // Locals: for now, treat all locals as i32 (no i64 locals yet)
        func_declare_locals(current_func, local_count, 0);

        // TODO: Emit body
        //if (fn->body)
        //    gen_stmt(fn->body);

        // If non-void function falls off the end, return 0
        if (!is_void(fn->ty->return_ty)) {
            emit_i32_const(current_func, 0);
            emit_return(current_func);
        }

        emit_op(current_func, OP_END);

        module_add_function_body(m, current_func);

        free(current_func->code->data);
        free(current_func->code);
        free(current_func);
        current_func = NULL;
    }

    // Finish module and write to FILE*
    Buffer *wasm = module_finish(m);
    fwrite(wasm->data, 1, wasm->len, output_file);

    // Free buffers (basic cleanup)
    free(wasm->data);
    free(wasm);

    free(m->types->data);
    free(m->types);
    free(m->imports->data);
    free(m->imports);
    free(m->functions->data);
    free(m->functions);
    free(m->memory->data);
    free(m->memory);
    free(m->globals->data);
    free(m->globals);
    free(m->exports->data);
    free(m->exports);
    free(m->code->data);
    free(m->code);
    free(m->data->data);
    free(m->data);

    free(m);
}
